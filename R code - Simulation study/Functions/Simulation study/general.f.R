
general.f <- function(population, beta, m.params,
                      cluster = NULL, strata = NULL, weights = NULL,
                      nh = NULL, nclus.h = NULL, n.ch = NULL,
                      seed = 1,
                      col.y, col.x, family = c("gaussian","binomial"),
                      nrun = 500,
                      cv.initial.name = NULL){
  
  start <- Sys.time()
  
  results <- list(col.y = col.y,
                  col.x = col.x,
                  family = family,
                  seed = seed,
                  nrun = nrun)
  
  if(!is.null(cluster)) {results$design$cluster <- cluster}
  if(!is.null(strata)) {results$design$strata <- strata}
  if(!is.null(weights)) {results$design$weights <- weights}
  if(!is.null(nh)) {results$design$nh <- nh}
  if(!is.null(nclus.h)) {results$design$nclus.h <- nclus.h}
  if(!is.null(n.ch)) {results$design$n.ch <- n.ch}
  
  nparams <- nrow(m.params)
  
  set.seed(seed)
  seeds <- sample(1:10000000, size = nrun*nparams)
  
  
  for(j in 1:nrun){
    
    start.j <- Sys.time()
    
    cat(">>>>> nrun = ", j, " >>>>> ")
    
    #sample <- obtain.sample(data = population, strata = strata, cluster = cluster,
    #                        nclus.h = nclus.h, n.ch = n.ch, nh = nh,
    #                        seed = seeds[j])
    sample <- sample.2step(data = population, nclus.h = nclus.h, n.ch = n.ch,
                           seed = seeds[j], cluster = cluster, strata = strata)
    
    model.orig <- glmnet(y = as.numeric(sample[,col.y]), 
                         x = as.matrix(sample[,col.x]), 
                         weights = as.numeric(sample[,weights]),
                         family = family)
    lambda.grid <- model.orig$lambda
    if(family == "binomial"){
      delete.id <- which(log(lambda.grid) < -7)
      lambda.grid <- lambda.grid[-delete.id]
    }
    
    pop.error <- population.error(population, model = model.orig, col.x, col.y, family)
    if(family == "binomial"){pop.error <- pop.error[-delete.id]}
    
    results[[paste0("sample_",j)]] <- list(data = sample, lambda.grid = lambda.grid, model.orig = model.orig, pop.error = pop.error)
    
    for(row.params in 1:nparams){
      
      method <- m.params[row.params,"method"]
      
      cat(method, " ---> ")
      
      k <- B <- R <- cv.error.ind <- train.prob <- method.split <- exact <- w.folds <- w.model <- NULL
      
      if(!is.na(m.params[row.params, "k"])) k <- m.params[row.params, "k"]
      if(!is.na(m.params[row.params, "B"])) B <- m.params[row.params, "B"]
      if(!is.na(m.params[row.params, "R"])) R <- m.params[row.params, "R"]
      if(!is.na(m.params[row.params, "cv.error.ind"])) cv.error.ind <- m.params[row.params, "cv.error.ind"]
      if(!is.na(m.params[row.params, "train.prob"])) train.prob <- m.params[row.params, "train.prob"]
      if(!is.na(m.params[row.params, "method.split"])) method.split <- m.params[row.params, "method.split"]
      if(!is.na(m.params[row.params, "w.folds"])) w.folds <- m.params[row.params, "w.folds"]
      if(!is.na(m.params[row.params, "w.model"])) w.model <- m.params[row.params, "w.model"]
      
      l.error <- estimate.error(data = sample, population = population, method = method, 
                                cluster = cluster, strata = strata, weights = weights, 
                                k = k, B = B, R = R, cv.error.ind = cv.error.ind,
                                train.prob = train.prob, method.split = method.split, 
                                w.folds = w.folds, w.model = w.model,
                                seed = seeds[(j-1)*nparams + row.params],
                                col.y = col.y, col.x = col.x, lambda.grid = lambda.grid, family = family)
      
      l.lambda <- results.lambda(obj = l.error, data = sample, col.y = col.y, col.x = col.x, weights = weights, lambda.grid = lambda.grid)
      l.coef <- results.coef(beta, l.lambda$coef[-1])
      l.coef.pop <- results.coef(coef(model.orig)[,which.min(pop.error)], l.lambda$coef[-1])
      results[[paste0("sample_",j)]][[paste0("method_",row.params)]] <- list(params = m.params[row.params,],
                                                                             l.error = l.error,
                                                                             l.lambda = l.lambda,
                                                                             l.coef = l.coef,
                                                                             l.coef.pop = l.coef.pop)
      
    }
    
    # Unweighted model, SRS CV
    # newsample <- replicate.weights.unw(data = sample, weights = NULL, k = 10, R=1, seed = seed*j*5)
    # l.error <- estimate.error.unw(data = newsample, lambda.grid, 
    #                               col.y, col.x, family, 
    #                               R, k, cv.error.ind = FALSE) 
    # l.lambda <- results.lambda(obj = l.error, data = newsample, col.y = col.y, col.x = col.x, weights = weights)
    # l.coef <- results.coef(beta, l.lambda$coef[-1])
    # l.coef.pop <- results.coef(coef(model.orig)[,which.min(pop.error)], l.lambda$coef[-1])
    # results[[paste0("sample_",j)]][[paste0("method_",nparams+1)]] <- list(params = "Unweighted model, SRS CV",
    #                                                                       l.error = l.error,
    #                                                                       l.lambda = l.lambda,
    #                                                                       l.coef = l.coef,
    #                                                                       l.coef.pop = l.coef.pop)
    # 
    # 
    # # Weighted model, SRS CV
    # newsample <- replicate.weights.unw(data = sample, weights = weights, k = 10, R=1, seed = seed*j*5)
    # l.error <- estimate.error.unw(data = newsample, lambda.grid , 
    #                               col.y, col.x, family, 
    #                               R, k, cv.error.ind = FALSE) 
    # l.lambda <- results.lambda(obj = l.error, data = newsample, col.y = col.y, col.x = col.x, weights = weights)
    # l.coef <- results.coef(beta, l.lambda$coef[-1])
    # l.coef.pop <- results.coef(coef(model.orig)[,which.min(pop.error)], l.lambda$coef[-1])
    # results[[paste0("sample_",j)]][[paste0("method_",nparams+2)]] <- list(params = "Weighted model, SRS CV",
    #                                                                       l.error = l.error,
    #                                                                       l.lambda = l.lambda,
    #                                                                       l.coef = l.coef,
    #                                                                       l.coef.pop = l.coef.pop)
    # 
    # 
    
    end.j <- Sys.time()
    time.j <- difftime(end.j, start.j, units = "secs")
    cat("----> ", as.numeric(time.j), " secs\n\n")
    
    results[[paste0("sample_",j)]]["computational.time"] <- as.numeric(time.j)
    
  }
  
  end <- Sys.time()
  
  results[["computational.time"]] <- difftime(end, start, units = "secs")
  
  return(results)
  
}
