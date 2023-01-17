
estimate.error <- function(data, population,
                           method = c("JKn", "cv", "bootstrap", "subbootstrap", 
                                      "BRR", "split", "extrapolation"),
                           cluster = NULL, strata = NULL, weights = NULL,
                           k = NULL, B = NULL, R=NULL, cv.error.ind = NULL,
                           train.prob = NULL, method.split = c("cv", "bootstrap", "subbootstrap"),
                           w.folds = NULL, w.model = NULL, 
                           seed = 1, col.y, col.x, lambda.grid, family = "gaussian"
                           ){
  
  start <- Sys.time()
  
  # Generate replicate weights ----------------------------------------------

  if(method == "JKn"){
    
    newdata <- replicate.weights(data = data, method = method, 
                                 cluster = cluster, strata = strata, weights = weights, 
                                 seed = seed)
  
  } else {
    
    if(method == "cv"){
      if(w.folds == TRUE){
        newdata <- replicate.weights(data = data, method = method, 
                                     cluster = cluster, strata = strata, weights = weights,
                                     k = k, R = R,
                                     seed = seed)
      } else {
        if(w.model == TRUE){
          newdata <- replicate.weights.unw(data = data, weights = weights, 
                                           k = k, R=R, seed = seed)
        } else {
          newdata <- replicate.weights.unw(data = data, weights = NULL, 
                                           k = k, R = R, seed = seed)
        }
        
      }
      
    } else {
      
      if(method %in% c("bootstrap", "subbootstrap")){
        
        newdata <- replicate.weights(data = data, method = method,
                                     cluster = cluster, strata = strata, weights = weights,
                                     B = B, seed = seed)
        
      } else {
        
        if(method == "BRR"){
          
          newdata <- replicate.weights(data = data, method = method,
                                       cluster = cluster, strata = strata, weights = weights,
                                       seed = seed)
          
        } else {
          
          if(method == "split"){
            
            newdata <- replicate.weights(data = data, method = method,
                                         cluster = cluster, strata = strata, weights = weights,
                                         R = R, train.prob = train.prob, method.split = method.split,
                                         seed = seed)
            
          } else {
            
            if(method == "extrapolation"){
              
              newdata <- replicate.weights(data = data, method = method,
                                           cluster = cluster, strata = strata, weights = weights,
                                           R = R, train.prob = train.prob, 
                                           seed = seed)
              
            }
            
          }
          
        }
        
      }
      
    }
    
  }
  
  
  # 1) Initialize:
  #    Fit the lasso model to newdata, in order to define the lambda grid
  #    We need to do this first step for all the methods.
  #model.orig <- glmnet(y = as.numeric(newdata[,col.y]), 
  #                     x = as.matrix(newdata[,col.x]), 
  #                     weights = as.numeric(newdata[,weights]),
  #                     family = family)
  #lambda.grid <- model.orig$lambda
  #if(family == "binomial") {lambda.grid <- lambda.grid[-which(log(lambda.grid) < -7)]}
  
  # 2) Fit the model and estimate the response
  res <- fit.apply(data = newdata, population = population, col.y, col.x, family, lambda.grid)

  # 3) Estimate the error
  error <- error.f(data = newdata, res$l.yhat, method, cv.error.ind,
                   R = R, k = k, B = B, w.folds = w.folds,
                   col.y, family, weights)
  
  mean.error <- apply(error, 2, mean)
  
  end <- Sys.time()
  
  time <- as.numeric(difftime(end, start, units="secs"))
  
  results <- list(#data = newdata,
                  computational.time = time)
  
  
  if(method == "cv"){
    results$method$cv.error.ind <- cv.error.ind
  }
  if(method == "split"){
    results$method$train.prob <- train.prob
    results$method$method.split <- method.split
  }
  if(method == "extrapolation"){
    results$method$train.prob <- train.prob
  }

  if(!is.null(seed)){results$seed <- seed}
  
  #results$l.yhat <- l.yhat
  results$error$all <- error
  results$error$mean <- mean.error
  
  results$error$population <- res$m.error.pop
  results$error$population.mean <- apply(res$m.error.pop, 2, mean)
  
  return(results)
  

}
