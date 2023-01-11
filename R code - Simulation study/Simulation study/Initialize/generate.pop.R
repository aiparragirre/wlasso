generate.pop <- function(npop = 10000, 
                         mu, rho, 
                         beta, gamma, var.clus = 0,
                         var.eps = NULL, beta0 = NULL,
                         family = c("gaussian", "binomial"),
                         H, C,
                         seed){
  
  ncoef <- length(mu) - var.clus
  
  sigma <- matrix(rho, nrow = ncoef, ncol = ncoef)
  for(i in 1:nrow(sigma)){
    sigma[i,i] <- 1
  }
  
  
  # Generate random variables
  set.seed(seed)
  if(var.clus == 0){
    variables <- mvrnorm(n = npop, mu = mu, Sigma = sigma)
    x <- variables[,1:length(beta)]
    z <- variables[,(length(beta)+1):length(mu)]
  } else {
    variables <- mvrnorm(n = npop, mu = mu[-(1:var.clus)], Sigma = sigma)
    x <- variables[,1:length(beta[-(1:var.clus)])]
    z <- variables[,(length(beta[-(1:var.clus)])+1):length(mu[-(1:var.clus)])]
  }
  
  
  if(family == "gaussian"){
    eps <- rnorm(npop, mean = 0, sd = sqrt(var.eps))
  } else {
    u <- runif(n = npop)
  }
  

  

  
  # Define the design
  pop <- data.frame(x = x)
  colnames(pop) <- paste0("x.", (var.clus+1):(length(colnames(pop)) + var.clus))
  pop <- pop[order(z %*% gamma),]
  pop$strata <- rep(1:H, each=npop/H)
  pop$cluster <- rep(rep(1:C, each=npop/(H*C)), H)
  
  if(var.clus != 0){
    # Generate cluster level covariates
    sigma.clus <- matrix(rho, nrow = var.clus, ncol = var.clus)
    for(i in 1:nrow(sigma.clus)){
      sigma.clus[i,i] <- 1
    }
    newx <- mvrnorm(n = H*C, mu = mu[1:var.clus], Sigma = sigma.clus)
    newx.N <- apply(newx, 2, function(x){rep(x, each = npop/(H*C))})
    new.pop <- cbind(newx.N, pop)
    names(new.pop) <- c(paste0("x.", 1:var.clus), names(pop))
    pop <- new.pop
    
    x <- cbind(newx.N, x)
  }
  
  if(family == "gaussian"){
    
    y <- x %*% beta + z %*% gamma + eps
    pop$y <- y[order(z %*% gamma)]
  
  } else {
    
    logitp <- x %*% beta + z %*% gamma + beta0
    p <- exp(logitp)/(1+exp(logitp))
    y <- ifelse(u < p, 1, 0)
    pop$y <- y[order(z %*% gamma)]
    
  }
  
  res <- list(data = pop, mu = mu, sigma = sigma, 
              beta = beta, gamma = gamma, var.eps = var.eps,
              var.clus = var.clus,
              family = family, 
              seed = seed)
  
  return(res)
  
}
