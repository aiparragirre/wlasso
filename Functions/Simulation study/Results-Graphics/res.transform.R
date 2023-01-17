res.transform <- function(res, method.ind){
  
  runs <- grep("sample_", names(res))
  
  methods <- grep("method_",names(res[[runs[1]]]))
  
  newres <- list()
  #newres[["lambda.grid"]] <- res[["sample_1"]][["method_1"]]$l.error$lambda.grid
  
  newres[["lambda.grid"]] <- newres[["population.error"]] <- newres[["pop.beta"]] <- list()
  newres[["pop.lambda.min"]] <- rep(NA, length(runs))
  newres[["pop.estimated.zeros"]] <- newres[["pop.estimated.nonzeros"]] <- rep(NA, length(runs))
  
  for(method in 1:(length(methods))){
    
    newres[[paste0("method_", method)]] <- list()
    newres[[paste0("method_", method)]][["error"]] <- list()
    newres[[paste0("method_", method)]][["estimated.beta"]] <- list()
    #newres[[paste0("method_", method)]][["all.lambda.min"]] <- list()
    newres[[paste0("method_", method)]][["lambda.min"]] <- rep(NA, length(runs))
    newres[[paste0("method_", method)]][["estimated.zeros"]] <- rep(NA, length(runs))
    newres[[paste0("method_", method)]][["estimated.nonzeros"]] <- rep(NA, length(runs))
    newres[[paste0("method_", method)]][["p.correct.class"]] <- rep(NA, length(runs))
    newres[[paste0("method_", method)]][["p.correct.zeros"]] <- rep(NA, length(runs))
    newres[[paste0("method_", method)]][["p.correct.nonzeros"]] <- rep(NA, length(runs))
    newres[[paste0("method_", method)]][["computation.time"]] <- rep(NA, length(runs))
    
  }
  
  for(run in 1:length(runs)){
    
    newres[["lambda.grid"]][[paste0("sample_",run)]] <- res[[paste0("sample_",run)]][["lambda.grid"]]
    
    newres[["population.error"]][[paste0("sample_",run)]] <- res[[paste0("sample_",run)]][["pop.error"]]
    newres[["pop.lambda.min"]][run] <- res[[paste0("sample_",run)]][["lambda.grid"]][which.min(res[[paste0("sample_",run)]][["pop.error"]])]
    newres[["pop.beta"]][[paste0("sample_",run)]] <- coef(res[[paste0("sample_",run)]][["model.orig"]])[,which.min(res[[paste0("sample_",run)]][["pop.error"]])]
    newres[["pop.estimated.zeros"]][run] <- length(which(coef(res[[paste0("sample_",run)]][["model.orig"]])[-1,which.min(res[[paste0("sample_",run)]][["pop.error"]])]==0))
    newres[["pop.estimated.nonzeros"]][run] <- length(which(coef(res[[paste0("sample_",run)]][["model.orig"]])[-1,which.min(res[[paste0("sample_",run)]][["pop.error"]])]!=0))
    
    #for(method in 1:(length(methods)-4)){
    for(method in 1:(length(methods))){  
      newres[[paste0("method_", method)]][["error"]][[paste0("sample_",run)]] <- res[[paste0("sample_",run)]][[paste0("method_",method)]]$l.error$error$mean
      newres[[paste0("method_", method)]][["estimated.beta"]][[paste0("sample_",run)]] <- res[[paste0("sample_",run)]][[paste0("method_",method)]]$l.lambda$coef
      #newres[[paste0("method_", method)]][["all.lambda.min"]][[paste0("sample_",run)]] <- res[[paste0("sample_",run)]][[paste0("method_",method)]]$l.lambda$all.lambda.min
      newres[[paste0("method_", method)]][["lambda.min"]][run] <- res[[paste0("sample_",run)]][[paste0("method_",method)]]$l.lambda$lambda.min
      newres[[paste0("method_", method)]][["estimated.zeros"]][run] <- res[[paste0("sample_",run)]][[paste0("method_",method)]]$l.coef$conf.matrix[3]
      newres[[paste0("method_", method)]][["estimated.nonzeros"]][run] <- res[[paste0("sample_",run)]][[paste0("method_",method)]]$l.coef$conf.matrix[6]
      newres[[paste0("method_", method)]][["p.correct.class"]][run] <- res[[paste0("sample_",run)]][[paste0("method_",method)]]$l.coef$p.correct.class
      newres[[paste0("method_", method)]][["p.correct.zeros"]][run] <- res[[paste0("sample_",run)]][[paste0("method_",method)]]$l.coef$p.correct.zeros
      newres[[paste0("method_", method)]][["p.correct.nonzeros"]][run] <- res[[paste0("sample_",run)]][[paste0("method_",method)]]$l.coef$p.correct.nonzeros
      newres[[paste0("method_", method)]][["p.correct.class.pop"]][run] <- res[[paste0("sample_",run)]][[paste0("method_",method)]]$l.coef.pop$p.correct.class
      newres[[paste0("method_", method)]][["p.correct.zeros.pop"]][run] <- res[[paste0("sample_",run)]][[paste0("method_",method)]]$l.coef.pop$p.correct.zeros
      newres[[paste0("method_", method)]][["p.correct.nonzeros.pop"]][run] <- res[[paste0("sample_",run)]][[paste0("method_",method)]]$l.coef.pop$p.correct.nonzeros
      
      newres[[paste0("method_", method)]][["computation.time"]][run] <- res[[paste0("sample_",run)]][[paste0("method_",method)]]$l.error$computational.time
      
    }
    
    # newres[[paste0("method_", length(methods)-3)]][["error"]][[paste0("sample_",run)]] <- res[[paste0("sample_",run)]][[paste0("method_",length(methods)-3)]]$l.error
    # newres[[paste0("method_", length(methods)-3)]][["lambda.min"]][run] <- res[[paste0("sample_",run)]][[paste0("method_",length(methods)-3)]]$lambda.min
    # newres[[paste0("method_", length(methods)-3)]][["estimated.zeros"]][run] <- res[[paste0("sample_",run)]][[paste0("method_",length(methods)-3)]]$l.coef$conf.matrix[3]
    # newres[[paste0("method_", length(methods)-3)]][["estimated.nonzeros"]][run] <- res[[paste0("sample_",run)]][[paste0("method_",length(methods)-3)]]$l.coef$conf.matrix[6]
    # newres[[paste0("method_", length(methods)-3)]][["p.correct.class"]][run] <- res[[paste0("sample_",run)]][[paste0("method_",length(methods)-3)]]$l.coef$p.correct.class
    # newres[[paste0("method_", length(methods)-3)]][["p.correct.zeros"]][run] <- res[[paste0("sample_",run)]][[paste0("method_",length(methods)-3)]]$l.coef$p.correct.zeros
    # newres[[paste0("method_", length(methods)-3)]][["p.correct.nonzeros"]][run] <- res[[paste0("sample_",run)]][[paste0("method_",length(methods)-3)]]$l.coef$p.correct.nonzeros
    # 
    # newres[[paste0("method_", length(methods)-2)]][["error"]][[paste0("sample_",run)]] <- res[[paste0("sample_",run)]][[paste0("method_",length(methods)-2)]]$l.error
    # newres[[paste0("method_", length(methods)-2)]][["lambda.min"]][run] <- res[[paste0("sample_",run)]][[paste0("method_",length(methods)-2)]]$lambda.min
    # newres[[paste0("method_", length(methods)-2)]][["estimated.zeros"]][run] <- res[[paste0("sample_",run)]][[paste0("method_",length(methods)-2)]]$l.coef$conf.matrix[3]
    # newres[[paste0("method_", length(methods)-2)]][["estimated.nonzeros"]][run] <- res[[paste0("sample_",run)]][[paste0("method_",length(methods)-2)]]$l.coef$conf.matrix[6]
    # newres[[paste0("method_", length(methods)-2)]][["p.correct.class"]][run] <- res[[paste0("sample_",run)]][[paste0("method_",length(methods)-2)]]$l.coef$p.correct.class
    # newres[[paste0("method_", length(methods)-2)]][["p.correct.zeros"]][run] <- res[[paste0("sample_",run)]][[paste0("method_",length(methods)-2)]]$l.coef$p.correct.zeros
    # newres[[paste0("method_", length(methods)-2)]][["p.correct.nonzeros"]][run] <- res[[paste0("sample_",run)]][[paste0("method_",length(methods)-2)]]$l.coef$p.correct.nonzeros
    # 
    # newres[[paste0("method_", length(methods)-1)]][["error"]][[paste0("sample_",run)]] <- res[[paste0("sample_",run)]][[paste0("method_",length(methods)-1)]]$l.error
    # newres[[paste0("method_", length(methods)-1)]][["lambda.min"]][run] <- res[[paste0("sample_",run)]][[paste0("method_",length(methods)-1)]]$lambda.min
    # newres[[paste0("method_", length(methods)-1)]][["estimated.zeros"]][run] <- res[[paste0("sample_",run)]][[paste0("method_",length(methods)-1)]]$l.coef$conf.matrix[3]
    # newres[[paste0("method_", length(methods)-1)]][["estimated.nonzeros"]][run] <- res[[paste0("sample_",run)]][[paste0("method_",length(methods)-1)]]$l.coef$conf.matrix[6]
    # newres[[paste0("method_", length(methods)-1)]][["p.correct.class"]][run] <- res[[paste0("sample_",run)]][[paste0("method_",length(methods)-1)]]$l.coef$p.correct.class
    # newres[[paste0("method_", length(methods)-1)]][["p.correct.zeros"]][run] <- res[[paste0("sample_",run)]][[paste0("method_",length(methods)-1)]]$l.coef$p.correct.zeros
    # newres[[paste0("method_", length(methods)-1)]][["p.correct.nonzeros"]][run] <- res[[paste0("sample_",run)]][[paste0("method_",length(methods)-1)]]$l.coef$p.correct.nonzeros
    # 
    # newres[[paste0("method_", length(methods))]][["error"]][[paste0("sample_",run)]] <- res[[paste0("sample_",run)]][[paste0("method_",length(methods))]]$l.error
    # newres[[paste0("method_", length(methods))]][["lambda.min"]][run] <- res[[paste0("sample_",run)]][[paste0("method_",length(methods))]]$lambda.min
    # newres[[paste0("method_", length(methods))]][["estimated.zeros"]][run] <- res[[paste0("sample_",run)]][[paste0("method_",length(methods))]]$l.coef$conf.matrix[3]
    # newres[[paste0("method_", length(methods))]][["estimated.nonzeros"]][run] <- res[[paste0("sample_",run)]][[paste0("method_",length(methods))]]$l.coef$conf.matrix[6]
    # newres[[paste0("method_", length(methods))]][["p.correct.class"]][run] <- res[[paste0("sample_",run)]][[paste0("method_",length(methods))]]$l.coef$p.correct.class
    # newres[[paste0("method_", length(methods))]][["p.correct.zeros"]][run] <- res[[paste0("sample_",run)]][[paste0("method_",length(methods))]]$l.coef$p.correct.zeros
    # newres[[paste0("method_", length(methods))]][["p.correct.nonzeros"]][run] <- res[[paste0("sample_",run)]][[paste0("method_",length(methods))]]$l.coef$p.correct.nonzeros
    
  }
  
  names(newres)[grep("method_", names(newres))] <- paste0("method_",method.ind)
  return(newres)
  
}
