
f.lines <- function(res, method.number, plot.title = NULL, ylim = NULL, opacity = 0.25){
  
  runs <- grep("sample_", names(res[[paste0("method_",method.number)]][["error"]]))
  
  min.loglambdas <- max.loglambdas <- min.errors <- max.errors <- rep(NA, length(res$lambda.grid))
  for(i in 1:length(res$lambda.grid)){
    
    min.loglambdas[i] <- min(log(res$lambda.grid[[paste0("sample_",i)]]))
    max.loglambdas[i] <- max(log(res$lambda.grid[[paste0("sample_",i)]]))
    min.errors[i] <- min(res$population.error[[paste0("sample_",i)]])
    inf.id <- which(res$population.error[[paste0("sample_",i)]] == "Inf")
    if(length(inf.id)>=1){
      res$population.error[[paste0("sample_",i)]][inf.id] <- max(res$population.error[[paste0("sample_",i)]][-which(res$population.error[[paste0("sample_",i)]] == "Inf")])
    }
    max.errors[i] <- max(res$population.error[[paste0("sample_",i)]])
  }
  min.loglambda <- min(min.loglambdas)
  max.loglambda <- max(max.loglambdas)
  min.error <- min(min.errors)
  max.error <- max(max.errors)
  
  # min.errors <- max.errors <- rep(NA, length(res[[paste0("method_",method.number)]][["error"]]))
  # for(i in 1:length(res[[paste0("method_",method.number)]][["error"]])){
  #   min.errors[i] <- min(res[[paste0("method_",method.number)]][["error"]][[i]])
  #   max.errors[i] <- max(res[[paste0("method_",method.number)]][["error"]][[i]])
  # }
  # min.error <- min(min.errors)
  # max.error <- max(max.errors)

  if(is.null(plot.title)) {plot.title = " "}
  if(is.null(ylim) & max.error > 10){ylim <- c(min.error-10, max.error+10)}
  if(is.null(ylim) & max.error < 10){ylim <- c(min.error-0.1, max.error+0.1)}
  
  plot(0, pch=19, cex=0.001,
       ylim = ylim, 
       xlim = c(min.loglambda-0.01,max.loglambda+0.01), 
       ylab = "",
       xlab = "log(lambda)",
       main = plot.title)
  
  for(run in 1:length(runs)){
    
    #v.error <- res[[paste0("method_",method.number)]]$error[[paste0("sample_",run)]]
    v.error <- res[["population.error"]][[paste0("sample_",run)]]
    v.lambda <- res[["lambda.grid"]][[paste0("sample_",run)]]
    
    lines(x = log(v.lambda), y = v.error, type="l", col="gray")

  }
  
  for(run in 1:length(runs)){
    
    abline(v = log(res[["pop.lambda.min"]][run]), col = scales::alpha("red", opacity), lty = 2)
    abline(v = log(res[[paste0("method_",method.number)]][["lambda.min"]][run]), col = scales::alpha("blue", opacity), lty = 2)
    
  }
  
  
  
}
