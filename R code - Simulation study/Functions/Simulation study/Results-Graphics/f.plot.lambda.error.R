f.plot.lambda.error <- function(res, obj.name, ylim = NULL, opacity = 0.25,
                                l.width = 8, l.height = 5, title = NULL,
                                method.ind, method.names, colours = "lightgray"){
  
  
  pdf(file = paste0("Graphics/error/error-lambda-",obj.name,".pdf"), width = l.width, height=l.height)
  
  par(mar = c(5,4,4,8) + 0.1)
  
  runs <- grep("sample_", names(res[[paste0("method_",method.ind[1])]][["error"]]))
  
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
  
  if(is.null(ylim) & max.error > 10){ylim <- c(min.error-10, max.error+10)}
  if(is.null(ylim) & max.error < 10){ylim <- c(min.error-0.1, max.error+0.1)}
  
  plot(0, pch=19, cex=0.001,
       ylim = ylim, 
       xlim = c(min.loglambda-0.01,max.loglambda+0.01), 
       ylab = "Error",
       xlab = "log(lambda)",
       main = title)
  
  for(run in 1:length(runs)){
    
    #v.error <- res[[paste0("method_",method.number)]]$error[[paste0("sample_",run)]]
    v.error <- res[["population.error"]][[paste0("sample_",run)]]
    v.lambda <- res[["lambda.grid"]][[paste0("sample_",run)]]
    
    lines(x = log(v.lambda), y = v.error, type="l", col="gray")
    
  }
  
  box.pos <- rev(seq(ylim[1], ylim[2], length.out = length(method.ind) + 1))
  pos <- 1
  
  boxplot(x = log(res[["pop.lambda.min"]]), 
          horizontal = TRUE, at = box.pos[pos], add = TRUE,
          col = colours[pos],
          boxwex = (ylim[2]-ylim[1])/10)
  
  for(method.number in method.ind){
    pos <- pos + 1
    boxplot(x = log(res[[paste0("method_",method.number)]][["lambda.min"]]), 
            horizontal = TRUE, at = box.pos[pos], add = TRUE,
            col = colours[pos],
            boxwex = (ylim[2]-ylim[1])/10)
  }
  
  
  
  axis(4, at = box.pos, labels = c("True (pop)", method.names), las = 2)
  
  dev.off()
  
  
  
}
