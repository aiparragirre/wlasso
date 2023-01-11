f.plot.error <- function(res, obj.name, ylim = NULL,
                         l.width = 8, l.height = 5,
                         method.ind, method.names, colours = "lightgray"){
  
  pdf(file = paste0("Graphics/error/error-",obj.name,".pdf"), width = l.width, height=l.height)
  par(mar = c(8, 4, 4, 2) + 0.1)
  
  l.error <- list()
  for(m in method.ind){
    lambda.id <- l.error[[paste0("method_",m)]] <- rep(NA, length(res[["lambda.grid"]]))
    for(r in 1:length(res[["lambda.grid"]])){
      lambda.id[r] <- which(res[["lambda.grid"]][[paste0("sample_",r)]] %in% res[[paste0("method_",m)]][["lambda.min"]][r])
      l.error[[paste0("method_",m)]][r] <- res[["population.error"]][[paste0("sample_",r)]][lambda.id[r]]
    }
  }
  names(l.error) <- method.names
  l.error[["True (pop)"]] <- rep(NA, length(res[["lambda.grid"]]))
  for(r in 1:length(res[["lambda.grid"]])){
    l.error[["True (pop)"]][r] <- min(res[["population.error"]][[paste0("sample_",r)]])
  }
  boxplot(l.error, las=2, cex.axis = 1, main = "Population error with the selected lambda", col = colours, ylim = ylim)
  abline(v = 9.5, col = "black", lty = 2)
  dev.off()
  
}
