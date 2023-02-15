f.plot.nvar <- function(res, obj.name, ylim = NULL, opacity = 0.25,
                        l.width = 8, l.height = 5, scenario = NULL,
                        method.ind, method.names, colours = "lightgray"){  
  
  # Number of variables in the final models
  pdf(file = paste0("Graphics/zeros/nvar-",obj.name,".pdf"), width = l.width, height=l.height)
  
  l.zeros <- list()
  l.zeros[["True"]] <- res[["pop.estimated.nonzeros"]]
  for(m in method.ind){
    l.zeros[[paste0("method_",m)]] <- res[[paste0("method_",m)]][["estimated.nonzeros"]]
  }
  names(l.zeros)[-1] <- method.names
  boxplot(l.zeros, las=2, cex.axis = 1, main = scenario, col = colours, ylab = "Number of variables")
  abline(v = 1.5, col = "gray", lty = 1, lwd=2)
  dev.off()
  
  
  # Difference between the number of variables between the fitted model and the "true" number of variables
  pdf(file = paste0("Graphics/zeros/diff-nvar-",obj.name,".pdf"), width = l.width, height=l.height)
  
  l.diffzeros <- list()
  for(m in method.ind){
    l.diffzeros[[paste0("method_",m)]] <- res[[paste0("method_",m)]][["estimated.nonzeros"]] - res[["pop.estimated.nonzeros"]]
  }
  names(l.diffzeros) <- method.names
  boxplot(l.diffzeros, las=2, cex.axis = 1, main = scenario, col = colours[-1], ylab = "Difference in number of variables")
  abline(h=0, col="red")
  dev.off()
  
}
