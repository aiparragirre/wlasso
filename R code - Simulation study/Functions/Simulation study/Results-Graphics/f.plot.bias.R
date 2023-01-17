f.plot.bias <- function(res, obj.name, ylim = NULL, opacity = 0.25,
                        l.width = 8, l.height = 5, scenario = NULL,
                        method.ind, method.names, colours = "lightgray"){
  
  pdf(file = paste0("Graphics/loglambda/bias-",obj.name,".pdf"), width = l.width, height=l.height)
  #par(mar = c(5, 4, 4, 2) + 0.1)
  l.loglambda.bias <- list()
  for(m in method.ind){
    l.loglambda.bias[[paste0("method_",m)]] <- log(res[[paste0("method_",m)]][["lambda.min"]]) - log(res[["pop.lambda.min"]])
  }
  names(l.loglambda.bias) <- method.names
  boxplot(l.loglambda.bias, las=2, cex.axis = 1, main = scenario, col = colours, ylim = ylim, ylab = "diff")
  abline(h=0, col="red")
  dev.off()
  
}
