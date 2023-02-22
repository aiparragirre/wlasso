
f.train.error <- function(res, run, methods, method.names, ylim, plot.title = "", 
                          cols, plot.type = c("train.to.pop", "train.to.test"),
                          x.legend, y.legend){
  
  plot(y = res[[paste0("sample_",run)]]$pop.error, 
       x = log(res[[paste0("sample_",run)]]$lambda.grid), 
       type = "l", ylim=ylim,
       xlab = "log(lambda)", ylab = "error", main = plot.title, lwd=2)
  
  
  if(plot.type == "train.to.pop"){
    for(m in methods){
      lines(y = res[[paste0("sample_",run)]][[paste0("method_",m)]]$l.error$error$population.mean, 
            x = log(res[[paste0("sample_",run)]]$lambda.grid), col = cols[which(methods %in% m)], lwd=2)
      abline(v = log(res[[paste0("sample_",run)]]$lambda.grid)[
        which.min(res[[paste0("sample_",run)]][[paste0("method_",m)]]$l.error$error$population.mean)], 
        lty = 2, col = cols[which(methods %in% m)])
    } 
  } else {
    for(m in methods){
      lines(y = res[[paste0("sample_",run)]][[paste0("method_",m)]]$l.error$error$mean, 
            x = log(res[[paste0("sample_",run)]]$lambda.grid), col = cols[which(methods %in% m)], lwd=2)
      abline(v = log(res[[paste0("sample_",run)]]$lambda.grid)[
        which.min(res[[paste0("sample_",run)]][[paste0("method_",m)]]$l.error$error$mean)], 
        col = cols[which(methods %in% m)], lty = 2)
    }
  }
  
  abline(v = log(res[[paste0("sample_",run)]]$lambda.grid)[
    which.min(res[[paste0("sample_",run)]]$pop.error)], lty = 2)
  
  legend(x = x.legend, y = y.legend, legend=method.names, col = cols, lwd = 2, cex=0.5, bty = "n")
  
  
  
  
}
