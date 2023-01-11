
f.pop.error <- function(res, method, title, run, col, ylim){
  
  plot(y = res[[paste0("sample_",run)]]$pop.error, 
       x = log(res[[paste0("sample_",run)]]$lambda.grid), 
       type = "l", ylim = ylim,
       xlab = "log(lambda)", ylab = "error", main = title, lwd=2)
  abline(v = log(res[[paste0("sample_",run)]]$lambda.grid)[
    which.min(res[[paste0("sample_",run)]]$pop.error)], lty = 2)
  
  ntrain.pop <- nrow(res[[paste0("sample_",run)]][[paste0("method_",method)]]$l.error$error$population)
  
  for(i in 1:ntrain.pop){
    
    lines(y = res[[paste0("sample_",run)]][[paste0("method_",method)]]$l.error$error$population[i,], 
          x = log(res[[paste0("sample_",run)]]$lambda.grid), col = scales::alpha("gray", 0.25), lty = 2, lwd=2)
  }
  
  ntrain.test <- nrow(res[[paste0("sample_",run)]][[paste0("method_",method)]]$l.error$error$all)
  
  for(i in 1:ntrain.test){
    
    lines(y = res[[paste0("sample_",run)]][[paste0("method_",method)]]$l.error$error$all[i,], 
          x = log(res[[paste0("sample_",run)]]$lambda.grid), col = scales::alpha("gray", 0.25), lwd=2)
    
  }
  
  lines(y = res[[paste0("sample_",run)]][[paste0("method_",method)]]$l.error$error$population.mean, 
        x = log(res[[paste0("sample_",run)]]$lambda.grid), col = col, lty = 2, lwd=2)
  abline(v = log(res[[paste0("sample_",run)]]$lambda.grid)[
    which.min(res[[paste0("sample_",run)]][[paste0("method_",method)]]$l.error$error$population.mean)], 
    lty = 2, col = col)
  
  lines(y = res[[paste0("sample_",run)]][[paste0("method_",method)]]$l.error$error$mean, 
        x = log(res[[paste0("sample_",run)]]$lambda.grid), col = col, lwd=2)
  abline(v = log(res[[paste0("sample_",run)]]$lambda.grid)[
    which.min(res[[paste0("sample_",run)]][[paste0("method_",method)]]$l.error$error$mean)], 
    col = col)
  
  legend(x = x.legend, y = y.legend, legend = c("model --> population", "tr.model --> population", "tr.model --> test"), 
         col = c("black", col, col), lty = c(1, 2, 1), lwd = 2, cex= 0.5 , bty = "n", seg.len=5)
  
}


