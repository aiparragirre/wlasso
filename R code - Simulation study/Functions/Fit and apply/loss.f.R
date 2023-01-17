
loss.f <- function(y.est, y, family = c("gaussian","binomial")){
  
  if(family == "gaussian"){
    l <- (y.est - y)^2
  }
  
  if(family == "binomial"){
    l <- rep(NA, length(y))
    l[which(y==1)] <- -log(y.est[which(y==1)])
    l[which(y==0)] <- -log(1-y.est[which(y==0)])
  }
  
  return(l)
  
}
