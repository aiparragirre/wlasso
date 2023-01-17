replicate.weights.unw <- function(data, weights = NULL, k = NULL, R=NULL, seed = 1){
  
  set.seed(seed)
  newdata <- data
  
  if(!is.null(weights)){
    
    for(r in 1:R){
      
      newdata[,paste0("folds_",r)] <- sample(cut(seq(1,nrow(newdata)),breaks=k,labels=FALSE))
      
      for(kk in 1:k){
        
        newdata[, paste0("sw_r_",r,"_train_", kk)] <- rep(0, nrow(newdata))  
        newdata[, paste0("sw_r_",r,"_test_", kk)] <- rep(0, nrow(newdata))
        newdata[which(newdata[,paste0("folds_",r)]!=kk), paste0("sw_r_",r,"_train_", kk)] <- newdata[which(newdata[,paste0("folds_",r)]!=kk), weights]
        newdata[which(newdata[,paste0("folds_",r)]==kk), paste0("sw_r_",r,"_test_", kk)] <- newdata[which(newdata[,paste0("folds_",r)]==kk), weights]
        
      }
      
    }
    
  } else {
    
    for(r in 1:R){
      
      newdata[,paste0("folds_",r)] <- sample(cut(seq(1,nrow(newdata)),breaks=k,labels=FALSE))
      
      for(kk in 1:k){
        
        newdata[, paste0("sw_r_",r,"_train_", kk)] <- rep(0, nrow(newdata))  
        newdata[, paste0("sw_r_",r,"_test_", kk)] <- rep(0, nrow(newdata))
        newdata[which(newdata[,paste0("folds_",r)]!=kk), paste0("sw_r_",r,"_train_", kk)] <- 1
        newdata[which(newdata[,paste0("folds_",r)]==kk), paste0("sw_r_",r,"_test_", kk)] <- 1
        
      }
      
    }
    
  }
  
  return(newdata)
  
}
