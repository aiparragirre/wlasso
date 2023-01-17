
matrix.methods <- function(methods = c("JKn", "cv", "bootstrap", "subbootstrap", 
                                     "BRR", "split", "extrapolation"),
                          k = c(5,10), B = 200, R = c(1,20,100), 
                          cv.error.ind = c(TRUE,FALSE),
                          train.prob = 0.7, 
                          method.split = c("cv", "bootstrap", "subbootstrap"),
                          w.model = c(TRUE,FALSE)){
  
  m <- matrix(NA, nrow = 1, ncol = 9)
  m <- as.data.frame(m)
  colnames(m) <- c("method", "k", "B", "R", "cv.error.ind", "train.prob", "method.split", "w.folds", "w.model")
  
  a <- 0
  
  for(method in methods){
    
    if(method == "JKn"){
      a <- a+1
      m[a,"method"] <- method
      m[a,"w.folds"] <- TRUE
      m[a,"w.model"] <- TRUE
    }
    
    if(method == "cv"){
      for(kk in k){
        for(rr in R){
          for(ee in cv.error.ind){
            a <- a+1
            m[a,"method"] <- method
            m[a,"k"] <- kk
            m[a,"R"] <- rr
            m[a,"cv.error.ind"] <- ee
            m[a,"w.folds"] <- TRUE
            m[a,"w.model"] <- TRUE
          }
        }
      }
    }
    
    if(method %in% c("bootstrap","subbootstrap")){
      a <- a+1
      m[a, "method"] <- method
      m[a, "B"] <- B
      m[a,"w.folds"] <- TRUE
      m[a,"w.model"] <- TRUE
    }
    
    if(method == "BRR"){
      a <- a+1
      m[a, "method"] <- method
      m[a,"w.folds"] <- TRUE
      m[a,"w.model"] <- TRUE
    }
    
    if(method == "split"){
      for(rr in R){
        for(pp in train.prob){
          for(mm in method.split){
            a <- a+1
            m[a, "method"] <- method
            m[a, "R"] <- rr
            m[a, "train.prob"] <- pp
            m[a, "method.split"] <- mm
            m[a,"w.folds"] <- TRUE
            m[a,"w.model"] <- TRUE
          }
        }
      }
    }
    
    if(method == "extrapolation"){
      for(rr in R){
        for(pp in train.prob){
          a <- a+1
          m[a, "method"] <- method
          m[a, "R"] <- rr
          m[a, "train.prob"] <- pp
          m[a,"w.folds"] <- TRUE
          m[a,"w.model"] <- TRUE
        }
      }
    }
    
  }
  
  for(kk in k){
    for(rr in R){
      for(ee in cv.error.ind){
        for(ww in w.model){
          a <- a+1
          m[a,"method"] <- "cv"
          m[a,"k"] <- kk
          m[a,"R"] <- rr
          m[a,"cv.error.ind"] <- ee
          m[a,"w.folds"] <- FALSE
          m[a,"w.model"] <- ww
        }
      }
    }
  }
  
  return(m)
  
}


