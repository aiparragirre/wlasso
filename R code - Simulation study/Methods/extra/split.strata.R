
split.strata <- function(data, train.prob, strata = NULL, weights, seed = 1, R = 1){
  
  set.seed(seed)
  seeds <- runif(R)*1000
  
  h <- unique(data[,strata])
  
  for(r in 1:R){
      
    set.seed(seeds[r])
    
    number.h <- floor(length(h)*train.prob)
    train.h <- sample(1:length(h), number.h)
    
    h.split <- vector(l=length(h))
    names(h.split) <- h
    
    h.split[train.h] <- "train"
    h.split[-train.h] <- "test"
    
    data[, paste0("set_", r)] <- as.vector(h.split[match(data[,strata], names(h.split))])
    
    data[, paste0("rw_r_",r,"_train")] <- rep(0, nrow(data))
    id.train <- which(data[,paste0("set_",r)] == "train")
    data[id.train, paste0("rw_r_",r,"_train")] <- data[id.train, weights]
    
    data[, paste0("rw_r_",r,"_test")] <- rep(0, nrow(data))
    id.test <- which(data[,paste0("set_",r)] == "test")
    data[id.test, paste0("rw_r_",r,"_test")] <- data[id.test, weights]
    
  }
    
    
  return(data)
  
}
