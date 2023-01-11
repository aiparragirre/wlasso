
rw.split <- function(data, train.prob, method = c("cv", "bootstrap", "subbootstrap"),
                     weights, strata = NULL, cluster = NULL, R = 1,
                     seed = 1){
  
  set.seed(seed)
  seeds <- runif(R)*100000
  
  for(r in 1:R){
    
    data <- split.sample(data, train.prob, r, strata, cluster, seeds[r])
    tags <- as.vector(unique(data[,paste0("set_r_",r)]))
    
    if(method == "cv"){
      
      for(tag in tags){
        data[,paste0("rw_r_",r,"_", tag)] <- repl.weights.test(data, 
                                                       folds = paste0("set_r_",r), 
                                                       test.fold = tag, 
                                                       weights, strata, cluster)
      }
      
    } else {
      
      if(method %in% c("bootstrap", "subbootstrap")){
        
        for(tag in tags){
          
          data <- replicate.sample(data, set = paste0("set_r_",r), tag, 
                                   strata, weights, r,
                                   boot.type = method)  
          
        }
        
        
      }
      
    }
    
  }
  
  
  return(data)
  
}
