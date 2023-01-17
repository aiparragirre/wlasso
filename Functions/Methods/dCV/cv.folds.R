cv.folds <- function(data, k, weights, seed=NULL, strata=NULL, cluster=NULL, R=1){
  
  set.seed(seed)
  seeds <- runif(R)*10000
  
  for(r in 1:R){
    
    data[,paste0("folds_",r)] <- f.folds(data, k=k, seed = seeds[r], strata=strata, cluster=cluster)
    
    for(kk in 1:k){
      data[,paste0("rw_r_",r,"_train_", kk)] <- repl.weights(data, 
                                                             folds = paste0("folds_",r), 
                                                             test.fold=kk, 
                                                             weights,
                                                             strata, 
                                                             cluster)
    }
    
    for(kk in 1:k){
      data[,paste0("rw_r_",r,"_test_", kk)] <- repl.weights.test(data, 
                                                                folds = paste0("folds_",r), 
                                                                test.fold=kk, 
                                                                weights,
                                                                strata,
                                                                cluster)
      
    }
    
  }
  
  return(data)
  
}
