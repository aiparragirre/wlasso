replicate.weights <- function(data, 
                              method = c("JKn", "cv", "bootstrap", "subbootstrap", 
                                         "BRR", "split", "extrapolation"),
                              cluster = NULL, strata = NULL, weights = NULL,
                              k = NULL, B = NULL, R=NULL, 
                              train.prob = NULL, method.split = c("cv", "bootstrap", "subbootstrap"),
                              exact = FALSE, seed = 1){
  
  
  if(method == "cv"){
    
    newdata <- cv.folds(data, k, weights, seed, strata, cluster, R)
    
    for(r in 1:R){
      
      for(kk in 1:k){
        
        newdata[, paste0("sw_r_",r,"_test_", kk)] <- rep(0, nrow(newdata))
        newdata[which(newdata[,paste0("folds_",r)]==kk), paste0("sw_r_",r,"_test_", kk)] <- newdata[which(newdata[,paste0("folds_",r)]==kk), weights]
        
      }
      
    }
    
    
    
  } else {
    
    if(method == "split"){
      
      newdata <- rw.split(data, train.prob, method = method.split,
                          weights, strata, cluster, R, seed)
      
    } else {
      
      if(method == "extrapolation"){
        
        newdata <- split.strata(data, train.prob, strata, weights, seed, R)
        
      } else {
      
      # Define cluster formula
      if(is.null(cluster)) {
        formula.cluster <- as.formula("~1")
      } else {
        formula.cluster <- as.formula(paste0("~", cluster))
      }
      
      # Define strata formula
      if(!is.null(strata)) {
        formula.strata <- as.formula(paste0("~", strata))
      }
      
      # Define weights formula
      if(!is.null(weights)){
        formula.weights <- as.formula(paste0("~", weights))
      }
      
      # Define the design
      des <- svydesign(ids = formula.cluster, 
                       strata = formula.strata, 
                       weights = formula.weights, 
                       data = data, nest=TRUE)
      
      
      # Generate replicate weights based on the selected method
      if(method %in% c("JKn", "bootstrap", "subbootstrap", "BRR")){
        
        if(method %in% c("bootstrap", "subbootstrap")){
          rep.des <- as.svrepdesign(design = des, type = method, replicates = B)
        } else {
          rep.des <- as.svrepdesign(design = des, type = method)
        }
        
        mat.repw.ind <- apply(rep.des$repweights$weights, 2, function(x){x[rep.des$repweights$index]})
        mat.repw <- apply(mat.repw.ind, 2, function(x){x*data[,weights]})
        colnames(mat.repw) <- paste0("rw_r_1_train_", 1:ncol(mat.repw))
        newdata <- cbind(data, mat.repw)
        
      }
      
      # Define replicate weights for the testing set in BRR
      if(method == "BRR"){
        
        mat.repw.test <- mat.repw.ind
        colnames(mat.repw.test) <- paste0("rw_r_1_test_", 1:ncol(mat.repw.test))
        mat.repw.test <- -1*(mat.repw.test - 2)
        mat.repw.test <- apply(mat.repw.test, 2, function(x){x*data[,weights]})
        newdata <- cbind(newdata, mat.repw.test)
        
      }
      
      # Define each unit as fold in JKn method
      if(method == "JKn"){
        
        mat.repw.test <- -mat.repw.ind
        mat.repw.test[which(mat.repw.test==0)] <- 1
        mat.repw.test[which(mat.repw.test < 0)] <- 0
        mat.repw.test <- apply(mat.repw.test, 2, function(x){x*data[,weights]})
        colnames(mat.repw.test) <- paste0("sw_r_1_test_", 1:ncol(mat.repw.test))
        newdata <- cbind(newdata, mat.repw.test)
        
        #newdata$folds <- 1:nrow(newdata)
        
      }
      
      
    }
      
    }
    
  } 
  
  return(newdata)
  
}
