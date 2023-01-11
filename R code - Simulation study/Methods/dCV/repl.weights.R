repl.weights <- function(data, folds, test.fold, weights, strata=NULL, cluster=NULL){
  
  v.repl.weights <- rep(0, nrow(data))
  
  id.test <- which(data[,folds]==test.fold)
  
  # if(!is.null(strata) & is.null(cluster)){
  #   
  #   data[,strata] <- as.factor(data[,strata])
  #   
  #   v.mh <- table(data[id.test, strata])
  #   v.nh <- table(data[,strata])
  #   coef <- v.nh/(v.nh - v.mh)
  #   
  #   v.repl.weights[-id.test] <- data[-id.test,weights]*coef[match(data[-id.test,strata], names(coef))]
  #   
  # } else {
  #   
  #   if(is.null(strata) & !is.null(cluster)){
  #     
  #     data[,cluster] <- as.factor(data[,cluster])
  #     
  #     v.mh <- length(which(table(data[id.test, cluster])!=0))
  #     v.nh <- length(table(data[,cluster]))
  #     coef <- v.nh/(v.nh - v.mh)
  #     
  #     v.repl.weights[-id.test] <- data[-id.test,weights]*coef
  #     
  #   } else {
  #     
  #     if(!is.null(strata) & !is.null(cluster)){
  #       
        
        data[,strata] <- as.factor(data[,strata])
        
        str.clus <- table(data[,strata], data[,cluster])!=0
        str.clus.test <- table(data[id.test,strata], data[id.test,cluster])!=0
        
        v.mh <- apply(str.clus.test, 1, sum)
        v.nh <- apply(str.clus, 1, sum)
        coef <- v.nh/(v.nh - v.mh)
        
        v.repl.weights[-id.test] <- data[-id.test,weights]*coef[match(data[-id.test,strata], names(coef))]
        
      #}
      
    #}
  #}
  
  
  return(v.repl.weights)
  
}
