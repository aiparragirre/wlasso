f.folds <- function(data, k=5, exact.folds = FALSE, strata=NULL, cluster=NULL, seed=NULL){
  
  if(!is.null(seed)){set.seed(seed)}
  
  #if(!is.null(strata) & is.null(cluster)){
    
    # n <- nrow(data)
    # 
    # v.folds <- sample(cut(seq(1,n),breaks=k,labels=FALSE))
    # 
    # h.fold <- table(data[,strata], v.folds)!=0
    # h.fold.sum <- apply(h.fold, 1, sum)
    # h.onefold <- which(h.fold.sum==1)
    # if(length(h.onefold)!=0){
    #   for(hh in h.onefold){
    #     kk <- which(h.fold[hh,]==1)
    #     id.h <- which(data[,strata]==hh)
    #     set.seed(seed*10)
    #     selected.id <- sample(id.h, 1)
    #     set.seed(seed*100)
    #     newk <- sample(c(1:k)[-kk], 1)
    #     v.folds[selected.id] <- newk
    #   }
    # }
    # 
  #} else {
    
    #if(!is.null(strata) & !is.null(cluster)){
      
      data$hclus <- interaction(data[,strata], data[,cluster], drop=TRUE)
      newids <- levels(data$hclus)
      
      n <- length(newids)
      
      v.folds.newids <- sample(cut(seq(1,n),breaks=k,labels=FALSE))
      names(v.folds.newids) <- newids
      v.folds <- v.folds.newids[match(data$hclus, names(v.folds.newids))]
      
      h.fold <- table(data[,strata], v.folds)!=0
      h.fold.sum <- apply(h.fold, 1, sum)
      h.onefold <- which(h.fold.sum==1)
      if(length(h.onefold)!=0){
        for(hh in h.onefold){
          kk <- which(h.fold[hh,]==1)
          id.h <- which(data[,strata]==hh) 
          psu.h <- unique(data$hclus[id.h])
          set.seed(seed*10)
          selected.psu <- sample(psu.h, 1)
          set.seed(seed*100)
          newk <- sample(c(1:k)[-kk], 1)
          id.selected.psu <- which(data$hclus==selected.psu)
          v.folds[id.selected.psu] <- newk
        }
      }
      
    #}
    
  #}
  
  return(v.folds)
  
}
