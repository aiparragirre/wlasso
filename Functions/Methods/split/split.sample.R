
split.sample <- function(data, train.prob, r,
                         strata = NULL, cluster = NULL, 
                         seed = 1){
  
  data[,strata] <- as.factor(data[,strata])
  
  set <- paste0("set_r_",r)
  
  set.seed(seed)
  
  #if(is.null(cluster)){
    
  #   n <- nrow(data)
  #   factor <- c(0, train.prob, 1)
  #   data[,set] <- as.factor(sample(cut(seq(1,n)/n, factor, labels = c("train", "test"))))
  #   
  #   train.0 <- table(data[which(data[,set]=="train"), strata])==0
  #   if(sum(train.0) != 0){
  #     h.0 <- which(train.0 == 1)
  #     for(hh in h.0){
  #       id.hh <- which(data[,strata]==hh)
  #       selected.id <- sample(id.hh, size=1)
  #       data[selected.id,set] <- "train"
  #     }
  #   }
  #   
  #   test.0 <- table(data[which(data[,set]=="test"), strata])==0
  #   if(sum(test.0) != 0){
  #     h.0 <- which(test.0 == 1)
  #     for(hh in h.0){
  #       id.hh <- which(data[,strata]==hh)
  #       selected.id <- sample(id.hh, size=1)
  #       data[selected.id,set] <- "test"
  #     }
  #   }
  #   
  # } else {
    
    data$hclus <- interaction(data[,strata], data[,cluster], drop=TRUE)
    newids <- levels(data$hclus)
    n <- length(newids)
    
    factor <- c(0, train.prob, 1)
    set.newids <- sample(cut(seq(1,n)/n, factor, labels = c("train", "test")))
    names(set.newids) <- newids
    data[,set] <- as.factor(set.newids[match(data$hclus, names(set.newids))])
    
    train.0 <- table(data[which(data[,set]=="train"), strata])==0
    if(sum(train.0) != 0){
      h.0 <- which(train.0 == 1)
      for(hh in h.0){
        id.hh <- which(data[,strata]==hh)
        psu.h <- unique(data$hclus[id.hh])
        selected.psu <- sample(psu.h, size=1)
        id.selected.psu <- which(data$hclus==selected.psu)
        data[id.selected.psu,set] <- "train"
      }
    }
    
    test.0 <- table(data[which(data[,set]=="test"), strata])==0
    if(sum(test.0) != 0){
      h.0 <- which(test.0 == 1)
      for(hh in h.0){
        id.hh <- which(data[,strata]==hh)
        psu.h <- unique(data$hclus[id.hh])
        selected.psu <- sample(psu.h, size=1)
        id.selected.psu <- which(data$hclus==selected.psu)
        data[id.selected.psu,set] <- "test"
      }
    }
    
  #}
  
  return(data)
  
}
