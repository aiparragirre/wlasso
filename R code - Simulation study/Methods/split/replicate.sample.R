
replicate.sample <- function(data, set, tag, strata, weights, r=1,
                             boot.type = c("bootstrap", "subbootstrap")){
  
  data[,paste0("bootrep_r_",r,"_",tag)] <- rep(0, nrow(data))
  data[which(data[,set] == tag),paste0("bootrep_r_",r,"_",tag)] <- 1
  
  nh0 <- table(data[,strata])
  if(boot.type == "bootstrap"){
    new.nh <- nh0
  } else {
    if(boot.type == "subbootstrap"){
      new.nh <- nh0 - 1
    }
  }
  
  nh0.tag <- table(data[which(data[,set] == tag), strata])
  
  for(hh in 1:length(unique(data[,strata]))){
    
    if(nh0.tag[hh] < new.nh[hh]){
      
      n.add <- new.nh[hh] - nh0.tag[hh]
      id.opt <- which(data[,set] == tag & data[,strata] == hh)
      if(length(id.opt)>1){
        selected.id <- sample(id.opt, size = n.add, replace = TRUE)
      } else {
        selected.id <- rep(id.opt, n.add)
      }
      n.adds <- table(selected.id)
      data[as.numeric(names(table(selected.id))),paste0("bootrep_r_",r,"_",tag)] <- data[as.numeric(names(table(selected.id))),paste0("bootrep_r_",r,"_",tag)] + n.adds 
      
    }
    
  }
  
  coef.h <- nh0/new.nh
  coef <- coef.h[match(data[,strata], names(coef.h))]
  
  data[,paste0("rw_r_",r,"_", tag)] <- data[, weights]*data[,paste0("bootrep_r_",r,"_",tag)]*coef
  
  # Delete bootstrap repetition columns
  col.bootrep <- grep("bootrep_", colnames(data))
  data <- data[,-col.bootrep]
  
  return(data)
  
}
