sample.2step <- function(data, nclus.h, n.ch, seed, cluster, strata){
  
  sample.ids <- vector(l=0)
  weights <- vector(l=0)
  
  count <- 0
  
  set.seed(seed)
  
  # Step 1: sample clusters
  for(hh in 1:length(nclus.h)){
    
    id.h <- which(data[,strata] == hh)
    id.ch <- unique(data[id.h, cluster])
    sel.ch <- sample(id.ch, size=nclus.h[hh])
    
    w.step1 <- length(id.ch)/nclus.h[hh]
    
    for(ch.i in 1:length(sel.ch)){
      
      count <- count+1
      
      id.h.ch <- which(data[,strata] == hh & data[,cluster] == sel.ch[ch.i])
      sel.ids <- sample(id.h.ch, size = n.ch[count])
      
      w.step2 <- length(id.h.ch)/length(sel.ids)
      
      sample.ids <- append(sample.ids, sel.ids)
      weights <- append(weights, rep(w.step1*w.step2, length(sel.ids)))
      
    }
    
  }
  
  sample <- data[sample.ids,]
  sample$weights <- weights
  
  return(sample)
  
}
