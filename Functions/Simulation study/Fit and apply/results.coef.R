results.coef <- function(true, estimate){
  
  true.zeros <- which(true == 0)
  true.nonzeros <- which(true != 0)
  
  n.true.zeros <- length(true.zeros)
  n.true.nonzeros <- length(true.nonzeros)
  
  estimated.zeros <- which(estimate == 0)
  estimated.nonzeros <- which(estimate != 0)
  
  n.estimated.zeros <- length(estimated.zeros)
  n.estimated.nonzeros <- length(estimated.nonzeros)
  
  n.correct.zeros <- length(which(true.zeros %in% estimated.zeros))
  p.correct.zeros <- n.correct.zeros/length(true.zeros)
  
  n.correct.nonzeros <- length(which(true.nonzeros %in% estimated.nonzeros))
  p.correct.nonzeros <- n.correct.nonzeros/length(true.nonzeros)
  
  p.correct.class <- (n.correct.zeros + n.correct.nonzeros)/length(true)
  
  conf.matrix <- matrix(NA, ncol = 3, nrow = 3)
  colnames(conf.matrix) <- c("Estimated zeros", "Estimated non-zeros", "Total")
  rownames(conf.matrix) <- c("True zeros", "True nonzeros", "Total")
  conf.matrix[1,1] <- n.correct.zeros
  conf.matrix[1,2] <- n.true.zeros - n.correct.zeros
  conf.matrix[1,3] <- n.true.zeros
  conf.matrix[2,1] <- n.true.nonzeros - n.correct.nonzeros
  conf.matrix[2,2] <- n.correct.nonzeros
  conf.matrix[2,3] <- n.true.nonzeros
  conf.matrix[3,1] <- n.estimated.zeros
  conf.matrix[3,2] <- n.estimated.nonzeros
  conf.matrix[3,3] <- length(true)
  
  results <- list(conf.matrix = conf.matrix,
                  
                  #n.true.zeros = n.true.zeros,
                  #n.true.nonzeros = n.true.nonzeros,
                  #n.estimated.zeros = n.estimated.zeros,
                  #n.estimated.nonzeros = n.estimated.nonzeros,
                  
                  #n.correct.zeros = n.correct.zeros,
                  #n.correct.nonzeros = n.correct.nonzeros,
                  
                  p.correct.zeros = p.correct.zeros,
                  p.correct.nonzeros = p.correct.nonzeros,
                  p.correct.class = p.correct.class
                  )
  
  return(results)
}
