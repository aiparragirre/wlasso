
fit.apply <- function(data, population, col.y, col.x, family, lambda.grid){
  
  mat.x <- as.matrix(data[,col.x])
  
  rwtraincols <- grep("_train", colnames(data))
  
  l.yhat <- list()
  m.error.pop <- matrix(NA, nrow = length(rwtraincols), ncol = length(lambda.grid))
  rownames(m.error.pop) <- as.character(1:nrow(m.error.pop))
  
  k <- 0
  
  for(col.w in rwtraincols){
    
    model <- glmnet(y = as.numeric(data[,col.y]), 
                    x = mat.x, 
                    weights = as.numeric(data[,col.w]),
                    lambda = lambda.grid,
                    family = family)
    
    # Sample yhat
    yhat <- predict(model, newx=mat.x, type = "response")
    l.yhat[[length(l.yhat) + 1]] <- yhat
    names(l.yhat)[[length(l.yhat)]] <- paste0("yhat_", colnames(data)[col.w])
    
    # Population error for the training model
    k <- k+1
    m.error.pop[k,] <- population.error(population, model = model, col.x, col.y, family)
    rownames(m.error.pop)[k] <- paste0("pop.error_", colnames(data)[col.w])
    
  }
  
  return(res = list(l.yhat = l.yhat, m.error.pop = m.error.pop))
  
}
