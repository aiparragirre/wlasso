results.lambda <- function(obj, data, col.y, col.x, weights, lambda.grid){
  
  lambda.ids <- apply(obj$error$all, 1, which.min)
  all.lambda.min <- lambda.grid[lambda.ids]
  
  lambda.min <- lambda.grid[which.min(obj$error$mean)]
  
  model <- glmnet(y = data[,col.y], 
                  x = as.matrix(data[,col.x]), 
                  weights = data[,weights],
                  lambda = lambda.min)
  
  results <- list(all.lambda.min = all.lambda.min,
                  lambda.min = lambda.min, 
                  model = model,
                  coef = as.numeric(coef(model)))

  return(results)
  
}
