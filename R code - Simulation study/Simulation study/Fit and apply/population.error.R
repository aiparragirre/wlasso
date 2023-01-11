population.error <- function(population, model, col.x, col.y, family){
  
  yhat <- predict(model, newx=as.matrix(population[,col.x]), type = "response")
  
  l.loss <- apply(yhat, 2, loss.f, y = as.numeric(population[,col.y]), family = family)
  
  error <- apply(l.loss, 2, mean)
  
  return(error)
  
}
