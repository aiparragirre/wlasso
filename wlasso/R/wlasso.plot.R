
#' Plot weighted LASSO object
#'
#' @param x an object of class "wlasso".
#'
#' @return a graph
#' @export
#'
#' @examples
#' data(simdata_lasso_binomial)
#' mcv <- wlasso(data = simdata_lasso_binomial, col.y = "y", col.x = 1:50,
#'               family = "binomial", cluster = "cluster", strata = "strata",
#'               weights = "weights", method = "dCV", k=10, R=20)
#' wlasso.plot(mcv)
wlasso.plot <- function(x){

  if(!inherits(x, "wlasso")){stop("Please, insert an object of class 'wlasso'.")}

  plot(x = log(x$lambda$grid), y = x$error$average, col = "red", pch = 20,
       xlab = bquote("log("~lambda~")"), ylab = "Average error")
  abline(v = log(x$lambda$min), lty = 2, col = "black")
  mtext(text = paste0("Variables in the selected model: ", x$model$min$df), side = 3)

}
