
#' Plot weighted LASSO object
#'
#' @param x object of class "wlasso"
#'
#' @return a graph
#' @export
#'
#' @examples
#' data(simdata_lasso_binomial)
#' mcv <- wlasso(data = simdata_lasso_binomial, col.y = "y", col.x = 1:50,
#'               family = "binomial", cluster = "cluster", strata = "strata",
#'               weights = "weights", method = "dCV", k=10, R=20, seed = 100)
#' plot(mcv)
plot.wlasso <- function(x){

  plot(x = log(x$lambda.grid), y = x$average.error, col = "red", pch = 20,
       xlab = bquote("log("~lambda~")"), ylab = "Average error")
  abline(v = log(x$lambda.min), lty = 2, col = "black")
  mtext(text = paste0("Variables in the selected model: ", length(which(as.numeric(coef(x$model.min))!=0))-1), side = 3)

}
