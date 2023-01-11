#' Weighted LASSO prediction models for complex survey data
#'
#'@description This function allows as to fit LASSO prediction (linear or logistic) models to complex survey data, considering sampling weights in the estimation process and selects the lambda that minimizes the error based on different replicating weights methods.
#'
#' @param data A data frame with information about the response variable and covariates, sampling weights and strata and cluster indicators.
#' @param col.y column of the response variable. It could be indicated as the column number or the column name.
#' @param col.x vector of columns indicating the covariates. It could be defined by numbers or names of the columns.
#' @param cluster name of the column indicating clusters
#' @param strata name of the column indicating strata
#' @param weights name of the column indicating sampling weights
#' @param design an object of class \code{survey.design}. It could be \code{NULL} if information about \code{cluster}, \code{strata}, \code{weights} and \code{data} are given.
#' @param family family to fit LASSO models, choose between \code{gaussian} or \code{binomial}.
#' @param lambda.grid a grid for penalization parameters. If it is not defined by the user, the function will define one.
#' @param method method to be applied to define replicate weights, to choose between one of these: \code{JKn}, \code{dCV}, \code{bootstrap}, \code{subbootstrap}, \code{BRR}, \code{split}, \code{extrapolation}
#' @param k number of folds to be defined (only for \code{cv}). Default is \code{k=10}.
#' @param R number of times the sample is partioned (needed and used only for \code{cv}, \code{split} or \code{extrapolation} methods). Default R=1.
#' @param B number of bootstrap resamples (only for \code{bootstrap} and \code{subbootstrap} methods). Default \code{B=200}.
#' @param cv.error.ind method for estimating the error for \code{cv} method. FALSE (default) estimates the error for each test set and defines the cross-validated error as the average of all those errors. Option TRUE estimates the cross-validated error as the weighted average of the loss for each unit
#' @param train.prob probability for defining training sets (only for \code{split} and \code{extrapolation} methods)
#' @param method.split \code{cv} or \code{bootstrap} (only for \code{split} method)
#' @param seed define a seed
#'
#' @return An object of class \code{wlasso}.
#' @export
#'
#' @examples
#' data(simdata_lasso_binomial)
#' mcv <- wlasso(data = simdata_lasso_binomial,
#'               col.y = "y", col.x = 1:50,
#'               family = "binomial",
#'               cluster = "cluster", strata = "strata", weights = "weights",
#'               method = "dCV", k=10, R=20, seed = 100)
#'
#' # Or equivalently:
#' mydesign <- survey::svydesign(ids=~1, strata = ~strata, weights = ~weights,
#'                               nest = TRUE, data = simdata_lasso_binomial)
#' mcv <- wlasso(col.y = "y", col.x = 1:50, design = mydesign,
#'               family = "binomial",
#'               method = "dCV", k=10, R=20, seed = 100)


wlasso <- function(data = NULL, col.y = NULL, col.x = NULL,
                   cluster = NULL, strata = NULL, weights = NULL, design = NULL,
                   family = c("gaussian", "binomial"),
                   lambda.grid = NULL,
                   method = c("dCV", "JKn", "bootstrap", "subbootstrap", "BRR", "split", "extrapolation"),
                   k = 10, R = 1, B = 200,
                   cv.error.ind = FALSE,
                   train.prob = 0.7, method.split = c("dCV", "bootstrap", "subbootstrap"),
                   seed = NULL){

  # Step 0: Notation
  if(!is.null(design)){
    cluster <- as.character(design$call$ids[2])
    if(cluster == "1"){
      cluster <- NULL
    }
    strata <- as.character(design$call$strata[2])
    weights <- as.character(design$call$weights[2])
    data <- get(design$call$data)
  }

  # Step 1: Generate replicate weights based on the method
  newdata <- replicate.weights(data = data, method = method,
                               cluster = cluster, strata = strata, weights = weights,
                               k = k, R = R, B = B,
                               train.prob = train.prob, method.split = method.split,
                               seed = seed)

  # Step 2: if is.null(lambda.grid), then initialize it
  if(is.null(lambda.grid)){
    model.orig <- glmnet::glmnet(y = as.numeric(newdata[,col.y]),
                                 x = as.matrix(newdata[,col.x]),
                                 weights = as.numeric(newdata[,weights]),
                                 family = family)
    lambda.grid <- model.orig$lambda
  }

  # Step 3: Fit the training models and estimate yhat for units in the sample
  rwtraincols <- grep("_train", colnames(newdata))
  l.yhat <- list()

  for(col.w in rwtraincols){

    model <- glmnet::glmnet(y = as.numeric(newdata[,col.y]),
                            x = as.matrix(newdata[,col.x]),
                            weights = as.numeric(newdata[,col.w]),
                            lambda = lambda.grid,
                            family = family)

    # Sample yhat
    yhat <- predict(model, newx=as.matrix(newdata[,col.x]), type = "response")
    l.yhat[[length(l.yhat) + 1]] <- yhat
    names(l.yhat)[[length(l.yhat)]] <- paste0("yhat_", colnames(newdata)[col.w])

  }

  # Step 4: estimate the error in the test sets
  error <- error.f(data = newdata, l.yhat = l.yhat,
                   method = method, cv.error.ind = cv.error.ind,
                   R = R, k = k, B = B,
                   col.y = col.y, family = family, weights = weights)
  mean.error <- apply(error, 2, mean)

  lambda.min <- lambda.grid[which.min(mean.error)]

  model <- glmnet::glmnet(y = data[,col.y],
                          x = as.matrix(data[,col.x]),
                          weights = data[,weights],
                          lambda = lambda.min)


  result <- list(lambda.grid = lambda.grid,
                 lambda.min = lambda.min,
                 average.error = mean.error,
                 all.error = error,
                 model.min = model,
                 model.grid = model.orig)

  class(result) <- "wlasso"

  return(result)

}
