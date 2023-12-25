error.f <- function(data, l.yhat, method, cv.error.ind = c(TRUE, FALSE),
                    R = NULL, k = NULL, B = NULL,
                    col.y, family, weights = NULL){

  if(method == "JKn"){
    R <- 1
    k <- length(grep("rw", colnames(data)))
    initial.name <- "sw"
    cv.error.ind <- TRUE
  }

  if(method %in% c("bootstrap", "subbootstrap")){
    R <- 1
    k <- B
    weights.name <- weights
    cv.error.ind <- FALSE
  }

  if(method == "dCV"){
    if(cv.error.ind){
      initial.name <- "sw"
    } else {
      initial.name <- "rw"
    }
  }

  if(method == "BRR"){
    R <- 1
    k <- length(l.yhat)
    initial.name <- "rw"
    cv.error.ind <- FALSE
  }

  if(method %in% c("split","extrapolation")){
    k <- 1
    initial.name <- "rw"
    cv.error.ind <- FALSE
  }

  if(!cv.error.ind){
    error.lambda.r <- matrix(NA, nrow = R*k, ncol = dim(l.yhat[[1]])[2])
    rownames(error.lambda.r) <- 1:(R*k)
  }

  l.loss <- l.loss.w <- list()

  for(r in 1:R){

    for(kk in 1:k){

      # Define the names of the weights' columns
      if(method %in% c("JKn", "dCV", "BRR", "bootstrap", "subbootstrap")){
        yhat.name <- paste0("yhat_rw_r_",r,"_train_",kk)
      }

      if(method %in% c("JKn", "dCV", "BRR")){
        weights.name <- paste0(initial.name, "_r_",r,"_test_",kk)
      }

      if(method %in% c("split","extrapolation")){
        yhat.name <- paste0("yhat_rw_r_",r,"_train")
        weights.name <- paste0(initial.name, "_r_",r,"_test")
      }

      # Calculate the loss and the weighted loss
      l.loss[[yhat.name]] <- apply(l.yhat[[yhat.name]], 2, loss.f, y = as.numeric(data[,col.y]), family = family)
      l.loss.w[[yhat.name]] <- apply(l.loss[[yhat.name]], 2, function(x){x*data[, weights.name]})

      # Calculate the error
      if(!cv.error.ind){
        sumwi.k <- sum(data[, weights.name])
        error.lambda.r[(r-1)*k + kk,] <- apply(l.loss.w[[yhat.name]],2,sum)/sumwi.k
        rownames(error.lambda.r)[(r-1)*k + kk] <- paste0(method, "_r_", r, "_k_", kk)
      }

    }

  }

  if(cv.error.ind){

    error.lambda.r <- matrix(NA, nrow = R, ncol = dim(l.yhat[[1]])[2])

    for(r in 1:R){

      rcol.yhat <- grep(paste0("r_",r,"_"), names(l.loss.w))
      rcol.sw.newdata <- grep(paste0(initial.name,"_r_",r,"_test"), names(data))

      wi.li.r <- matrix(NA, nrow = length(rcol.yhat), ncol = dim(l.yhat[[1]])[2])
      for(ind in 1:length(rcol.yhat)){
        wi.li.r[ind,] <- apply(l.loss.w[[rcol.yhat[ind]]], 2, sum)
      }
      sum.wi.li.r <- apply(wi.li.r, 2, sum)

      sum.wi.r <- sum(data[,rcol.sw.newdata])
      error.lambda.r[r,] <- sum.wi.li.r/sum.wi.r

    }
  }

  return(error.lambda.r)


}



loss.f <- function(y.est, y, family = c("gaussian","binomial")){

  if(family == "gaussian"){
    l <- (y.est - y)^2
  }

  if(family == "binomial"){
    l <- rep(NA, length(y))
    l[which(y==1)] <- -log(y.est[which(y==1)])
    l[which(y==0)] <- -log(1-y.est[which(y==0)])
  }

  return(l)

}

