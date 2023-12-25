#' Replicate weights
#'
#' @description This function allows calculating replicate weights.
#'
#' @param data A data frame with information of cluster and strata indicators and sampling weights.
#' @param method To choose between \code{JKn}, \code{dCV}, \code{bootstrap}, \code{subbootstrap}, \code{BRR}, \code{split} or \code{extrapolation}
#' @param cluster Name of the column indicating clusters
#' @param strata Name of the column indicating strata
#' @param weights Name of the column indicating sampling weights
#' @param k Number of folds, if \code{dCV} method is selected
#' @param B Number of bootstrap resamples if \code{bootstrap} or \code{subbootstrap} methods are selected
#' @param R Number of times the sample is split (only for \code{cv}, \code{split} and \code{extrapolation} methods)
#' @param train.prob Proportion of PSUs set as training set.
#' @param method.split To choose between \code{dCV}, \code{bootstrap} or \code{subbootstrap}
#' @param seed seed
#'
#' @return This function returns a new data frame with new columns, each of them indicating replicate weights for different subsets.
#' @export
#'
#' @examples
#' data(simdata_lasso_binomial)
#' newdata <- replicate.weights(data = simdata_lasso_binomial, method = "dCV",
#'                              cluster = "cluster", strata = "strata", weights = "weights",
#'                              k = 10, R = 20, seed = 1)
replicate.weights <- function(data,
                              method = c("JKn", "dCV", "bootstrap", "subbootstrap",
                                         "BRR", "split", "extrapolation"),
                              cluster = NULL, strata = NULL, weights = NULL,
                              k = 10, B = 200, R=1,
                              train.prob = NULL, method.split = c("dCV", "bootstrap", "subbootstrap"),
                              seed = NULL){


  if(method == "dCV"){

    newdata <- cv.folds(data, k, weights, seed, strata, cluster, R)

    for(r in 1:R){

      for(kk in 1:k){

        newdata[, paste0("sw_r_",r,"_test_", kk)] <- rep(0, nrow(newdata))
        newdata[which(newdata[,paste0("folds_",r)]==kk), paste0("sw_r_",r,"_test_", kk)] <- newdata[which(newdata[,paste0("folds_",r)]==kk), weights]

      }

    }



  } else {

    if(method == "split"){

      newdata <- rw.split(data, train.prob, method = method.split,
                          weights, strata, cluster, R, seed)

    } else {

      if(method == "extrapolation"){

        newdata <- split.strata(data, train.prob, strata, weights, seed, R)

      } else {

      # Define cluster formula
      if(is.null(cluster)) {
        formula.cluster <- as.formula("~1")
      } else {
        formula.cluster <- as.formula(paste0("~", cluster))
      }

      # Define strata formula
      if(!is.null(strata)) {
        formula.strata <- as.formula(paste0("~", strata))
      }

      # Define weights formula
      if(!is.null(weights)){
        formula.weights <- as.formula(paste0("~", weights))
      }

      # Define the design
      des <- survey::svydesign(ids = formula.cluster,
                               strata = formula.strata,
                               weights = formula.weights,
                               data = data, nest=TRUE)


      # Generate replicate weights based on the selected method
      if(method %in% c("JKn", "bootstrap", "subbootstrap", "BRR")){

        if(method %in% c("bootstrap", "subbootstrap")){
          rep.des <- survey::as.svrepdesign(design = des, type = method, replicates = B)
        } else {
          rep.des <- survey::as.svrepdesign(design = des, type = method)
        }

        mat.repw.ind <- apply(rep.des$repweights$weights, 2, function(x){x[rep.des$repweights$index]})
        mat.repw <- apply(mat.repw.ind, 2, function(x){x*data[,weights]})
        colnames(mat.repw) <- paste0("rw_r_1_train_", 1:ncol(mat.repw))
        newdata <- cbind(data, mat.repw)

      }

      # Define replicate weights for the testing set in BRR
      if(method == "BRR"){

        mat.repw.test <- mat.repw.ind
        colnames(mat.repw.test) <- paste0("rw_r_1_test_", 1:ncol(mat.repw.test))
        mat.repw.test <- -1*(mat.repw.test - 2)
        mat.repw.test <- apply(mat.repw.test, 2, function(x){x*data[,weights]})
        newdata <- cbind(newdata, mat.repw.test)

      }

      # Define each unit as fold in JKn method
      if(method == "JKn"){

        mat.repw.test <- -mat.repw.ind
        mat.repw.test[which(mat.repw.test==0)] <- 1
        mat.repw.test[which(mat.repw.test < 0)] <- 0
        mat.repw.test <- apply(mat.repw.test, 2, function(x){x*data[,weights]})
        colnames(mat.repw.test) <- paste0("sw_r_1_test_", 1:ncol(mat.repw.test))
        newdata <- cbind(newdata, mat.repw.test)

      }


    }

    }

  }

  return(newdata)

}



# dCV

cv.folds <- function(data, k, weights, seed=NULL, strata=NULL, cluster=NULL, R=1){

  set.seed(seed)
  seeds <- runif(R)*10000

  if(is.null(cluster) & !is.null(strata)){
    data$cluster <- 1:nrow(data)
    cluster <- "cluster"
  } else {
    if(!is.null(cluster) & is.null(strata)){
      data$strata <- rep(1, nrow(data))
      strata <- "strata"
    } else {
      if(is.null(cluster) & is.null(strata)){
        data$strata <- rep(1, nrow(data))
        data$cluster <- 1:nrow(data)
        strata <- "strata"
        cluster <- "cluster"
      }
    }
  }

  for(r in 1:R){

    data[,paste0("folds_",r)] <- f.folds(data, k=k, seed = seeds[r], strata=strata, cluster=cluster)

    for(kk in 1:k){
      data[,paste0("rw_r_",r,"_train_", kk)] <- repl.weights(data,
                                                             folds = paste0("folds_",r),
                                                             test.fold=kk,
                                                             weights,
                                                             strata,
                                                             cluster)
    }

    for(kk in 1:k){
      data[,paste0("rw_r_",r,"_test_", kk)] <- repl.weights.test(data,
                                                                 folds = paste0("folds_",r),
                                                                 test.fold=kk,
                                                                 weights,
                                                                 strata,
                                                                 cluster)

    }

  }

  return(data)

}



f.folds <- function(data, k=5, strata=NULL, cluster=NULL, seed=NULL){

  if(!is.null(seed)){set.seed(seed)}

  data$hclus <- interaction(data[,strata], data[,cluster], drop=TRUE)
  newids <- levels(data$hclus)
  n <- length(newids)

  v.folds.newids <- sample(cut(seq(1,n),breaks=k,labels=FALSE))
  names(v.folds.newids) <- newids
  v.folds <- v.folds.newids[match(data$hclus, names(v.folds.newids))]

  h.fold <- table(data[,strata], v.folds)!=0
  h.fold.sum <- apply(h.fold, 1, sum)
  h.onefold <- which(h.fold.sum==1)
  if(length(h.onefold)!=0){
    for(hh in h.onefold){
      kk <- which(h.fold[hh,]==1)
      id.h <- which(data[,strata]==hh)
      psu.h <- unique(data$hclus[id.h])
      set.seed(seed*10)
      selected.psu <- sample(psu.h, 1)
      set.seed(seed*100)
      newk <- sample(c(1:k)[-kk], 1)
      id.selected.psu <- which(data$hclus==selected.psu)
      v.folds[id.selected.psu] <- newk
    }
  }



  return(v.folds)

}


repl.weights <- function(data, folds, test.fold, weights, strata=NULL, cluster=NULL){

  v.repl.weights <- rep(0, nrow(data))

  id.test <- which(data[,folds]==test.fold)

  data[,strata] <- as.factor(data[,strata])

  str.clus <- table(data[,strata], data[,cluster])!=0
  str.clus.test <- table(data[id.test,strata], data[id.test,cluster])!=0

  v.mh <- apply(str.clus.test, 1, sum)
  v.nh <- apply(str.clus, 1, sum)
  coef <- v.nh/(v.nh - v.mh)

  v.repl.weights[-id.test] <- data[-id.test,weights]*coef[match(data[-id.test,strata], names(coef))]


  return(v.repl.weights)

}

repl.weights.test <- function(data, folds, test.fold, weights, strata=NULL, cluster=NULL){

  v.repl.weights <- rep(0, nrow(data))

  id.test <- which(data[,folds]!=test.fold)

  data[,strata] <- as.factor(data[,strata])

  str.clus <- table(data[,strata], data[,cluster])!=0
  str.clus.test <- table(data[id.test,strata], data[id.test,cluster])!=0

  v.mh <- apply(str.clus.test, 1, sum)
  v.nh <- apply(str.clus, 1, sum)
  coef <- v.nh/(v.nh - v.mh)

  v.repl.weights[-id.test] <- data[-id.test,weights]*coef[match(data[-id.test,strata], names(coef))]


  return(v.repl.weights)

}



# Split-sample ------------------------------------------------------------


rw.split <- function(data, train.prob, method = c("dCV", "bootstrap", "subbootstrap"),
                     weights, strata = NULL, cluster = NULL, R = 1,
                     seed = 1){

  set.seed(seed)
  seeds <- runif(R)*100000

  if(is.null(cluster) & !is.null(strata)){
    data$cluster <- 1:nrow(data)
    cluster <- "cluster"
  } else {
    if(!is.null(cluster) & is.null(strata)){
      data$strata <- rep(1, nrow(data))
      strata <- "strata"
    } else {
      if(is.null(cluster) & is.null(strata)){
        data$strata <- rep(1, nrow(data))
        data$cluster <- 1:nrow(data)
        strata <- "strata"
        cluster <- "cluster"
      }
    }
  }

  for(r in 1:R){

    data <- split.sample(data, train.prob, r, strata, cluster, seeds[r])
    tags <- as.vector(unique(data[,paste0("set_r_",r)]))

    if(method == "dCV"){

      for(tag in tags){
        data[,paste0("rw_r_",r,"_", tag)] <- repl.weights.test(data,
                                                               folds = paste0("set_r_",r),
                                                               test.fold = tag,
                                                               weights, strata, cluster)
      }

    } else {

      if(method %in% c("bootstrap", "subbootstrap")){

        for(tag in tags){

          data <- replicate.sample(data, set = paste0("set_r_",r), tag,
                                   strata, weights, r,
                                   boot.type = method)

        }


      }

    }

  }


  return(data)

}



split.sample <- function(data, train.prob, r,
                         strata = NULL, cluster = NULL,
                         seed = 1){

  data[,strata] <- as.factor(data[,strata])

  set <- paste0("set_r_",r)

  set.seed(seed)

  data$hclus <- interaction(data[,strata], data[,cluster], drop=TRUE)
  newids <- levels(data$hclus)
  n <- length(newids)

  factor <- c(0, train.prob, 1)
  set.newids <- sample(cut(seq(1,n)/n, factor, labels = c("train", "test")))
  names(set.newids) <- newids
  data[,set] <- as.factor(set.newids[match(data$hclus, names(set.newids))])

  train.0 <- table(data[which(data[,set]=="train"), strata])==0
  if(sum(train.0) != 0){
    h.0 <- which(train.0 == 1)
    for(hh in h.0){
      id.hh <- which(data[,strata]==hh)
      psu.h <- unique(data$hclus[id.hh])
      selected.psu <- sample(psu.h, size=1)
      id.selected.psu <- which(data$hclus==selected.psu)
      data[id.selected.psu,set] <- "train"
    }
  }

  test.0 <- table(data[which(data[,set]=="test"), strata])==0
  if(sum(test.0) != 0){
    h.0 <- which(test.0 == 1)
    for(hh in h.0){
      id.hh <- which(data[,strata]==hh)
      psu.h <- unique(data$hclus[id.hh])
      selected.psu <- sample(psu.h, size=1)
      id.selected.psu <- which(data$hclus==selected.psu)
      data[id.selected.psu,set] <- "test"
    }
  }



  return(data)

}



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



# Extrapolation -----------------------------------------------------------


split.strata <- function(data, train.prob, strata = NULL, weights, seed = 1, R = 1){

  if(is.null(strata)){stop("Extrapolation method cannot be applied if strata are not defined")}

  set.seed(seed)
  seeds <- runif(R)*1000

  h <- unique(data[,strata])

  for(r in 1:R){

    set.seed(seeds[r])

    number.h <- floor(length(h)*train.prob)
    train.h <- sample(1:length(h), number.h)

    h.split <- vector(l = length(h))
    names(h.split) <- h

    h.split[train.h] <- "train"
    h.split[-train.h] <- "test"

    data[, paste0("set_", r)] <- as.vector(h.split[match(data[,strata], names(h.split))])

    data[, paste0("rw_r_",r,"_train")] <- rep(0, nrow(data))
    id.train <- which(data[,paste0("set_",r)] == "train")
    data[id.train, paste0("rw_r_",r,"_train")] <- data[id.train, weights]

    data[, paste0("rw_r_",r,"_test")] <- rep(0, nrow(data))
    id.test <- which(data[,paste0("set_",r)] == "test")
    data[id.test, paste0("rw_r_",r,"_test")] <- data[id.test, weights]

  }


  return(data)

}

