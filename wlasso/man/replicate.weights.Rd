% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/replicate.weights.R
\name{replicate.weights}
\alias{replicate.weights}
\title{Replicate weights}
\usage{
replicate.weights(
  data,
  method = c("JKn", "dCV", "bootstrap", "subbootstrap", "BRR", "split", "extrapolation"),
  cluster = NULL,
  strata = NULL,
  weights = NULL,
  k = 10,
  B = 200,
  R = 1,
  train.prob = NULL,
  method.split = c("dCV", "bootstrap", "subbootstrap"),
  seed = NULL
)
}
\arguments{
\item{data}{A data frame with information of cluster and strata indicators and sampling weights.}

\item{method}{To choose between \code{JKn}, \code{dCV}, \code{bootstrap}, \code{subbootstrap}, \code{BRR}, \code{split} or \code{extrapolation}}

\item{cluster}{Name of the column indicating clusters}

\item{strata}{Name of the column indicating strata}

\item{weights}{Name of the column indicating sampling weights}

\item{k}{Number of folds, if \code{dCV} method is selected}

\item{B}{Number of bootstrap resamples if \code{bootstrap} or \code{subbootstrap} methods are selected}

\item{R}{Number of times the sample is split (only for \code{cv}, \code{split} and \code{extrapolation} methods)}

\item{train.prob}{Proportion of PSUs set as training set.}

\item{method.split}{To choose between \code{dCV}, \code{bootstrap} or \code{subbootstrap}}

\item{seed}{seed}
}
\value{
This function returns a new data frame with new columns, each of them indicating replicate weights for different subsets.
}
\description{
This function allows calculating replicate weights.
}
\examples{
data(simdata_lasso_binomial)
newdata <- replicate.weights(data = simdata_lasso_binomial, method = "dCV",
                             cluster = "cluster", strata = "strata", weights = "weights",
                             k = 10, R = 20, seed = 1)
}
