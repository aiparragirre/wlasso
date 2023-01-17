rm(list=ls())

# Packages ----------------------------------------------------------------

library(MASS)
library(survey)
library(glmnet)

# Functions ---------------------------------------------------------------

sapply(paste0("Functions/",list.files(path = "Functions", full.names=F, recursive = TRUE, pattern='\\.[rR]$')), source)

# Params ------------------------------------------------------------------

m.params <- matrix.methods(methods = c("JKn", "cv", "subbootstrap", 
                                       "BRR", "split", "extrapolation"),
                           k = c(5,10), B = 200, R = c(1,5,20), 
                           cv.error.ind = c(TRUE,FALSE),
                           train.prob = 0.7, 
                           method.split = c("cv", "subbootstrap"))

method.names <- c("JKn", 
                  "K=5, R=1, T", "K=5, R=1, F",
                  "K=5, R=5, T", "K=5, R=5, F",
                  "K=5, R=20, T", "K=5, R=20, F",
                  "K=10, R=1, T", "K=10, R=1, F",
                  "K=10, R=5, T", "K=10, R=5, F",
                  "K=10, R=20, T", "K=10, R=20, F",
                  "Bootstrap (n-1)",
                  "BRR",
                  "Split R=1 (cv)", "Split R=1 (boot)",
                  "Split R=5 (cv)", "Split R=5 (boot)",
                  "Split R=20 (cv)", "Split R=20 (boot)",
                  "Extrapolation R=1", "Extrapolation R=5", "Extrapolation R=20",
                  "SRSCV K=5, R=1, T, w", "SRSCV K=5, R=1, T, unw",
                  "SRSCV K=5, R=1, F, w", "SRSCV K=5, R=1, F, unw",
                  "SRSCV K=5, R=5, T, w", "SRSCV K=5, R=5, T, unw",
                  "SRSCV K=5, R=5, F, w", "SRSCV K=5, R=5, F, unw",
                  "SRSCV K=5, R=20, T, w", "SRSCV K=5, R=20, T, unw",
                  "SRSCV K=5, R=20, F, w", "SRSCV K=5, R=20, F, unw",
                  "SRSCV K=10, R=1, T, w", "SRSCV K=10, R=1, T, unw",
                  "SRSCV K=10, R=1, F, w", "SRSCV K=10, R=1, F, unw",
                  "SRSCV K=10, R=5, T, w", "SRSCV K=10, R=5, T, unw",
                  "SRSCV K=10, R=5, F, w", "SRSCV K=10, R=5, F, unw",
                  "SRSCV K=10, R=20, T, w", "SRSCV K=10, R=20, T, unw",
                  "SRSCV K=10, R=20, F, w", "SRSCV K=10, R=20, F, unw")


# Scenario 1: 0 cluster-level variables -----------------------------------

# Linear

pop_linear_scenario1 <- generate.pop(npop = 100000,
                                     mu = rep(0, 75),
                                     rho = 0.15,
                                     var.eps = 10^2,
                                     beta = c(rep(-2,11),rep(0,9),rep(-2,10),rep(0,9),rep(-2,11)),
                                     gamma = rep(2, 25),
                                     family = "gaussian",
                                     H = 5, C = 20,
                                     seed = 1)


res_linear_scenario1 <- general.f(population = pop_linear_scenario1$data,
                                  beta = pop_linear_scenario1$beta,
                                  m.params = m.params[c(1, 9, 14, 15, 20:21, 24, 39:40),],
                                  cluster = "cluster", strata = "strata", weights = "weights",
                                  nh = NULL,
                                  nclus.h = rep(4, 5),
                                  n.ch = c(rep(500, 4), rep(50, 4), rep(25, 4), rep(10, 4), rep(5, 4)),
                                  seed = 1,
                                  col.y = "y",
                                  col.x = grep("x.", names(pop_linear_scenario1$data)),
                                  family = "gaussian",
                                  nrun = 500)
save(res_linear_scenario1, file="Simulations/res_linear_scenario1.RData")


# Logistic

pop_logistic_scenario1 <- generate.pop(npop = 100000,
                                       mu = rep(0, 75),
                                       rho = 0.15,
                                       beta0 = -10,
                                       beta = c(rep(-2,11),rep(0,9),rep(-2,10),rep(0,9),rep(-2,11)),
                                       gamma = rep(2, 25),
                                       family = "binomial",
                                       H = 5, C = 20,
                                       seed = 1)

res_logistic_scenario1 <- general.f(population = pop_logistic_scenario1$data,
                                    beta = pop_logistic_scenario1$beta,
                                    m.params = m.params[c(1, 9, 14, 15, 20:21, 24, 39:40),],
                                    cluster = "cluster", strata = "strata", weights = "weights",
                                    nh = NULL,
                                    nclus.h = rep(4, 5),
                                    n.ch = c(rep(5, 4), rep(10, 4), rep(25, 4), rep(50, 4), rep(500, 4)),
                                    seed = 1,
                                    col.y = "y",
                                    col.x = grep("x.", names(pop_logistic_scenario1$data)),
                                    family = "binomial",
                                    nrun = 500)
save(res_logistic_scenario1, file="Simulations/res_logistic_scenario1.RData")



# Scenario 2: 5 cluster-level variables -----------------------------------

# Linear

pop_linear_scenario2 <- generate.pop(npop = 100000,
                                     mu = rep(0, 75),
                                     rho = 0.15,
                                     var.eps = 10^2,
                                     beta = c(rep(-2,11),rep(0,9),rep(-2,10),rep(0,9),rep(-2,11)),
                                     gamma = rep(2, 25),
                                     var.clus = 5,
                                     family = "gaussian",
                                     H = 5, C = 20,
                                     seed = 1)

res_linear_scenario2 <- general.f(population = pop_linear_scenario2$data,
                                  beta = pop_linear_scenario2$beta,
                                  m.params = m.params[c(1, 9, 14, 15, 20:21, 24, 39:40),],
                                  cluster = "cluster", strata = "strata", weights = "weights",
                                  nh = NULL,
                                  nclus.h = rep(4, 5),
                                  n.ch = c(rep(250, 4), rep(100, 4), rep(50, 4), rep(25, 4), rep(5, 4)),
                                  seed = 1,
                                  col.y = "y",
                                  col.x = grep("x.", names(pop_linear_scenario2$data)),
                                  family = "gaussian",
                                  nrun = 500)
save(res_linear_scenario2, file="Simulations/res_linear_scenario2.RData")


# Logistic

pop_logistic_scenario2 <- generate.pop(npop = 100000,
                                       mu = rep(0, 75),
                                       rho = 0.15,
                                       var.eps = 10^2,
                                       beta = c(rep(-2,11),rep(0,9),rep(-2,10),rep(0,9),rep(-2,11)),
                                       gamma = rep(2, 25),
                                       var.clus = 5,
                                       beta0 = -10,
                                       family = "binomial",
                                       H = 5, C = 20,
                                       seed = 1)

res_logistic_scenario2 <- general.f(population = pop_logistic_scenario2$data,
                                    beta = pop_logistic_scenario2$beta,
                                    m.params = m.params[c(1, 9, 14, 15, 20:21, 24, 39:40),],
                                    cluster = "cluster", strata = "strata", weights = "weights",
                                    nh = NULL,
                                    nclus.h = rep(4, 5),
                                    n.ch = c(rep(5, 4), rep(25, 4), rep(50, 4), rep(100, 4), rep(250, 4)),
                                    seed = 1,
                                    col.y = "y",
                                    col.x = grep("x.", names(pop_logistic_scenario2$data)),
                                    family = "binomial",
                                    nrun = 500)
save(res_logistic_scenario2, file="Simulations/res_logistic_scenario2.RData")



