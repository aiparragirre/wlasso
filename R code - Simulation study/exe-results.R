rm(list=ls())
setwd("/Users/amaiaiparragirre/SynologyDrive/Lana/Tesia/2022-07 Estantzia NZ/Ikerkuntza/Programazioa/GitHub/R code - Simulation study")

# Packages ----------------------------------------------------------------

library(MASS)
library(survey)
library(glmnet)
library(ggplot2)

# Functions ---------------------------------------------------------------

sapply(paste0("Functions/Results-Graphics/",list.files("Functions/Results-Graphics", full.names=F, pattern='\\.[rR]$')), source)
source("Functions/Simulation study/Initialize/matrix.methods.R")

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


# Results -----------------------------------------------------------------

# Indicate the name of the file related to the scenario and run the rest:
# Select between: res_linear_scenario1, res_linear_scenario2, res_logistic_scenario1, res_logistic_scenario2
obj.name <- "res_linear_scenario1" 
load(paste0("Results/",obj.name,".RData"))

# Run all:
method.run <- c(1, 9, 14, 15, 20:21, 24, 39:40)

if(obj.name %in% c("res_linear_scenario1", "res_linear_scenario2")){
  down.y <- 100
  up.y <- 800
  x.legend <- -3.5
  y.legend <- up.y
  if(obj.name == "res_linear_scenario1"){
    scenario <- "S1(G)"
  } else {
    scenario <- "S2(G)"
  }
} else {
  down.y <- 0.3
  up.y <- 1.3
  x.legend <- -6
  y.legend <- up.y
  if(obj.name == "res_logistic_scenario1"){
    scenario <- "S1(B)"
  } else {
    scenario <- "S2(B)"
  }
}


# Log-lambda bias:
f.plot.bias(res = res.transform(get(obj.name), method.ind = method.run), 
            obj.name = obj.name, ylim = c(-5.5, 6), opacity = 0.25,
            l.width = 8, l.height = 5, scenario = scenario,
            method.ind = method.run, 
            method.names = c("JKn", "dCV", "Bootstrap", "BRR", "Split-cv", 
                             "Split-boot", "Extrap.","SRSCV", "Unw"),
            colours = viridis::viridis(length(method.run)))

# Number of variables in the selected models:
f.plot.nvar(res = res.transform(get(obj.name), method.ind = method.run), 
            obj.name = obj.name, ylim = NULL, opacity = 0.25,
            l.width = 8, l.height = 5, scenario = scenario,
            method.ind = method.run, 
            method.names = c("JKn", "dCV", "Bootstrap", "BRR", "Split-cv", 
                             "Split-boot", "Extrap.","SRSCV", "Unw"),
            colours = c("lightgray",viridis::viridis(length(method.run))))


# Training models' error (supporting information, not shown in the paper):
f.plot.train.error(obj.name = obj.name, runs = c(100, 200, 300, 400), 
                   ylim = c(down.y, up.y),
                   l.width = 15, l.height = 8,
                   methods = c(1:length(method.run)), 
                   method.names = c("JKn", "dCV", "Bootstrap", "BRR", "Split R=20 (cv)", 
                                    "Split R=20 (boot)", "Extrap. R=20","SRSCV","Unw"),
                   cols = rainbow(length(method.run)),
                   x.legend = x.legend, y.legend = y.legend)


# Numberical results
f.table(res = res.transform(get(obj.name), method.ind = method.run), 
        obj.name = obj.name, 
        method.ind = method.run, 
        method.names = c("JKn", "dCV", "Bootstrap", "BRR", "Split R=20 (cv)", 
                         "Split R=20 (boot)", "Extrap. R=20","SRSCV (K=10)","Unw (K=10)"))
