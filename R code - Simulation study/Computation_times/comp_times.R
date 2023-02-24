
#library("devtools")
#install_github("aiparragirre/wlasso/wlasso")
library(wlasso)

load("../Simulations/res_logistic_scenario1.RData")

res <- res_logistic_scenario1
family <- "binomial"

m.times <- matrix(nrow = 3, ncol = 500)
rownames(m.times) <- c("JKn", "dCV", "Bootstrap")

for(r in 1:500){
  
  print(r)
  
  sample <- res[[paste0("sample_",r)]][["data"]]
  
  # JKn
  start.r.jkn <- Sys.time()
  mjkn <- wlasso(data = sample, col.y = "y", col.x = 1:50, family = family, 
                 cluster = "cluster", strata = "strata", weights = "weights",
                 method = "JKn", seed = r)
  end.r.jkn <- Sys.time()
  m.times[1,r] <- difftime(end.r.jkn, start.r.jkn, units = "secs")
  
  # dCV (k=10, R=1)
  start.r.dCV <- Sys.time()
  mjkn <- wlasso(data = sample, col.y = "y", col.x = 1:50, family = family, 
                 cluster = "cluster", strata = "strata", weights = "weights",
                 method = "dCV", k=10, R=1, seed = r)
  end.r.dCV <- Sys.time()
  m.times[2,r] <- difftime(end.r.dCV, start.r.dCV, units = "secs")
  
  # Bootstrap
  start.r.boot <- Sys.time()
  mboot <- wlasso(data = sample, col.y = "y", col.x = 1:50, family = family, 
                  cluster = "cluster", strata = "strata", weights = "weights",
                  method = "subbootstrap", B=200, seed = r)
  end.r.boot <- Sys.time()
  m.times[3,r] <- difftime(end.r.boot, start.r.boot, units = "secs")
  
  
}
save(m.times, file = "times_res_logistic_scenario1.RData")

apply(m.times, 1, mean)


