
# Example -----------------------------------------------------------------

# Linear regression

load("example-data/sample_G.RData")

# --> Design-based cross-validation (dCV): k=10 folds and R=1 replicates
mcv <- wlasso(data = sample_G, col.y = "y", col.x = 1:50, family = "gaussian", 
              cluster = "cluster", strata = "strata", weights = "weights",
              method = "dCV", k=10, R=1, seed = 100)
plot(mcv)
coef(mcv$model.min)

# --> Bootstrap (ah-1 PSUs, "subbootstrap")
mboot <- wlasso(data = sample_G, col.y = "y", col.x = 1:50, family = "gaussian", 
                cluster = "cluster", strata = "strata", weights = "weights",
                method = "subbootstrap", B=200, seed = 100)
plot(mboot)
coef(mboot$model.min)

# --> Jacknife Repeated Replication (JKn)
mjkn <- wlasso(data = sample_G, col.y = "y", col.x = 1:50, family = "gaussian", 
               cluster = "cluster", strata = "strata", weights = "weights",
               method = "JKn", seed = 100)
plot(mjkn)
coef(mjkn$model.min)

# --> Balanced Repeated Replication (BRR)
mbrr <- wlasso(data = sample_G, col.y = "y", col.x = 1:50, family = "gaussian", 
               cluster = "cluster", strata = "strata", weights = "weights",
               method = "BRR", seed = 100)
plot(mbrr)
coef(mbrr$model.min)

# --> Split-sample Repeated Replication with cross-validated replicate weights (R=20)
msplit <- wlasso(data = sample_G, col.y = "y", col.x = 1:50, family = "gaussian", 
                 cluster = "cluster", strata = "strata", weights = "weights",
                 method = "split", train.prob = 0.7, method.split = "cv", R=20,
                 seed = 100)
plot(msplit)
coef(mbrr$model.min)

# --> Split-sample Repeated Replication with bootstrap replicate weights (R=20)
msplit <- wlasso(data = sample_G, col.y = "y", col.x = 1:50, family = "gaussian", 
                 cluster = "cluster", strata = "strata", weights = "weights",
                 method = "split", train.prob = 0.7, method.split = "bootstrap", R=20,
                 seed = 100)
plot(msplit)
coef(msplit$model.min)

# --> Extrapolation (R=20)
mextra <- wlasso(data = sample_G, col.y = "y", col.x = 1:50, family = "gaussian", 
                 cluster = "cluster", strata = "strata", weights = "weights",
                 method = "extrapolation", train.prob = 0.7, R=20,
                 seed = 100)
plot(mextra)
coef(mextra$model.min)


# Logistic regression

load("example-data/sample_B.RData")

mcv <- wlasso(data = sample_B, col.y = "y", col.x = 1:50, family = "binomial", 
              cluster = "cluster", strata = "strata", weights = "weights",
              method = "dCV", k=10, R=1, seed = 100)
plot(mcv)
coef(mcv$model.min)

mboot <- wlasso(data = sample_B, col.y = "y", col.x = 1:50, family = "binomial", 
                cluster = "cluster", strata = "strata", weights = "weights",
                method = "bootstrap", B=200, seed = 100)
plot(mboot)
coef(mboot$model.min)

mjkn <- wlasso(data = sample_B, col.y = "y", col.x = 1:50, family = "binomial", 
               cluster = "cluster", strata = "strata", weights = "weights",
               method = "JKn", seed = 100)
plot(mjkn)
coef(mjkn$model.min)

mbrr <- wlasso(data = sample_B, col.y = "y", col.x = 1:50, family = "binomial", 
               cluster = "cluster", strata = "strata", weights = "weights",
               method = "BRR", seed = 100)
plot(mbrr)
coef(mbrr$model.min)

msplit <- wlasso(data = sample_B, col.y = "y", col.x = 1:50, family = "binomial", 
                 cluster = "cluster", strata = "strata", weights = "weights",
                 method = "split", train.prob = 0.7, method.split = "cv", R=20,
                 seed = 100)
plot(msplit)
coef(mbrr$model.min)

msplit <- wlasso(data = sample_B, col.y = "y", col.x = 1:50, family = "binomial", 
                 cluster = "cluster", strata = "strata", weights = "weights",
                 method = "split", train.prob = 0.7, method.split = "bootstrap", R=20,
                 seed = 100)
plot(msplit)
coef(msplit$model.min)

mextra <- wlasso(data = sample_B, col.y = "y", col.x = 1:50, family = "binomial", 
                 cluster = "cluster", strata = "strata", weights = "weights",
                 method = "extrapolation", train.prob = 0.7, R=20,
                 seed = 100)
plot(mextra)
coef(mextra$model.min)

