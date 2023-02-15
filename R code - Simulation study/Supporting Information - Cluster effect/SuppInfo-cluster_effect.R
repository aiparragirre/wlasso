rm(list=ls())

# Packages ----------------------------------------------------------------

library(MASS)
library(survey)
library(glmnet)


# S1G

load("res_linear_scenario1.RData")

formula <- as.formula(paste0("y~",paste0("x.", 1:50, collapse = "+")))


## r = 100 

# ----------------------------------------------------------------------------
# ---> r = 100
# ----------------------------------------------------------------------------

sample <- res_linear_scenario1$sample_100$data

# SRS
# ----------------------------------------------------------------------------
S1G.unw <- lm(formula, data = sample)
v.S1G.unw <- vcov(S1G.unw)

#Trace 
tr.S1G.unw <- sum(diag(v.S1G.unw[-1,-1]))


# Weighted
# ----------------------------------------------------------------------------
des.wt <- svydesign(ids = ~1, strata = NULL, weights = ~weights, data = sample)
S1G.wt <- svyglm(formula, design = des.wt, family = gaussian())
v.S1G.wt <- vcov(S1G.wt)

#Trace 
tr.S1G.wt <- sum(diag(v.S1G.wt[-1,-1]))


# Cluster
# ----------------------------------------------------------------------------
des.clus <- svydesign(ids = ~cluster, strata = ~strata, weights = ~weights, data = sample, nest = TRUE)
S1G.clus <- svyglm(formula, design = des.clus, family = gaussian())
v.S1G.clus <- vcov(S1G.clus)

#Trace 
tr.S1G.clus <- sum(diag(v.S1G.clus[-1,-1]))


# Results
# ----------------------------------------------------------------------------
S1G.rt1 <- c(tr.S1G.unw, tr.S1G.wt, tr.S1G.wt/tr.S1G.unw)
names(S1G.rt1) <- c("unw", "wt", "ratio")
S1G.rt3 <- c(tr.S1G.unw, tr.S1G.clus, tr.S1G.clus/tr.S1G.unw)
names(S1G.rt3) <- c("unw", "clus", "ratio")
S1G.rt4 <- c(tr.S1G.wt, tr.S1G.clus, tr.S1G.clus/tr.S1G.wt)
names(S1G.rt4) <- c("wt", "clus", "ratio")

S1G.rt1 # wt/unw
S1G.rt3 # clus/unw
S1G.rt4 # clus/wt


## r = 200 

# ----------------------------------------------------------------------------
# ---> r = 200
# ----------------------------------------------------------------------------

sample <- res_linear_scenario1$sample_200$data

# SRS
# ----------------------------------------------------------------------------
S1G.unw <- lm(formula, data = sample)
v.S1G.unw <- vcov(S1G.unw)

#Trace 
tr.S1G.unw <- sum(diag(v.S1G.unw[-1,-1]))


# Weighted
# ----------------------------------------------------------------------------
des.wt <- svydesign(ids = ~1, strata = NULL, weights = ~weights, data = sample)
S1G.wt <- svyglm(formula, design = des.wt, family = gaussian())
v.S1G.wt <- vcov(S1G.wt)

#Trace 
tr.S1G.wt <- sum(diag(v.S1G.wt[-1,-1]))


# Cluster
# ----------------------------------------------------------------------------
des.clus <- svydesign(ids = ~cluster, strata = ~strata, weights = ~weights, data = sample, nest = TRUE)
S1G.clus <- svyglm(formula, design = des.clus, family = gaussian())
v.S1G.clus <- vcov(S1G.clus)

#Trace 
tr.S1G.clus <- sum(diag(v.S1G.clus[-1,-1]))


# Results
# ----------------------------------------------------------------------------
S1G.rt1 <- c(tr.S1G.unw, tr.S1G.wt, tr.S1G.wt/tr.S1G.unw)
names(S1G.rt1) <- c("unw", "wt", "ratio")
S1G.rt3 <- c(tr.S1G.unw, tr.S1G.clus, tr.S1G.clus/tr.S1G.unw)
names(S1G.rt3) <- c("unw", "clus", "ratio")
S1G.rt4 <- c(tr.S1G.wt, tr.S1G.clus, tr.S1G.clus/tr.S1G.wt)
names(S1G.rt4) <- c("wt", "clus", "ratio")

S1G.rt1 # wt/unw
S1G.rt3 # clus/unw
S1G.rt4 # clus/wt


## r = 300 

# ----------------------------------------------------------------------------
# ---> r = 300
# ----------------------------------------------------------------------------

sample <- res_linear_scenario1$sample_300$data

# SRS
# ----------------------------------------------------------------------------
S1G.unw <- lm(formula, data = sample)
v.S1G.unw <- vcov(S1G.unw)

#Trace 
tr.S1G.unw <- sum(diag(v.S1G.unw[-1,-1]))


# Weighted
# ----------------------------------------------------------------------------
des.wt <- svydesign(ids = ~1, strata = NULL, weights = ~weights, data = sample)
S1G.wt <- svyglm(formula, design = des.wt, family = gaussian())
v.S1G.wt <- vcov(S1G.wt)

#Trace 
tr.S1G.wt <- sum(diag(v.S1G.wt[-1,-1]))


# Cluster
# ----------------------------------------------------------------------------
des.clus <- svydesign(ids = ~cluster, strata = ~strata, weights = ~weights, data = sample, nest = TRUE)
S1G.clus <- svyglm(formula, design = des.clus, family = gaussian())
v.S1G.clus <- vcov(S1G.clus)

#Trace 
tr.S1G.clus <- sum(diag(v.S1G.clus[-1,-1]))


# Results
# ----------------------------------------------------------------------------
S1G.rt1 <- c(tr.S1G.unw, tr.S1G.wt, tr.S1G.wt/tr.S1G.unw)
names(S1G.rt1) <- c("unw", "wt", "ratio")
S1G.rt3 <- c(tr.S1G.unw, tr.S1G.clus, tr.S1G.clus/tr.S1G.unw)
names(S1G.rt3) <- c("unw", "clus", "ratio")
S1G.rt4 <- c(tr.S1G.wt, tr.S1G.clus, tr.S1G.clus/tr.S1G.wt)
names(S1G.rt4) <- c("wt", "clus", "ratio")

S1G.rt1 # wt/unw
S1G.rt3 # clus/unw
S1G.rt4 # clus/wt


## r = 400 

# ----------------------------------------------------------------------------
# ---> r = 400
# ----------------------------------------------------------------------------

sample <- res_linear_scenario1$sample_400$data

# SRS
# ----------------------------------------------------------------------------
S1G.unw <- lm(formula, data = sample)
v.S1G.unw <- vcov(S1G.unw)

#Trace 
tr.S1G.unw <- sum(diag(v.S1G.unw[-1,-1]))


# Weighted
# ----------------------------------------------------------------------------
des.wt <- svydesign(ids = ~1, strata = NULL, weights = ~weights, data = sample)
S1G.wt <- svyglm(formula, design = des.wt, family = gaussian())
v.S1G.wt <- vcov(S1G.wt)

#Trace 
tr.S1G.wt <- sum(diag(v.S1G.wt[-1,-1]))


# Cluster
# ----------------------------------------------------------------------------
des.clus <- svydesign(ids = ~cluster, strata = ~strata, weights = ~weights, data = sample, nest = TRUE)
S1G.clus <- svyglm(formula, design = des.clus, family = gaussian())
v.S1G.clus <- vcov(S1G.clus)

#Trace 
tr.S1G.clus <- sum(diag(v.S1G.clus[-1,-1]))


# Results
# ----------------------------------------------------------------------------
S1G.rt1 <- c(tr.S1G.unw, tr.S1G.wt, tr.S1G.wt/tr.S1G.unw)
names(S1G.rt1) <- c("unw", "wt", "ratio")
S1G.rt3 <- c(tr.S1G.unw, tr.S1G.clus, tr.S1G.clus/tr.S1G.unw)
names(S1G.rt3) <- c("unw", "clus", "ratio")
S1G.rt4 <- c(tr.S1G.wt, tr.S1G.clus, tr.S1G.clus/tr.S1G.wt)
names(S1G.rt4) <- c("wt", "clus", "ratio")
S1G.rt5 <- c(tr.S1G.str, tr.S1G.clus, tr.S1G.clus/tr.S1G.str)
names(S1G.rt5) <- c("wt", "clus", "ratio")

S1G.rt1 # wt/unw
S1G.rt3 # clus/unw
S1G.rt4 # clus/wt





# S2G

load("res_linear_scenario2.RData")

formula <- as.formula(paste0("y~",paste0("x.", 1:50, collapse = "+")))

## r = 100 

# ----------------------------------------------------------------------------
# ---> r = 100
# ----------------------------------------------------------------------------

sample <- res_linear_scenario2$sample_100$data

# SRS
# ----------------------------------------------------------------------------
S2G.unw <- lm(formula, data = sample)
v.S2G.unw <- vcov(S2G.unw)

#Trace 
tr.S2G.unw <- sum(diag(v.S2G.unw[-1,-1]))


# Weighted
# ----------------------------------------------------------------------------
des.wt <- svydesign(ids = ~1, strata = NULL, weights = ~weights, data = sample)
S2G.wt <- svyglm(formula, design = des.wt, family = gaussian())
v.S2G.wt <- vcov(S2G.wt)

#Trace 
tr.S2G.wt <- sum(diag(v.S2G.wt[-1,-1]))


# Cluster
# ----------------------------------------------------------------------------
des.clus <- svydesign(ids = ~cluster, strata = ~strata, weights = ~weights, data = sample, nest = TRUE)
S2G.clus <- svyglm(formula, design = des.clus, family = gaussian())
v.S2G.clus <- vcov(S2G.clus)

#Trace 
tr.S2G.clus <- sum(diag(v.S2G.clus[-1,-1]))


# Results
# ----------------------------------------------------------------------------
S2G.rt1 <- c(tr.S2G.unw, tr.S2G.wt, tr.S2G.wt/tr.S2G.unw)
names(S2G.rt1) <- c("unw", "wt", "ratio")
S2G.rt3 <- c(tr.S2G.unw, tr.S2G.clus, tr.S2G.clus/tr.S2G.unw)
names(S2G.rt3) <- c("unw", "clus", "ratio")
S2G.rt4 <- c(tr.S2G.wt, tr.S2G.clus, tr.S2G.clus/tr.S2G.wt)
names(S2G.rt4) <- c("wt", "clus", "ratio")

S2G.rt1 # wt/unw
S2G.rt3 # clus/unw
S2G.rt4 # clus/wt


## r = 200 

# ----------------------------------------------------------------------------
# ---> r = 200
# ----------------------------------------------------------------------------

sample <- res_linear_scenario2$sample_200$data

# SRS
# ----------------------------------------------------------------------------
S2G.unw <- lm(formula, data = sample)
v.S2G.unw <- vcov(S2G.unw)

#Trace 
tr.S2G.unw <- sum(diag(v.S2G.unw[-1,-1]))


# Weighted
# ----------------------------------------------------------------------------
des.wt <- svydesign(ids = ~1, strata = NULL, weights = ~weights, data = sample)
S2G.wt <- svyglm(formula, design = des.wt, family = gaussian())
v.S2G.wt <- vcov(S2G.wt)

#Trace 
tr.S2G.wt <- sum(diag(v.S2G.wt[-1,-1]))


# Cluster
# ----------------------------------------------------------------------------
des.clus <- svydesign(ids = ~cluster, strata = ~strata, weights = ~weights, data = sample, nest = TRUE)
S2G.clus <- svyglm(formula, design = des.clus, family = gaussian())
v.S2G.clus <- vcov(S2G.clus)

#Trace 
tr.S2G.clus <- sum(diag(v.S2G.clus[-1,-1]))


# Results
# ----------------------------------------------------------------------------
S2G.rt1 <- c(tr.S2G.unw, tr.S2G.wt, tr.S2G.wt/tr.S2G.unw)
names(S2G.rt1) <- c("unw", "wt", "ratio")
S2G.rt3 <- c(tr.S2G.unw, tr.S2G.clus, tr.S2G.clus/tr.S2G.unw)
names(S2G.rt3) <- c("unw", "clus", "ratio")
S2G.rt4 <- c(tr.S2G.wt, tr.S2G.clus, tr.S2G.clus/tr.S2G.wt)
names(S2G.rt4) <- c("wt", "clus", "ratio")

S2G.rt1 # wt/unw
S2G.rt3 # clus/unw
S2G.rt4 # clus/wt


## r = 300 

# ----------------------------------------------------------------------------
# ---> r = 300
# ----------------------------------------------------------------------------

sample <- res_linear_scenario2$sample_300$data

# SRS
# ----------------------------------------------------------------------------
S2G.unw <- lm(formula, data = sample)
v.S2G.unw <- vcov(S2G.unw)

#Trace 
tr.S2G.unw <- sum(diag(v.S2G.unw[-1,-1]))


# Weighted
# ----------------------------------------------------------------------------
des.wt <- svydesign(ids = ~1, strata = NULL, weights = ~weights, data = sample)
S2G.wt <- svyglm(formula, design = des.wt, family = gaussian())
v.S2G.wt <- vcov(S2G.wt)

#Trace 
tr.S2G.wt <- sum(diag(v.S2G.wt[-1,-1]))


# Cluster
# ----------------------------------------------------------------------------
des.clus <- svydesign(ids = ~cluster, strata = ~strata, weights = ~weights, data = sample, nest = TRUE)
S2G.clus <- svyglm(formula, design = des.clus, family = gaussian())
v.S2G.clus <- vcov(S2G.clus)

#Trace 
tr.S2G.clus <- sum(diag(v.S2G.clus[-1,-1]))


# Results
# ----------------------------------------------------------------------------
S2G.rt1 <- c(tr.S2G.unw, tr.S2G.wt, tr.S2G.wt/tr.S2G.unw)
names(S2G.rt1) <- c("unw", "wt", "ratio")
S2G.rt3 <- c(tr.S2G.unw, tr.S2G.clus, tr.S2G.clus/tr.S2G.unw)
names(S2G.rt3) <- c("unw", "clus", "ratio")
S2G.rt4 <- c(tr.S2G.wt, tr.S2G.clus, tr.S2G.clus/tr.S2G.wt)
names(S2G.rt4) <- c("wt", "clus", "ratio")

S2G.rt1 # wt/unw
S2G.rt3 # clus/unw
S2G.rt4 # clus/wt


## r = 400 

# ----------------------------------------------------------------------------
# ---> r = 400
# ----------------------------------------------------------------------------

sample <- res_linear_scenario2$sample_400$data

# SRS
# ----------------------------------------------------------------------------
S2G.unw <- lm(formula, data = sample)
v.S2G.unw <- vcov(S2G.unw)

#Trace 
tr.S2G.unw <- sum(diag(v.S2G.unw[-1,-1]))


# Weighted
# ----------------------------------------------------------------------------
des.wt <- svydesign(ids = ~1, strata = NULL, weights = ~weights, data = sample)
S2G.wt <- svyglm(formula, design = des.wt, family = gaussian())
v.S2G.wt <- vcov(S2G.wt)

#Trace 
tr.S2G.wt <- sum(diag(v.S2G.wt[-1,-1]))


# Cluster
# ----------------------------------------------------------------------------
des.clus <- svydesign(ids = ~cluster, strata = ~strata, weights = ~weights, data = sample, nest = TRUE)
S2G.clus <- svyglm(formula, design = des.clus, family = gaussian())
v.S2G.clus <- vcov(S2G.clus)

#Trace 
tr.S2G.clus <- sum(diag(v.S2G.clus[-1,-1]))


# Results
# ----------------------------------------------------------------------------
S2G.rt1 <- c(tr.S2G.unw, tr.S2G.wt, tr.S2G.wt/tr.S2G.unw)
names(S2G.rt1) <- c("unw", "wt", "ratio")
S2G.rt3 <- c(tr.S2G.unw, tr.S2G.clus, tr.S2G.clus/tr.S2G.unw)
names(S2G.rt3) <- c("unw", "clus", "ratio")
S2G.rt4 <- c(tr.S2G.wt, tr.S2G.clus, tr.S2G.clus/tr.S2G.wt)
names(S2G.rt4) <- c("wt", "clus", "ratio")

S2G.rt1 # wt/unw
S2G.rt3 # clus/unw
S2G.rt4 # clus/wt




# S1B

load("res_logistic_scenario1.RData")

formula <- as.formula(paste0("y~",paste0("x.", 1:50, collapse = "+")))

## r = 100 

# ----------------------------------------------------------------------------
# ---> r = 100
# ----------------------------------------------------------------------------

sample <- res_logistic_scenario1$sample_100$data

# SRS
# ----------------------------------------------------------------------------
S1B.unw <- glm(formula, data = sample, family = binomial())
v.S1B.unw <- vcov(S1B.unw)

#Trace 
tr.S1B.unw <- sum(diag(v.S1B.unw[-1,-1]))


# Weighted
# ----------------------------------------------------------------------------
des.wt <- svydesign(ids = ~1, strata = NULL, weights = ~weights, data = sample)
S1B.wt <- svyglm(formula, design = des.wt, family = quasibinomial())
v.S1B.wt <- vcov(S1B.wt)

#Trace 
tr.S1B.wt <- sum(diag(v.S1B.wt[-1,-1]))


# Cluster
# ----------------------------------------------------------------------------
des.clus <- svydesign(ids = ~cluster, strata = ~strata, weights = ~weights, data = sample, nest = TRUE)
S1B.clus <- svyglm(formula, design = des.clus, family = quasibinomial())
v.S1B.clus <- vcov(S1B.clus)

#Trace 
tr.S1B.clus <- sum(diag(v.S1B.clus[-1,-1]))


# Results
# ----------------------------------------------------------------------------
S1B.rt1 <- c(tr.S1B.unw, tr.S1B.wt, tr.S1B.wt/tr.S1B.unw)
names(S1B.rt1) <- c("unw", "wt", "ratio")
S1B.rt3 <- c(tr.S1B.unw, tr.S1B.clus, tr.S1B.clus/tr.S1B.unw)
names(S1B.rt3) <- c("unw", "clus", "ratio")
S1B.rt4 <- c(tr.S1B.wt, tr.S1B.clus, tr.S1B.clus/tr.S1B.wt)
names(S1B.rt4) <- c("wt", "clus", "ratio")

S1B.rt1 # wt/unw
S1B.rt3 # clus/unw
S1B.rt4 # clus/wt


## r = 200 

# ----------------------------------------------------------------------------
# ---> r = 200
# ----------------------------------------------------------------------------

sample <- res_logistic_scenario1$sample_200$data

# SRS
# ----------------------------------------------------------------------------
S1B.unw <- glm(formula, data = sample, family = binomial())
v.S1B.unw <- vcov(S1B.unw)

#Trace 
tr.S1B.unw <- sum(diag(v.S1B.unw[-1,-1]))


# Weighted
# ----------------------------------------------------------------------------
des.wt <- svydesign(ids = ~1, strata = NULL, weights = ~weights, data = sample)
S1B.wt <- svyglm(formula, design = des.wt, family = quasibinomial())
v.S1B.wt <- vcov(S1B.wt)

#Trace 
tr.S1B.wt <- sum(diag(v.S1B.wt[-1,-1]))


# Cluster
# ----------------------------------------------------------------------------
des.clus <- svydesign(ids = ~cluster, strata = ~strata, weights = ~weights, data = sample, nest = TRUE)
S1B.clus <- svyglm(formula, design = des.clus, family = quasibinomial())
v.S1B.clus <- vcov(S1B.clus)

#Trace 
tr.S1B.clus <- sum(diag(v.S1B.clus[-1,-1]))


# Results
# ----------------------------------------------------------------------------
S1B.rt1 <- c(tr.S1B.unw, tr.S1B.wt, tr.S1B.wt/tr.S1B.unw)
names(S1B.rt1) <- c("unw", "wt", "ratio")
S1B.rt3 <- c(tr.S1B.unw, tr.S1B.clus, tr.S1B.clus/tr.S1B.unw)
names(S1B.rt3) <- c("unw", "clus", "ratio")
S1B.rt4 <- c(tr.S1B.wt, tr.S1B.clus, tr.S1B.clus/tr.S1B.wt)
names(S1B.rt4) <- c("wt", "clus", "ratio")

S1B.rt1 # wt/unw
S1B.rt3 # clus/unw
S1B.rt4 # clus/wt


## r = 300 

# ----------------------------------------------------------------------------
# ---> r = 100
# ----------------------------------------------------------------------------

sample <- res_logistic_scenario1$sample_300$data

# SRS
# ----------------------------------------------------------------------------
S1B.unw <- glm(formula, data = sample, family = binomial())
v.S1B.unw <- vcov(S1B.unw)

#Trace 
tr.S1B.unw <- sum(diag(v.S1B.unw[-1,-1]))


# Weighted
# ----------------------------------------------------------------------------
des.wt <- svydesign(ids = ~1, strata = NULL, weights = ~weights, data = sample)
S1B.wt <- svyglm(formula, design = des.wt, family = quasibinomial())
v.S1B.wt <- vcov(S1B.wt)

#Trace 
tr.S1B.wt <- sum(diag(v.S1B.wt[-1,-1]))


# Cluster
# ----------------------------------------------------------------------------
des.clus <- svydesign(ids = ~cluster, strata = ~strata, weights = ~weights, data = sample, nest = TRUE)
S1B.clus <- svyglm(formula, design = des.clus, family = quasibinomial())
v.S1B.clus <- vcov(S1B.clus)

#Trace 
tr.S1B.clus <- sum(diag(v.S1B.clus[-1,-1]))


# Results
# ----------------------------------------------------------------------------
S1B.rt1 <- c(tr.S1B.unw, tr.S1B.wt, tr.S1B.wt/tr.S1B.unw)
names(S1B.rt1) <- c("unw", "wt", "ratio")
S1B.rt3 <- c(tr.S1B.unw, tr.S1B.clus, tr.S1B.clus/tr.S1B.unw)
names(S1B.rt3) <- c("unw", "clus", "ratio")
S1B.rt4 <- c(tr.S1B.wt, tr.S1B.clus, tr.S1B.clus/tr.S1B.wt)
names(S1B.rt4) <- c("wt", "clus", "ratio")

S1B.rt1 # wt/unw
S1B.rt3 # clus/unw
S1B.rt4 # clus/wt


## r = 400 

# ----------------------------------------------------------------------------
# ---> r = 400
# ----------------------------------------------------------------------------

sample <- res_logistic_scenario1$sample_400$data

# SRS
# ----------------------------------------------------------------------------
S1B.unw <- glm(formula, data = sample, family = binomial())
v.S1B.unw <- vcov(S1B.unw)

#Trace 
tr.S1B.unw <- sum(diag(v.S1B.unw[-1,-1]))


# Weighted
# ----------------------------------------------------------------------------
des.wt <- svydesign(ids = ~1, strata = NULL, weights = ~weights, data = sample)
S1B.wt <- svyglm(formula, design = des.wt, family = quasibinomial())
v.S1B.wt <- vcov(S1B.wt)

#Trace 
tr.S1B.wt <- sum(diag(v.S1B.wt[-1,-1]))


# Cluster
# ----------------------------------------------------------------------------
des.clus <- svydesign(ids = ~cluster, strata = ~strata, weights = ~weights, data = sample, nest = TRUE)
S1B.clus <- svyglm(formula, design = des.clus, family = quasibinomial())
v.S1B.clus <- vcov(S1B.clus)

#Trace 
tr.S1B.clus <- sum(diag(v.S1B.clus[-1,-1]))


# Results
# ----------------------------------------------------------------------------
S1B.rt1 <- c(tr.S1B.unw, tr.S1B.wt, tr.S1B.wt/tr.S1B.unw)
names(S1B.rt1) <- c("unw", "wt", "ratio")
S1B.rt3 <- c(tr.S1B.unw, tr.S1B.clus, tr.S1B.clus/tr.S1B.unw)
names(S1B.rt3) <- c("unw", "clus", "ratio")
S1B.rt4 <- c(tr.S1B.wt, tr.S1B.clus, tr.S1B.clus/tr.S1B.wt)
names(S1B.rt4) <- c("wt", "clus", "ratio")

S1B.rt1 # wt/unw
S1B.rt3 # clus/unw
S1B.rt4 # clus/wt





# S2B

load("res_logistic_scenario2.RData")

formula <- as.formula(paste0("y~",paste0("x.", 1:50, collapse = "+")))

## r = 100 

# ----------------------------------------------------------------------------
# ---> r = 100
# ----------------------------------------------------------------------------

sample <- res_logistic_scenario2$sample_100$data

# SRS
# ----------------------------------------------------------------------------
S2B.unw <- glm(formula, data = sample, family = binomial())
v.S2B.unw <- vcov(S2B.unw)

#Trace 
tr.S2B.unw <- sum(diag(v.S2B.unw[-1,-1]))


# Weighted
# ----------------------------------------------------------------------------
des.wt <- svydesign(ids = ~1, strata = NULL, weights = ~weights, data = sample)
S2B.wt <- svyglm(formula, design = des.wt, family = quasibinomial())
v.S2B.wt <- vcov(S2B.wt)

#Trace 
tr.S2B.wt <- sum(diag(v.S2B.wt[-1,-1]))


# Cluster
# ----------------------------------------------------------------------------
des.clus <- svydesign(ids = ~cluster, strata = ~strata, weights = ~weights, data = sample, nest = TRUE)
S2B.clus <- svyglm(formula, design = des.clus, family = quasibinomial())
v.S2B.clus <- vcov(S2B.clus)

#Trace 
tr.S2B.clus <- sum(diag(v.S2B.clus[-1,-1]))


# Results
# ----------------------------------------------------------------------------
S2B.rt1 <- c(tr.S2B.unw, tr.S2B.wt, tr.S2B.wt/tr.S2B.unw)
names(S2B.rt1) <- c("unw", "wt", "ratio")
S2B.rt3 <- c(tr.S2B.unw, tr.S2B.clus, tr.S2B.clus/tr.S2B.unw)
names(S2B.rt3) <- c("unw", "clus", "ratio")
S2B.rt4 <- c(tr.S2B.wt, tr.S2B.clus, tr.S2B.clus/tr.S2B.wt)
names(S2B.rt4) <- c("wt", "clus", "ratio")

S2B.rt1 # wt/unw
S2B.rt3 # clus/unw
S2B.rt4 # clus/wt


## r = 200 

# ----------------------------------------------------------------------------
# ---> r = 200
# ----------------------------------------------------------------------------

sample <- res_logistic_scenario2$sample_200$data

# SRS
# ----------------------------------------------------------------------------
S2B.unw <- glm(formula, data = sample, family = binomial())
v.S2B.unw <- vcov(S2B.unw)

#Trace 
tr.S2B.unw <- sum(diag(v.S2B.unw[-1,-1]))


# Weighted
# ----------------------------------------------------------------------------
des.wt <- svydesign(ids = ~1, strata = NULL, weights = ~weights, data = sample)
S2B.wt <- svyglm(formula, design = des.wt, family = quasibinomial())
v.S2B.wt <- vcov(S2B.wt)

#Trace 
tr.S2B.wt <- sum(diag(v.S2B.wt[-1,-1]))


# Cluster
# ----------------------------------------------------------------------------
des.clus <- svydesign(ids = ~cluster, strata = ~strata, weights = ~weights, data = sample, nest = TRUE)
S2B.clus <- svyglm(formula, design = des.clus, family = quasibinomial())
v.S2B.clus <- vcov(S2B.clus)

#Trace 
tr.S2B.clus <- sum(diag(v.S2B.clus[-1,-1]))


# Results
# ----------------------------------------------------------------------------
S2B.rt1 <- c(tr.S2B.unw, tr.S2B.wt, tr.S2B.wt/tr.S2B.unw)
names(S2B.rt1) <- c("unw", "wt", "ratio")
S2B.rt3 <- c(tr.S2B.unw, tr.S2B.clus, tr.S2B.clus/tr.S2B.unw)
names(S2B.rt3) <- c("unw", "clus", "ratio")
S2B.rt4 <- c(tr.S2B.wt, tr.S2B.clus, tr.S2B.clus/tr.S2B.wt)
names(S2B.rt4) <- c("wt", "clus", "ratio")

S2B.rt1 # wt/unw
S2B.rt3 # clus/unw
S2B.rt4 # clus/wt


## r = 300 

# ----------------------------------------------------------------------------
# ---> r = 100
# ----------------------------------------------------------------------------

sample <- res_logistic_scenario2$sample_300$data

# SRS
# ----------------------------------------------------------------------------
S2B.unw <- glm(formula, data = sample, family = binomial())
v.S2B.unw <- vcov(S2B.unw)

#Trace 
tr.S2B.unw <- sum(diag(v.S2B.unw[-1,-1]))


# Weighted
# ----------------------------------------------------------------------------
des.wt <- svydesign(ids = ~1, strata = NULL, weights = ~weights, data = sample)
S2B.wt <- svyglm(formula, design = des.wt, family = quasibinomial())
v.S2B.wt <- vcov(S2B.wt)

#Trace 
tr.S2B.wt <- sum(diag(v.S2B.wt[-1,-1]))


# Cluster
# ----------------------------------------------------------------------------
des.clus <- svydesign(ids = ~cluster, strata = ~strata, weights = ~weights, data = sample, nest = TRUE)
S2B.clus <- svyglm(formula, design = des.clus, family = quasibinomial())
v.S2B.clus <- vcov(S2B.clus)

#Trace 
tr.S2B.clus <- sum(diag(v.S2B.clus[-1,-1]))


# Results
# ----------------------------------------------------------------------------
S2B.rt1 <- c(tr.S2B.unw, tr.S2B.wt, tr.S2B.wt/tr.S2B.unw)
names(S2B.rt1) <- c("unw", "wt", "ratio")
S2B.rt3 <- c(tr.S2B.unw, tr.S2B.clus, tr.S2B.clus/tr.S2B.unw)
names(S2B.rt3) <- c("unw", "clus", "ratio")
S2B.rt4 <- c(tr.S2B.wt, tr.S2B.clus, tr.S2B.clus/tr.S2B.wt)
names(S2B.rt4) <- c("wt", "clus", "ratio")

S2B.rt1 # wt/unw
S2B.rt3 # clus/unw
S2B.rt4 # clus/wt


## r = 400 

# ----------------------------------------------------------------------------
# ---> r = 400
# ----------------------------------------------------------------------------

sample <- res_logistic_scenario2$sample_400$data

# SRS
# ----------------------------------------------------------------------------
S2B.unw <- glm(formula, data = sample, family = binomial())
v.S2B.unw <- vcov(S2B.unw)

#Trace 
tr.S2B.unw <- sum(diag(v.S2B.unw[-1,-1]))


# Weighted
# ----------------------------------------------------------------------------
des.wt <- svydesign(ids = ~1, strata = NULL, weights = ~weights, data = sample)
S2B.wt <- svyglm(formula, design = des.wt, family = quasibinomial())
v.S2B.wt <- vcov(S2B.wt)

#Trace 
tr.S2B.wt <- sum(diag(v.S2B.wt[-1,-1]))


# Cluster
# ----------------------------------------------------------------------------
des.clus <- svydesign(ids = ~cluster, strata = ~strata, weights = ~weights, data = sample, nest = TRUE)
S2B.clus <- svyglm(formula, design = des.clus, family = quasibinomial())
v.S2B.clus <- vcov(S2B.clus)

#Trace 
tr.S2B.clus <- sum(diag(v.S2B.clus[-1,-1]))


# Results
# ----------------------------------------------------------------------------
S2B.rt1 <- c(tr.S2B.unw, tr.S2B.wt, tr.S2B.wt/tr.S2B.unw)
names(S2B.rt1) <- c("unw", "wt", "ratio")
S2B.rt3 <- c(tr.S2B.unw, tr.S2B.clus, tr.S2B.clus/tr.S2B.unw)
names(S2B.rt3) <- c("unw", "clus", "ratio")
S2B.rt4 <- c(tr.S2B.wt, tr.S2B.clus, tr.S2B.clus/tr.S2B.wt)
names(S2B.rt4) <- c("wt", "clus", "ratio")

S2B.rt1 # wt/unw
S2B.rt3 # clus/unw
S2B.rt4 # clus/wt

