## ---- include=FALSE------------------------------------------------------
# overall knitr options
knitr::opts_chunk$set(cache = FALSE, echo = TRUE, warning = FALSE, message = FALSE)

## ----gbm_package---------------------------------------------------------
# remove.packages('gbm')
# install.packages("gbm")
library(gbm)

## ----set_seed------------------------------------------------------------
set.seed(1)

## ----set_n---------------------------------------------------------------
n <- 1000 # number of observations

## ----generate_covariates-------------------------------------------------
Gender <- factor(sample(c("m", "f"), n, replace=TRUE))
Age <- sample(c(18:65), n, replace=TRUE)
Split <- factor(sample(c("yes", "no"), n, replace = TRUE))
Sport <- factor(sample(c("yes", "no"), n, replace = TRUE))

## ----specify_lambda------------------------------------------------------
lambda <- 0.1*ifelse(Gender == "m", 1.5, 1)
lambda <- lambda*ifelse(Age >= 18 & Age < 30, 3, 1)
lambda <- lambda*ifelse(Age >= 30 & Age < 45, 2, 1)
lambda <- lambda*ifelse(Sport == "yes", 2.5, 1)

## ----generate_poisson_data-----------------------------------------------
N <- rpois(n, lambda)
data <- data.frame(N, Gender, Age, Sport, Split)

## ----inspect_data--------------------------------------------------------
head(data, n = 10)

## ----gbm_fit_sim---------------------------------------------------------
set.seed(3)
gbm_sim <- gbm(N ~ Gender + Age + Split + Sport, # formula
               data = data, # data set
               var.monotone = c(0,0,0,0), # -1: monotone decrease, +1: monotone   increase, 0: no monotone restrictions
               distribution = "poisson", # see the help for other choices
               n.trees = 250, # number of trees
               shrinkage = 0.05, # shrinkage or learning rate
               interaction.depth = 3, # 1: additive model, 2: two-way interactions, etc.
               bag.fraction = 0.5, # subsampling fraction
               train.fraction = 1, # fraction of data for training
               n.minobsinnode = 10, # minimum total weight needed in each node
               cv.folds = 3, # do 3-fold cross-validation
               keep.data = TRUE, # keep a copy of the dataset with the object
               verbose = FALSE, # don’t print out progress
               n.cores = 1) # use only a single core

## ----print_gbm-----------------------------------------------------------
print(gbm_sim)

## ----var_imp-------------------------------------------------------------
summary(gbm_sim)

## ----var_imp_sum_100-----------------------------------------------------
sum(summary(gbm_sim)$rel.inf)

## ----initF---------------------------------------------------------------
gbm_sim$initF
exp(gbm_sim$initF)
sum(data$N)/n

## ------------------------------------------------------------------------
gbm_sim$train.error[250]
-2*mean(data$N*(gbm_sim$fit+log(1))-exp(gbm_sim$fit+log(1)))
plot(gbm_sim$train.error, type = "l", xlab = "Number of boosted trees")

## ----inspect_trees_gbm---------------------------------------------------
pretty.gbm.tree(gbm_sim, i.tree = 1)
pretty.gbm.tree(gbm_sim, i.tree = 2)
pretty.gbm.tree(gbm_sim, i.tree = 250)

## ----opt_boost_it1-------------------------------------------------------
best.iter.oob <- gbm.perf(gbm_sim, method = "OOB")
print(best.iter.oob)

## ----gbm_fit_train_fraction----------------------------------------------
gbm_sim <- gbm(N ~ Gender + Age + Split + Sport, # formula
               data = data, # data set
               var.monotone = c(0,0,0,0), # -1: monotone decrease, +1: monotone   increase, 0: no monotone restrictions
               distribution = "poisson", # see the help for other choices
               n.trees = 250, # number of trees
               shrinkage = 0.05, # shrinkage or learning rate
               interaction.depth = 3, # 1: additive model, 2: two-way interactions, etc.
               bag.fraction = 0.5, # subsampling fraction
               train.fraction = 0.5, # fraction of data for training
               n.minobsinnode = 10, # minimum total weight needed in each node
               cv.folds = 3, # do 3-fold cross-validation
               keep.data = TRUE, # keep a copy of the dataset with the object
               verbose = FALSE, # don’t print out progress
               n.cores = 1) # use only a single core

## ----opt_boost_it2-------------------------------------------------------
best.iter.test <- gbm.perf(gbm_sim, method = "test")
print(best.iter.test)

## ----opt_boost_it3-------------------------------------------------------
best.iter.cv <- gbm.perf(gbm_sim, method = "cv")
print(best.iter.cv)

## ----var_imp_opt---------------------------------------------------------
summary(gbm_sim, n.trees = best.iter.cv)

## ----pdp_sim-------------------------------------------------------------
par(mfrow = c(2,2))
plot(gbm_sim, 1, best.iter.cv, ylim=c(-1.5,-0.5))
plot(gbm_sim, 2, best.iter.cv, ylim=c(-1.5,-0.5))
plot(gbm_sim, 3, best.iter.cv, ylim=c(-1.5,-0.5))
plot(gbm_sim, 4, best.iter.cv, ylim=c(-1.5,-0.5))

## ------------------------------------------------------------------------
par(mfrow = c(1,1))
plot(gbm_sim, c(1,2,4), best.iter.cv)

## ------------------------------------------------------------------------
#install.packages("pdp")
library(pdp)
pd <- partial(gbm_sim, n.trees = best.iter.cv, pred.var = c("Age"))
age_pdp <- plotPartial(pd)

## ------------------------------------------------------------------------
set.seed(8711)
age_ice <- partial(gbm_sim, n.trees = best.iter.cv, pred.var = "Age", ice = TRUE)
plotPartial(age_ice[age_ice$yhat.id%in%sample(unique(age_ice$yhat.id), 100),], plot.pdp = FALSE, alpha = 0.2)
sport_ice <- partial(gbm_sim, n.trees = best.iter.cv, pred.var = "Sport", ice = TRUE)
plotPartial(sport_ice[sport_ice$yhat.id%in%sample(unique(sport_ice$yhat.id), 100),], plot.pdp = FALSE, alpha = 0.2)

## ----pred----------------------------------------------------------------
lambda_hat <- predict(gbm_sim, data, n.trees = best.iter.cv, type = 'response')
data$lambda_hat <- lambda_hat
head(data, n = 10)

## ----mtpl_data-----------------------------------------------------------
path <- file.path('C:/Users/u0043788/Dropbox/IIR Machine learning en Data Science opleiding/R demos/Gradient boosting')
path.MTPL <- file.path(path, "P&Cdata.txt")
data <- read.table(path.MTPL, header = TRUE)
data <- as.data.frame(data)
head(data)

## ----subset_mtpl---------------------------------------------------------
str(data)
data_subset <- data[ , c(2, 5:14)]

## ----gbm_mtpl------------------------------------------------------------
set.seed(3)
gbm_mtpl <- gbm(NCLAIMS ~ COVERAGE + FUEL + USE + FLEET + SEX + AGEPH + BM + AGEC + POWER + offset(log(EXP)), # formula
               data =  data_subset, # data set
               var.monotone = rep(0,9), # -1: monotone decrease, +1: monotone increase, 0: no monotone restrictions
               distribution = "poisson", # see the help for other choices
               n.trees = 500, # number of trees
               shrinkage = 0.01, # shrinkage or learning rate
               interaction.depth = 2, # 1: additive model, 2: two-way interactions, etc.
               bag.fraction = 0.5, # subsampling fraction
               train.fraction = 0.5, # fraction of data for training
               n.minobsinnode = 10000, # minimum total weight needed in each node
               cv.folds = 3, # do 3-fold cross-validation
               keep.data = TRUE, # keep a copy of the dataset with the object
               verbose = FALSE, # don’t print out progress
               n.cores = 1) # use only a single core

## ---- eval=FALSE---------------------------------------------------------
## # this code can be used to extract the R code from an R Markdown (Rmd) document
## library(knitr)
## setwd('C:/Users/u0043788/Dropbox/Data science for non-life insurance/Computer labs/Gradient boosting')
## file.exists("2019-04-26 Gradient boosting with gbm.Rmd")
## purl("2019-04-26 Gradient boosting with gbm.Rmd")

