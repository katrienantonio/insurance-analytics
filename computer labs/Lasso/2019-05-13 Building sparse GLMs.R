## ---- include=FALSE------------------------------------------------------
# overall knitr options
knitr::opts_chunk$set(cache = FALSE, echo = TRUE, warning = FALSE, message = FALSE)

## ----load_data_set-------------------------------------------------------
path <- "C:/Users/u0043788/Dropbox/APC Module Data Science/R demos/Lasso"
MTPL.path <- file.path(path, "P&Cdata_extended.txt")
MTPL <- read.table(MTPL.path)
head(MTPL)

## ----data_inspection-----------------------------------------------------
names(MTPL)

## ----GLM-----------------------------------------------------------------
MTPL_formula <- NCLAIMS ~ 1 + AGEPH + SEX + BM + POPUL + PAYFREQ + COVERAGE + AGEC + FUEL + SPORT + FLEET + MONOVOL + POWER + FOUR + USE
MTPL_GLM <- glm(MTPL_formula, data = MTPL, offset = log(MTPL$DURATION), family=poisson())
summary(MTPL_GLM)

## ----install_package_glmnet, eval = FALSE, include = TRUE----------------
## install.packages(glmnet)

## ----glmnet0, eval = TRUE, include = FALSE-------------------------------
library(glmnet)

## ----glmnet, eval = FALSE, include = TRUE--------------------------------
## library(glmnet)
## ? glmnet

## ----glmnet_fit----------------------------------------------------------
y <- MTPL$NCLAIMS
# set up model matrix, note that factors with more than two levels are coded without a reference category (see the contrasts.arg argument)
x <- model.matrix( ~ 1 + AGEPH + SEX + BM + POPUL + PAYFREQ + COVERAGE + AGEC + FUEL + SPORT + FLEET + MONOVOL + POWER + FOUR + USE, data = MTPL, contrasts.arg = lapply(MTPL[,c("PAYFREQ", "COVERAGE")], contrasts, contrasts=FALSE))[,-1]
# check first 10 rows in x
x[1:10,]
# ensure a Lasso penalty
alpha <- 1    
# fit regularized Poisson model with Lasso penalty 
MTPL_glmnet <- glmnet(x = x, y = y, family = "poisson", offset = log(MTPL$DURATION), alpha = alpha, standardize = TRUE, intercept = TRUE)
# show the coefficient names in order of the fitted model
(coefficient_names <- row.names(MTPL_glmnet$beta)) 
# plots the estimates for all different lambdas; numbers refer to the order of coefficients
plot(MTPL_glmnet, xvar = 'lambda', label =TRUE)    

## ----cv.glmnet1----------------------------------------------------------
# to make results reproducible, fix the random number seed and the allocation of the observations to the different folds
set.seed(942045)
foldid <- sample(rep(1:10, length.out = nrow(MTPL)), nrow(MTPL))

MTPL_glmnet_cv <- cv.glmnet(x, y, family = "poisson", alpha = 1, nfolds = 10, foldid = foldid, type.measure = "deviance", standardize = TRUE, intercept = TRUE)
plot(MTPL_glmnet_cv)

## ----cv.glmnet2----------------------------------------------------------
MTPL_glmnet_cv$lambda.min
coef(MTPL_glmnet_cv, s = "lambda.min")
MTPL_glmnet_cv$lambda.1se
coef(MTPL_glmnet_cv, s = "lambda.1se")

## ----refit1--------------------------------------------------------------
library(plyr)
# first we need to recode the PAYFREQ variable:
MTPL$PAYFREQ    <- revalue(MTPL$PAYFREQ, c("biyearly" = "bi+month", "monthly" = "bi+month")) # to merge these levels
MTPL$PAYFREQ    <- relevel(MTPL$PAYFREQ, "bi+month") # to make this the reference category

# now we can put the relevant variables in a GLM
MTPL_formula_refit <- NCLAIMS ~ 1 + AGEPH + BM + POPUL + PAYFREQ + FUEL + POWER
MTPL_glm_refit <- glm(MTPL_formula_refit, data = MTPL, offset = log(MTPL$DURATION), family = poisson())
summary(MTPL_glm_refit)

## ---- eval=FALSE---------------------------------------------------------
## # this code can be used to extract the R code from an R Markdown (Rmd) document
## library(knitr)
## setwd(path)
## file.exists("2018-09-25 Building sparse GLMs.Rmd")
## purl("2018-09-25 Building sparse GLMs.Rmd")

