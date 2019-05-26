## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(cache = TRUE)

## ----get_rpart_gbm, eval=F-----------------------------------------------
## # remove.packages('rpart')
## # install.packages(devtools)
## # library(devtools)
## # devtools::install_github('RoelHenckaerts/distRforest', dependencies = TRUE, INSTALL_opts = c('--no-lock'))
## # library(rpart)
## # remove.packages('gbm')
## # install_github("harrysouthworth/gbm", dependencies = TRUE, INSTALL_opts = c('--no-lock'))
## # library(gbm)

## ----mtpl_data-----------------------------------------------------------
path <- file.path('C:/Users/u0043788/Dropbox/IIR Machine learning en Data Science opleiding/R demos/Gradient boosting')
path.MTPL <- file.path(path, "P&Cdata.txt")
data <- read.table(path.MTPL, header = TRUE)
data <- as.data.frame(data)

## ----predictors----------------------------------------------------------
predictors <- c('COVERAGE', 'AGEPH', 'SEX', 'BM', 'POWER', 'AGEC', 'FUEL', 'USE', 'FLEET', 'LONG', 'LAT')

## ----get_rpart_Roel, eval=FALSE------------------------------------------
## # remove.packages('rpart')
## # install.packages(devtools)
## # devtools::install_github('RoelHenckaerts/distRforest', dependencies = TRUE, INSTALL_opts = c('--no-lock'))
## # library(rpart)

## ----frequency tree------------------------------------------------------
library(rpart)
# efficient way to specify the model formula
as.formula(paste('cbind(EXP, NCLAIMS)', '~', paste(predictors, collapse=' + ')))
tree_freq <- rpart(formula = as.formula(paste('cbind(EXP, NCLAIMS)',
                                              '~',
                                              paste(predictors,collapse=' + '))), 
                   data = data, 
                   method = 'poisson', 
                   parms = list(shrink = 0.125),
                   control = rpart.control(cp = 1.3e-04,
                                           xval = 0,
                                           maxcompete = 0,
                                           maxsurrogate = 0,
                                           minbucket = 0.01*nrow(data)))
print(tree_freq)

## ----severity tree-------------------------------------------------------
tree_sev <- rpart(formula = as.formula(paste('AVG',
                                             '~',
                                             paste(predictors, collapse = ' + '))),
                  data = data[!is.na(data$AVG), ],
                  method = 'gamma',
                  weights = NCLAIMS,
                  control = rpart.control(cp = 5.2e-03,
                                          xval = 0,
                                          maxcompete = 0,
                                          maxsurrogate = 0,
                                          minbucket = 0.01*nrow(data[!is.na(data$AVG), ])))
print(tree_sev)

## ----predict tree--------------------------------------------------------
pred_tree_freq <- predict(tree_freq, data)
head(pred_tree_freq)
pred_tree_sev <- predict(tree_sev, data)
head(pred_tree_sev)

## ----frequency_rf--------------------------------------------------------
library(data.table)
data <- as.data.table(data)
rf_freq <- rforest(formula = as.formula(paste('cbind(EXP, NCLAIMS)',
                                              '~',
                                              paste(predictors,collapse=' + '))),
                   data = data,
                   method = 'poisson',
                   parms = list(shrink = 1),
                   ncand = 5,
                   ntrees = 1000,
                   subsample = 0.75,
                   redmem = TRUE,
                   control = rpart.control(cp = 0,
                                           xval = 0,
                                           maxcompete = 0,
                                           maxsurrogate = 0,
                                           minbucket = 0.75*0.01*nrow(data)))

## ----severity_rf---------------------------------------------------------
rf_sev <- rforest(formula = as.formula(paste('AVG',
                                             '~',
                                             paste(predictors,collapse=' + '))),
                  data = data[!is.na(data$AVG), ],
                  method = 'gamma',
                  weights = NCLAIMS,
                  ncand = 2,
                  ntrees = 300,
                  subsample = 0.75,
                  redmem = TRUE,
                  control = rpart.control(cp = 0,
                                          xval = 0,
                                          maxcompete = 0,
                                          maxsurrogate = 0,
                                          minbucket = 0.75*0.01*nrow(data[!is.na(data$AVG), ])))

## ----predict rf v1-------------------------------------------------------
pred_rf_freq <- rowMeans(sapply(1:length(rf_freq),
                                function(i) predict(rf_freq[[i]], data)))
head(pred_rf_freq)
pred_rf_sev <- rowMeans(sapply(1:length(rf_sev),
                               function(i) predict(rf_sev[[i]], data)))
head(pred_rf_sev)

## ----predict rf v2-------------------------------------------------------
pred <- rep(0,nrow(data))
for(i in 1:length(rf_freq)) pred <- pred + (1/length(rf_freq))*predict(rf_freq[[i]], data)

## ----equal---------------------------------------------------------------
all(abs(pred - pred_rf_freq) <= 1e-15)

## ----gbm, warning=FALSE, message=FALSE, eval = F-------------------------
## # remove.packages("gbm")
## # install.packages("devtools")
## # library(devtools)
## # install_github("harrysouthworth/gbm", dependencies = TRUE, INSTALL_opts = c('--no-lock'))
## # library(gbm)

## ----frequency gbm-------------------------------------------------------
library(gbm)
gbm_freq <- gbm(formula = as.formula(paste('NCLAIMS',
                                           '~',
                                           paste(predictors,collapse=' + '),
                                           '+ offset(log(EXP))')),
                distribution = 'poisson',
                data = data,
                var.monotone = rep(0,length(predictors)),
                n.trees = 2000,
                interaction.depth = 4,
                shrinkage = 0.01,
                bag.fraction = 0.75,
                n.minobsinnode = 0.75*0.01*nrow(data),
                verbose = FALSE,
                n.cores = 1)

## ----severity gbm--------------------------------------------------------
gbm_sev <- gbm(formula = as.formula(paste('AVG',
                                         '~',
                                         paste(predictors, collapse=' + '))),
              distribution = 'gamma',
              data = data[!is.na(data$AVG),],
              weights = NCLAIMS,
              var.monotone = rep(0,length(predictors)), 
              n.trees = 500,
              interaction.depth = 1,
              shrinkage = 0.01, 
              bag.fraction = 0.75,
              n.minobsinnode = 0.75*0.01*nrow(data[!is.na(data$AVG),]),
              verbose = FALSE,
              n.cores = 1)

## ----predict gbm, warning=FALSE------------------------------------------
pred_gbm_freq <- predict(gbm_freq, data, type = 'response', n.trees = gbm_freq$n.trees)
head(pred_gbm_freq)
pred_gbm_sev <- predict(gbm_sev, data, type = 'response', n.trees = gbm_sev$n.trees)
head(pred_gbm_sev)

## ----folds---------------------------------------------------------------
K_folds <- 6
data <- data[order(data$NCLAIMS, data$AVG, data$EXP),]
data$fold <- paste0('data', rep(1:K_folds, length = nrow(data)))
head(data)

## ----strat_stats---------------------------------------------------------
library(dplyr)
data %>% group_by(fold) %>% 
  summarize(emp_claim_freq = sum(NCLAIMS)/n())
data %>% na.omit() %>% group_by(fold) %>% 
  summarize(emp_claim_sev = sum(AVG)/n())
data %>% filter(!is.na(AVG)) %>% group_by(fold) %>% 
  summarize(emp_claim_sev = sum(AVG)/n())
data %>% filter(complete.cases(.)) %>% group_by(fold) %>% 
  summarize(emp_claim_sev = sum(AVG)/n())

## ----split1--------------------------------------------------------------
test_data <- data[fold == 'data1', ]
valid_data <- data[fold == 'data2', ]
train_data <- data[!(fold %in% c('data1','data2')), ]

## ----split2--------------------------------------------------------------
test_data <- data[fold == 'data1', ]
valid_data <- data[fold == 'data3', ]
train_data <- data[!(fold %in% c('data1','data3')), ]

## ----poiss_dev-----------------------------------------------------------
#' Calculate the Poisson deviance
#' @param y The true values (numeric vector)
#' @param yhat The estimates for y (numeric vector)
#' @param w Optional case weights (numeric vector)
#' @param scaled Deviance scaled by number of observations or not (boolean)
#' @return A single number containing the Poisson deviance
poiss_dev <- function(y, yhat, w = 1, scaled = TRUE){
  sf <- ifelse(scaled, 1/length(y[!is.na(y)]), 1)
  return(-2*sf*sum(w*(dpois(y,yhat,log=TRUE) - dpois(y,y,log=TRUE)), na.rm = TRUE))
}

## ----gamma_dev-----------------------------------------------------------
#' Calculate the Gamma deviance
#' @param y The true values (numeric vector)
#' @param yhat The estimates for y (numeric vector)
#' @param w Optional case weights (numeric vector)
#' @param scaled Deviance scaled by number of observations or not (boolean)
#' @return A single number containing the gamma deviance
gamma_dev <- function(y, yhat, w = 1, scaled = TRUE){
  sf <- ifelse(scaled, 1/length(y[!is.na(y)]), 1)
  return(-2*sf*sum(w*(log(y/yhat) - (y - yhat)/yhat), na.rm = TRUE))
}

## ----cross_val-----------------------------------------------------------
cp_grid <- as.vector(outer(1:9,10^(-5:-3))) # 1e-05 ... 9e-05 1e-04 ... 9e-04 1e-03 ... 9e-03
cv_perf <- rep(0, length(cp_grid)) # empty vector to store results over cp values
test_fold <- 'data1'
val_folds <- c('data2','data3','data4','data5','data6')
for(cp in cp_grid){
  cp_perf <- rep(0, length(val_folds)) # empty vector to store results over val folds
  for(val_fold in val_folds){
    train_data <- data[!(data$fold %in% c(test_fold, val_fold)), ]
    val_data <- data[data$fold == val_fold, ]
    tree_fit <- rpart(formula = as.formula(paste('cbind(EXP, NCLAIMS)','~',paste(predictors,collapse=' + '))), 
                      data = train_data, # only use training data
                      method = 'poisson', 
                      control = rpart.control(cp = cp, # use the current cp value from the grid
                                              xval = 0, 
                                              maxcompete = 0, 
                                              maxsurrogate = 0, 
                                              minbucket = 1000)) 
    tree_pred <- predict(tree_fit, val_data) # predict the tree on the validation data
    tree_perf <- poiss_dev(y = val_data$NCLAIMS, yhat = tree_pred*val_data$EXP) # evaluate the tree on the validation data
    cp_perf[grep(val_fold,val_folds)] <- tree_perf
  }
  cv_perf[grep(cp,cp_grid)] <- mean(cp_perf)
}

## ----cv_vis--------------------------------------------------------------
library(ggplot2)
ggplot(data.table('cp' = as.factor(cp_grid), 'perf' = cv_perf), aes(cp, perf, group = 1)) + geom_line() + theme_bw() + theme(axis.text.x = element_text(angle = 90)) + ylab('Cross-validation performance')

## ----ggplot2-------------------------------------------------------------
library(ggplot2)

## ----vi tree freq--------------------------------------------------------
vi <- tree_freq$variable.importance
vi[attr(tree_freq$terms, 'term.labels')[!(attr(tree_freq$terms, 'term.labels') %in% names(vi))]] = 0

DTvi <- data.table(variable = names(vi), vi = vi)
DTvi <- DTvi[, vi := round(vi/sum(vi), digits = 4)][order(-vi)]

ggplot(DTvi, aes(reorder(variable,vi),vi)) + geom_col(colour = '#003366',fill = '#003366') + coord_flip() + theme_bw() + labs(x = '', y = 'variable importance') + ggtitle('Frequency tree')

## ----vi gbm freq---------------------------------------------------------
df_vi <- summary(gbm_freq, plotit = FALSE, normalize = FALSE)
vi <- mapply(function(x,y) { y }, as.character(df_vi$var), df_vi$rel.inf, SIMPLIFY = TRUE, USE.NAMES = TRUE)

DTvi <- data.table(variable = names(vi), vi = vi)
DTvi <- DTvi[, vi := round(vi/sum(vi), digits = 4)][order(-vi)]

ggplot(DTvi, aes(reorder(variable,vi),vi)) + geom_col(colour = '#003366',fill = '#003366') + coord_flip() + theme_bw() + labs(x = '', y = 'variable importance') + ggtitle('Frequency gbm')

## ----pdp tree ageph------------------------------------------------------
DTeffect <- data.table('AGEPH' = seq(min(data$AGEPH), max(data$AGEPH)))
data_copy <- data.table::copy(data)
effect <- sapply(1:nrow(DTeffect), 
                 function(i) mean(predict(tree_freq, data_copy[,'AGEPH' := DTeffect[i, 'AGEPH', with=FALSE]])))
DTeffect[, 'effect' := effect]

ggplot(DTeffect, aes(x = AGEPH, y = effect)) + geom_line(colour = '#003366', size = 1) + theme_bw() + xlab('ageph') + ylab('Partial dependence') + ggtitle('Frequency tree')

## ----pdp gbm ageph, warning=FALSE----------------------------------------
DTeffect <- data.table('AGEPH' = seq(min(data$AGEPH), max(data$AGEPH)))
data_copy <- data.table::copy(data)
effect <- sapply(1:nrow(DTeffect), 
                 function(i) mean(predict(gbm_freq, data_copy[,'AGEPH' := DTeffect[i, 'AGEPH', with=FALSE]], type = 'response', n.trees = gbm_freq$n.trees)))
DTeffect[, 'effect' := effect]

ggplot(DTeffect, aes(x = AGEPH, y = effect)) + geom_line(colour = '#003366', size = 1) + theme_bw() + xlab('ageph') + ylab('Partial dependence') + ggtitle('Frequency gbm')

## ----shapefile, message=FALSE--------------------------------------------
library(rgdal)
library(ggmap)
setwd('C:/Users/u0043788/Dropbox/Data science for non-life insurance/Computer labs/Tree-based ML')
readShapefile = function(){
  belgium_shape <- readOGR(dsn = path.expand(paste(getwd(),"/Shape file Belgie postcodes",sep="")), layer = "npc96_region_Project1")
  belgium_shape <- spTransform(belgium_shape, CRS('+proj=longlat +datum=WGS84'))
  belgium_shape$id <- row.names(belgium_shape)
  return(belgium_shape)
}

## ----pdp tree spatial, message=FALSE, warning=FALSE----------------------
DTeffect <- data.table('LONG' = unname(coordinates(readShapefile())[,1]),
                       'LAT' = unname(coordinates(readShapefile())[,2]))
data_copy <- data.table::copy(data)
effect <- sapply(1:nrow(DTeffect), 
                 function(i) mean(predict(tree_freq, data_copy[, c('LONG','LAT') := DTeffect[i, c('LONG','LAT'), with=FALSE]])))
DTeffect[, 'effect' := effect]

belgium_shape <- readShapefile()
belgium_shape@data <- cbind(belgium_shape@data, DTeffect[,'effect'])
belgium_shape_f <- fortify(belgium_shape)
belgium_shape_f <- merge(belgium_shape_f, belgium_shape@data, all.x = TRUE)
ggplot(belgium_shape_f, aes(long, lat, group = group)) + geom_polygon(aes(fill = belgium_shape_f$effect)) + scale_fill_gradient(low='#99CCFF',high='#003366') + theme_nothing(legend = TRUE) + labs(fill='Part. dep.') + ggtitle('Frequency tree')

## ----pdp gbm spatial, message=FALSE, warning=FALSE-----------------------
DTeffect <- data.table('LONG' = unname(coordinates(readShapefile())[,1]),
                       'LAT' = unname(coordinates(readShapefile())[,2]))
data_copy <- data.table::copy(data)
effect <- sapply(1:nrow(DTeffect), 
                 function(i) mean(predict(gbm_freq, data_copy[, c('LONG','LAT') := DTeffect[i, c('LONG','LAT'), with=FALSE]], type = 'response', n.trees = gbm_freq$n.trees)))
DTeffect[, 'effect' := effect]

belgium_shape <- readShapefile()
belgium_shape@data <- cbind(belgium_shape@data, DTeffect[,'effect'])
belgium_shape_f <- fortify(belgium_shape)
belgium_shape_f <- merge(belgium_shape_f, belgium_shape@data, all.x = TRUE)
ggplot(belgium_shape_f, aes(long, lat, group = group)) + geom_polygon(aes(fill = belgium_shape_f$effect)) + scale_fill_gradient(low='#99CCFF',high='#003366') + theme_nothing(legend = TRUE) + labs(fill='Part. dep.') + ggtitle('Frequency gbm')

## ----ice tree ageph------------------------------------------------------
DTeffect <- data.table('AGEPH' = seq(min(data$AGEPH), max(data$AGEPH)))
data_copy <- data.table::copy(data)
effect <- sapply(1:nrow(DTeffect), 
                 function(i) predict(tree_freq, data_copy[,'AGEPH' := DTeffect[i, 'AGEPH', with=FALSE]]))
DTeffect <- cbind(DTeffect, t(effect))

set.seed(54321)
# Select 10 000 random profiles for the ice curves
ice_profiles <- c(1,sample(2:ncol(DTeffect),10000))
ggplt <- ggplot()
# Add the ice curves
ggplt <- ggplt + geom_line(data = melt(DTeffect[, ice_profiles, with = FALSE], id.vars = 'AGEPH'), aes(x = AGEPH, y = value, group = variable), alpha = 0.1, colour = 'grey75')
# Add the pdp curve
ggplt <- ggplt + geom_line(data = data.table('AGEPH' = DTeffect[['AGEPH']], 'effect' = rowMeans(DTeffect[, 2:ncol(DTeffect)])), aes(x = AGEPH, y = effect, group = 1), colour = 'navy', size = 1)
# Add some style
ggplt <- ggplt + theme_bw() + xlab('ageph') + ylab('Individual conditional expectation') + ggtitle('Frequency tree')
ggplt

## ----ice gbm ageph, message=FALSE, warning=FALSE-------------------------
DTeffect <- data.table('AGEPH' = seq(min(data$AGEPH), max(data$AGEPH)))
data_copy <- data.table::copy(data)
effect <- sapply(1:nrow(DTeffect), 
                 function(i) predict(gbm_freq, data_copy[,'AGEPH' := DTeffect[i, 'AGEPH', with=FALSE]]))
DTeffect <- cbind(DTeffect, t(effect))

set.seed(54321)
# Select 10 000 random profiles for the ice curves
ice_profiles <- c(1,sample(2:ncol(DTeffect), 10000))
ggplt <- ggplot()
# Add the ice curves
ggplt <- ggplt + geom_line(data = melt(DTeffect[, ice_profiles, with = FALSE], id.vars = 'AGEPH'), aes(x = AGEPH, y = value, group = variable), alpha = 0.1, colour = 'grey75')
# Add the pdp curve
ggplt <- ggplt + geom_line(data = data.table('AGEPH' = DTeffect[['AGEPH']], 'effect' = rowMeans(DTeffect[, 2:ncol(DTeffect)])), aes(x = AGEPH, y = effect, group = 1), colour = 'navy', size = 1)
# Add some style
ggplt <- ggplt + theme_bw() + xlab('AGEPH') + ylab('Individual conditional expectation') + ggtitle('Frequency tree')
ggplt

## ---- eval=FALSE---------------------------------------------------------
## # this code can be used to extract the R code from an R Markdown (Rmd) document
## library(knitr)
## setwd('C:/Users/u0043788/Dropbox/Data science for non-life insurance/Computer labs/Tree-based ML')
## file.exists("2019-05-01 Tree-based ML.Rmd")
## purl("2019-05-01 Tree-based ML.Rmd")

