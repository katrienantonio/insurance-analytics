## ---- include=FALSE------------------------------------------------------
# overall knitr options
knitr::opts_chunk$set(cache = FALSE, echo = TRUE, warning = FALSE, message = FALSE)

## ----set_seed------------------------------------------------------------
set.seed(1)

## ----set_n---------------------------------------------------------------
n <- 500000 # number of observations

## ----generate_covariates-------------------------------------------------
Gender <- factor(sample(c("m", "f"), n, replace = TRUE))
Age <- sample(c(18:65), n, replace = TRUE)
Split <- factor(sample(c("yes", "no"), n, replace = TRUE))
Sport <- factor(sample(c("yes", "no"), n, replace = TRUE))

## ----specify_lambda------------------------------------------------------
lambda <- 0.1*ifelse(Gender == "m", 1.1, 1)
lambda <- lambda*ifelse(Age >= 18 & Age < 30, 1.4, 1)
lambda <- lambda*ifelse(Age >= 30 & Age < 45, 1.2, 1)
lambda <- lambda*ifelse(Sport == "yes", 1.15, 1)

## ----generate_poisson_data-----------------------------------------------
N <- rpois(n, lambda)
data <- data.frame(N, Gender, Age, Sport, Split)

## ----exercise----------------------
library(dplyr)
data %>% group_by(Gender, Age, Sport) %>% 
  summarize(claim_freq = sum(N)/n())

## ----load_rpart----------------------------------------------------------
# install.packages("rpart")
library(rpart)

## ---- eval=FALSE---------------------------------------------------------
## rpart(formula, data, weights, subset, na.action = na.rpart, method,
## model = FALSE, x = FALSE, y = TRUE, parms, control, cost, ...)

## ----tree_with_cp_1------------------------------------------------------
tree_cp_1 <- rpart(N ~ Gender + Age + Split + Sport, data = data, method = "poisson", control = rpart.control(cp = 1, maxdepth = 5))
tree_cp_1
summary(tree_cp_1)

## ------------------------------------------------------------------------
sum(data$N)

## ------------------------------------------------------------------------
sum(data$N)/n

## ------------------------------------------------------------------------
279043.3/(500000-0)

## ----tree_with_cp_0------------------------------------------------------
tree_cp_0 <- rpart(N ~ Gender + Age + Split + Sport, data = data, method = "poisson", control = rpart.control(cp = 0, maxdepth = 5))
tree_cp_0

## ------------------------------------------------------------------------
#install.packages("dplyr")
library(dplyr)
data %>% filter(Age >= 44.5, Sport == "no", Gender == "f", Split == "yes", Age < 64.5) %>% 
  summarize(claim_freq_split = sum(N)/n())  

## ------------------------------------------------------------------------
tree_cp_0_star <- rpart(N ~ Gender + Age + Split + Sport, data = data, method = "poisson", parms = list(shrink = 10), control = rpart.control(cp = 0, maxdepth = 5))
tree_cp_0_star

## ------------------------------------------------------------------------
lambda_0 <- sum(data$N)/n
lambda_0
test <- data %>% filter(Age >= 44.5, Sport == "no", Gender == "f", Split == "yes", Age < 64.5) %>% 
  summarize(claims_node_32 = sum(N), exp_node_32 = n())
lambda_32 <- (1 + test$claims_node_32)/(1/lambda_0 + test$exp_node_32)
lambda_32

## ------------------------------------------------------------------------
summary(tree_cp_0)
tree_cp_0$variable.importance
round(100*tree_cp_0$variable.importance/sum(tree_cp_0$variable.importance), digits = 0)

## ----visualize_tree_partykit---------------------------------------------
# plot tree
# install.packages(`partykit')
library(partykit) # package to produce plots for rpart trees
tree_cp_0_party <- as.party(tree_cp_0)
plot(tree_cp_0_party)

## ------------------------------------------------------------------------
tree_cp_0.00005 <- rpart(N ~ Gender + Age + Split + Sport, data = data, method = "poisson", control = rpart.control(cp = 0.00005, maxdepth = 5))
summary(tree_cp_0.00005)
# plot tree
tree_cp_0.00005_party <- as.party(tree_cp_0.00005)
plot(tree_cp_0.00005_party)

# fit model with cp=0.0002
tree_cp_0.0002 <- rpart(N ~ Gender + Age + Split + Sport, data = data, method = "poisson", control = rpart.control(cp = 0.0002, maxdepth = 5))
summary(tree_cp_0.0002)
# plot tree
tree_cp_0.0002_party <- as.party(tree_cp_0.0002)
plot(tree_cp_0.0002_party)

## ------------------------------------------------------------------------
printcp(tree_cp_0)
plotcp(tree_cp_0)

## ------------------------------------------------------------------------
111779.7000 + 166261.4000

## ------------------------------------------------------------------------
(111779.7000 + 166261.4000)/279043

## ------------------------------------------------------------------------
c_opt <- tree_cp_0$cptable[which.min(tree_cp_0$cptable[,"xerror"]),"CP"]
c_opt

## ------------------------------------------------------------------------
tree_opt <- prune(tree_cp_0, cp = c_opt)
tree_opt <- as.party(tree_opt)
plot(tree_opt)

## ------------------------------------------------------------------------
lambda_hat <- predict(tree_opt) 
data$lambda_hat <- lambda_hat
head(data)

## ------------------------------------------------------------------------
class <- partykit:::.list.rules.party(tree_opt)
data$class <- class[as.character(predict(tree_opt, type = "node"))]
head(data)

## ------------------------------------------------------------------------
s <- subset(data, select = c(lambda_hat, class))

s <- unique(s)

s[order(s$lambda_hat), ]

## ------------------------------------------------------------------------
path <- file.path('C:/Users/u0043788/Dropbox/IIR Machine learning en Data Science opleiding/R demos/Pricing analytics')
path.MTPL <- file.path(path, "P&Cdata.txt")
path.MTPL
DT <- read.table(path.MTPL, header = TRUE)
DT <- as.data.frame(DT)
head(DT)

## ------------------------------------------------------------------------
tree <- rpart( cbind(EXP, NCLAIMS) ~ AGEPH + POWER + FUEL + SEX + COVERAGE + USE + FLEET, data = DT, method="poisson", control = rpart.control(cp = 0, maxdepth = 5)) 
printcp(tree) 

## ---- eval=FALSE---------------------------------------------------------
## # this code can be used to extract the R code from an R Markdown (Rmd) document
## library(knitr)
## setwd('C:/Users/u0043788/Dropbox/Data science for non-life insurance/Computer labs/Regression trees')
## file.exists("2019-04-26 Regression trees with rpart.Rmd")
## purl("2019-04-26 Regression trees with rpart.Rmd")

