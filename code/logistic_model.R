##################################
# R source code file used to model the ATUS respondents binary tv time use (watch, didn't watch)
# Created by Maxime, March 14, 2014
# Updated March 14, 2014
# hosted on Github repo 'sahirbhatnagar/atus'
# NOTE: 
##################################

# directory where the data and list of variables are located
setwd("~/git_repositories/atus.git/data")

# import the data
source("~/git_repositories/atus.git/code/prelim_fit.R")

X <- merge(DT.cont, apply(DT.cat, 2, as.numeric), by="TUCASEID")
X<-X[,-1,]

#logistic regression with "penalized"
library(penalized)

prefit <- penalized(TVind, ~., data=X, model="logistic", lambda2=0, steps="Park")
m.lambda <- prefit[[1]]@lambda1

cv.fit <- optL1 (TVind, ~., data=X, lambda2 = 0, minlambda1=0, maxlambda1=m.lambda, model = "logistic", fold=5)
fit <- penalized(TVind, ~., data=X, model="logistic", lambda1=cv.fit$lambda)
