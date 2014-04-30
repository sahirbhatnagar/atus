##################################
# R source code file used to model the ATUS respondents binary tv time use
#(watch, didn't watch)
# Created by Maxime, March 14, 2014
# Updated April 30, 2014
# hosted on Github repo 'sahirbhatnagar/atus'
# NOTE: 
##################################

# directory where the data and list of variables are located
setwd("~/git_repositories/atus.git/data")

# import the data
source("~/git_repositories/atus.git/code/data_cleaning.R")

#logistic regression with "grplasso"
library(grplasso)

#Recall that the following variable are categorical
#TUYEAR (10), TUMONTH (12), TUDIARYDAY (7), GEREG (4)
#PEMARITL (6), TELFS (5), PEEDUCA (5), PRCITSHP (5)
#PTDTRACE (5), TEMJOT (3), TRDPFTPT (3), EDUC (5)
#In order to use grplasso we need to introduce dummy variables

#Note: for the list of possible values, use the function unique()

DT$TUYEAR2003 <- as.numeric(DT$TUYEAR==2003)
DT$TUYEAR2004 <- as.numeric(DT$TUYEAR==2004)
DT$TUYEAR2005 <- as.numeric(DT$TUYEAR==2005)
DT$TUYEAR2006 <- as.numeric(DT$TUYEAR==2006)
DT$TUYEAR2007 <- as.numeric(DT$TUYEAR==2007)
DT$TUYEAR2008 <- as.numeric(DT$TUYEAR==2008)
DT$TUYEAR2009 <- as.numeric(DT$TUYEAR==2009)
DT$TUYEAR2010 <- as.numeric(DT$TUYEAR==2010)
DT$TUYEAR2011 <- as.numeric(DT$TUYEAR==2011)

DT$TUMONTH1 <- as.numeric(DT$TUMONTH==1)
DT$TUMONTH2 <- as.numeric(DT$TUMONTH==2)
DT$TUMONTH3 <- as.numeric(DT$TUMONTH==3)
DT$TUMONTH4 <- as.numeric(DT$TUMONTH==4)
DT$TUMONTH5 <- as.numeric(DT$TUMONTH==5)
DT$TUMONTH6 <- as.numeric(DT$TUMONTH==6)
DT$TUMONTH7 <- as.numeric(DT$TUMONTH==7)
DT$TUMONTH8 <- as.numeric(DT$TUMONTH==8)
DT$TUMONTH9 <- as.numeric(DT$TUMONTH==9)
DT$TUMONTH10 <- as.numeric(DT$TUMONTH==10)
DT$TUMONTH11 <- as.numeric(DT$TUMONTH==11)

DT$TUDIARYDAY1 <- as.numeric(DT$TUDIARYDAY==1)
DT$TUDIARYDAY2 <- as.numeric(DT$TUDIARYDAY==2)
DT$TUDIARYDAY3 <- as.numeric(DT$TUDIARYDAY==3)
DT$TUDIARYDAY4 <- as.numeric(DT$TUDIARYDAY==4)
DT$TUDIARYDAY5 <- as.numeric(DT$TUDIARYDAY==5)
DT$TUDIARYDAY6 <- as.numeric(DT$TUDIARYDAY==6)

DT$GEREG1 <- as.numeric(DT$GEREG==1)
DT$GEREG2 <- as.numeric(DT$GEREG==2)
DT$GEREG3 <- as.numeric(DT$GEREG==3)

DT$PEMARITL1 <- as.numeric(DT$PEMARITL==1)
DT$PEMARITL2 <- as.numeric(DT$PEMARITL==2)
DT$PEMARITL3 <- as.numeric(DT$PEMARITL==3)
DT$PEMARITL4 <- as.numeric(DT$PEMARITL==4)
DT$PEMARITL5 <- as.numeric(DT$PEMARITL==5)

DT$TELFS1 <- as.numeric(DT$TELFS==1)
DT$TELFS2 <- as.numeric(DT$TELFS==2)
DT$TELFS3 <- as.numeric(DT$TELFS==3)
DT$TELFS4 <- as.numeric(DT$TELFS==4)

DT$PEEDUCA1 <- as.numeric(DT$PEEDUCA==1)
DT$PEEDUCA2 <- as.numeric(DT$PEEDUCA==2)
DT$PEEDUCA3 <- as.numeric(DT$PEEDUCA==3)
DT$PEEDUCA4 <- as.numeric(DT$PEEDUCA==4)

DT$PRCITSHP1 <- as.numeric(DT$PRCITSHP==1)
DT$PRCITSHP2 <- as.numeric(DT$PRCITSHP==2)
DT$PRCITSHP3 <- as.numeric(DT$PRCITSHP==3)
DT$PRCITSHP4 <- as.numeric(DT$PRCITSHP==4)

DT$PTDTRACE1 <- as.numeric(DT$PTDTRACE==1)
DT$PTDTRACE2 <- as.numeric(DT$PTDTRACE==2)
DT$PTDTRACE3 <- as.numeric(DT$PTDTRACE==3)
DT$PTDTRACE4 <- as.numeric(DT$PTDTRACE==4)

DT$TEMJOT1 <- as.numeric(DT$TEMJOT==1)
DT$TEMJOT2 <- as.numeric(DT$TEMJOT==2)

DT$TRDPFTPT1 <- as.numeric(DT$TRDPFTPT==1)
DT$TRDPFTPT2 <- as.numeric(DT$TRDPFTPT==2)

DT$EDUC1 <- as.numeric(DT$EDUC==1)
DT$EDUC2 <- as.numeric(DT$EDUC==2)
DT$EDUC3 <- as.numeric(DT$EDUC==3)
DT$EDUC4 <- as.numeric(DT$EDUC==4)

DT <- subset(DT, select=!(colnames(DT) %in% c("TUYEAR", "TUMONTH", "TUDIARYDAY", 
      "GEREG", "PEMARITL", "TELFS", "PEEDUCA", "PRCITSHP", "PTDTRACE", "TEMJOT", 
      "TRDPFTPT", "EDUC")))

#response is TVIND
Y <- as.vector(DT$TVIND)

#for the design matrix, we need to include the intercept
#moreover, we remove TUCASEID, TVTIME, TVIND (response), and FAMINC (not reliable)
X <- subset(DT, select=!(colnames(DT) %in% c("TUCASEID", "TVTIME", "TVIND", "FAMINC")))
X <- as.matrix(cbind(rep(1, nrow(X)), X))

#we then need an index parameter
index <- c(NA, 1:13, rep(15,9), rep(16,11), rep(17,6), rep(18,3), rep(19,5),
         rep(20,4), rep(21,4), rep(22,4), rep(23,4), rep(24,2), rep(25,2), rep(26,4))

#find max value of tuning parameter that kicks out all regression parameters
lamb.max <- lambdamax(X, Y, index)

#we define a lambda grid
lambda <- seq(50, lamb.max, length.out=25)

#we fit the penalized model
fit <- grplasso(X, Y, index, model = LogReg(), lambda=lambda, center = TRUE, standardize = TRUE)

#we can compute the BIC
BIC <- rep(0,26)
log.lik <- LogReg()@nloglik
matrix <- predict(fit, type='link')


for(i in 1:26){
  BIC[i] <- 2*log.lik(Y, matrix[,i], rep(1,length(Y))) + log(nrow(X))*sum(fit$coef[,i]!=0)
}