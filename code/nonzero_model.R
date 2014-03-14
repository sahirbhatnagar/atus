##################################
# R source code file used to model the ATUS respondents with non-zero tv time use
# Created by Sahir, March 14, 2014
# Updated March 14, 2014
# hosted on Github repo 'sahirbhatnagar/atus'
# NOTE: 
##################################

# directory where the data and list of variables are located
setwd("~/git_repositories/atus.git/data")

# import the data
source("~/git_repositories/atus.git/code/prelim_fit.R")

library(grplasso)

#number of 
DT[,.N, by=TVind]

#only 324 people who watched religious tv
DT.y[t120304!=0]




contr <- rep(list("contr.sum"), ncol(DT.cat) )
names(contr) <- names(DT.cat)

fit <- grplasso(TVTIME ~ ., data = DT, model = LinReg(), lambda = 20,
                contrasts = contr, center = TRUE, standardize = TRUE)
str(TVtime)

str(DT.cat)

kk<-DT[,print(.SD),by=TUCASEID]
##################

data(splice)

## Define a list with the contrasts of the factors
contr <- rep(list("contr.sum"), ncol(splice) - 1)
names(contr) <- names(splice)[-1]

fit.splice <- grplasso(y ~ ., data = splice, model = LinReg(), lambda = 20,
                       contrasts = contr, center = TRUE, standardize = TRUE)
summary(fit.splice)
names(fit.splice)
plot(fit.splice)
