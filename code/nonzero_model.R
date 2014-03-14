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
library(biglm)

#number of people who watched vs not watched 
DT[,.N, by=TVind]

#DT with non zero indicators
DT <- DT[TVind!=0]

#remove id variable
DT <- DT[,-1,with=FALSE]


fit <- grplasso(TVtime ~ ., data = DT, model = LinReg(), lambda = 20,
          center = TRUE, standardize = TRUE)





larFit$coefficients

str(X)
larFitBlocked <- biglars.fit(diabetes$x, diabetes$y, type = "lar",
                             blockSize = 50)

data(diabetes)

str(diabetes)

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
