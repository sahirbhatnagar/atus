##################################
# R source code file used to model the ATUS respondents with non-zero tv time use
# Created by Sahir, March 14, 2014
# Updated March 14, 2014
# hosted on Github repo 'sahirbhatnagar/atus'
# NOTE: 
##################################

# directory where the data and list of variables are located -----
setwd("~/git_repositories/atus/data")

# import the data -----
source("~/git_repositories/atus/code/prelim_fit.R")

library(grplasso)
library(biglm)

#number of people who watched vs not watched ----
DT[,.N, by=TVind]


# Subset the data ---------------------------------------------------------

#DT with non zero indicators 
DT <- DT[TVind!=0]

#remove id and tv indicator variable 
DT <- DT[,c(-1,-3),with=FALSE]

# reduced dataset to work with ggplot2 (full dataset crashes ggplot2 if I 
# make a coding error when using ggplot2)
set.seed(1234)
DT <- DT[sample(x=1:nrow(DT), size=0.20*nrow(DT)), , ]


# Fit Gamma GLM -----------------------------------------------------------

#gamma regression with log link
fit<- glm(TVtime ~ ., data = DT, family=Gamma(link="log"))
summary(fit)

#residuals look good
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(fit)

#very long
step<-step(fit, direction="backward")

#backward selection
drop<-sdrop1(fit, test="LRT")







# Group Lasso (not working) -----------------------------------------------

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
