##################################
# R source code file used to fit gamma_model.txt for TVTIME > 0
# Created by Sy, May 3rd, 2014
# Updated May 6th, 2014
# hosted on Github repo 'sahirbhatnagar/atus'
# NOTE:
##################################

#source("~/git_repositories//atus//code//data_cleaning.R")

#On kevisco's laptop
# setwd("~/atus/data")
# On Sy's laptop
setwd("~//git_repositories/atus/data/")
# On Sy's desktop
#setwd("~//git_repositories//atus.git//data/")
load("data.Rda")

library(data.table)
library(bit64)
library(plotrix)
library(rjags)
list.modules()
load.module("glm")
list.modules()
#unload.module("glm")
#list.modules()

xtabs(~DT$TUYEAR)

#DT with non zero indicators 
DT <- DT[TVIND!=0]
#Sample subset of survey data
set.seed(1234)
prop <- 0.005
DTS <- DT[sample(x=1:nrow(DT), size=prop*nrow(DT)), , ]
xtabs(~DTS$QUARTER)

#We first create the datafiles for JAGS

# outpath <- "~//git_repositories//atus.git//data"
# inpath <- "~//git_repositories//atus.git//code"

outpath <- "~//git_repositories//atus//data"
inpath <- "~//git_repositories//atus//code"


datalist.gam <- list('DIARYDAY'=DTS$TUDIARYDAY, 
                     'REGION'=DTS$GEREG, 
                     'HISPANIC'=DTS$PEHSPNON, 
                     'SEX'=DTS$TESEX, 
                     'RACE'=DTS$PTDTRACE, 
                     'ECON1'=DTS$ECON1,
                     'ECON2'=DTS$ECON2,
                     'QUARTER'=DTS$QUARTER,
                     'YEAR'=DTS$TUYEAR-2012+1,
                     'MONTH'=DTS$TUMONTH,
                     'RESPONSE'=DTS$TVTIME, 
                     'N'=nrow(DTS))
dput(datalist.gam, file.path(outpath, "datagam.txt"))


# Then we need initial values (technically we dont need them, and since we 
# dont have empirical estimates, so we let JAGS decide)
# initslist.gam <- list('alpha_diary'=c(rep(1,6),NA), 'alpha_region'=c(rep(1,3),NA), 
#                       'alpha_hispanic'=1, 'alpha_sex'=1, 'alpha_race'=c(rep(1,4),NA), 
#                       'beta_econ_1'=1, 'beta_econ_2'=1)
# dput(initslist.gam, file.path(outpath, "initsgam.txt"))

#Models

model <- jags.model(file.path(inpath, 'gamma_model.txt'), data=datalist.gam, 
                    n.chains=2, n.adapt=10000, quiet=FALSE)

# ss = coda.samples(model, c("alpha_diary","alpha_region","alpha_hispanic","alpha_sex",
#                            "alpha_race", "beta_econ_1", "beta_econ_2"), 10000, thin=10)
#summary(ss)

ss_econ = coda.samples(model, c("beta_econ_1", "beta_econ_2", "gamma"), 10000, thin=10)

summary(ss_econ)
dat <- as.matrix(ss_econ)
boxplot(dat[,1:40],use.cols=TRUE, main=expression(paste(hat(beta)[quarter], " for 40 quarters for econ1")),
        xaxt="n")
axis(1,at=1:40, 
labels=c("Jan03","Apr03","Jul03","Oct03",
         "Jan04","Apr04","Jul04","Oct04",
         "Jan05","Apr05","Jul05","Oct05",
         "Jan06","Apr06","Jul06","Oct06",
         "Jan07","Apr07","Jul07","Oct07",
         "Jan08","Apr08","Jul08","Oct08",
         "Jan09","Apr09","Jul09","Oct09",
         "Jan10","Apr10","Jul10","Oct10",
         "Jan11","Apr11","Jul11","Oct11",
         "Jan12","Apr12","Jul12","Oct12")
,cex.axis=0.7, tck=-.01, las=3)
abline(h=0,col="red", pch=2)

boxplot(dat[,c(seq(81,191,by=10),seq(82,192,by=10),
               seq(83,193,by=10),seq(84,194,by=10),
               seq(85,195,by=10),
               seq(86,196,by=10),seq(87,197,by=10),
               seq(88,198,by=10),seq(89,179,by=10),199,
               seq(90,200,by=10))],use.cols=TRUE, main=expression(paste(hat(beta)[quarter], " - ",hat(beta)[other]," for 29 calendar years")))

