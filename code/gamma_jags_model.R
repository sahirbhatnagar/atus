##################################
# R source code file used to fit gamma_model.txt for TVTIME > 0
# Created by Sy, May 3rd, 2014
# Updated April 3rd, 2014
# hosted on Github repo 'sahirbhatnagar/atus'
# NOTE:
##################################

#source("~/git_repositories//atus//code//data_cleaning.R")

#On kevisco's laptop
# setwd("~/atus/data")
# On Sy's laptop
#setwd("~//git_repositories/atus/data/")
# On Sy's desktop
setwd("~//git_repositories//atus.git//data/")
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

#DT with non zero indicators 
DT <- DT[TVIND!=0]
#Sample subset of survey data
set.seed(1234)
prop <- 0.1  
DTS <- DT[sample(x=1:nrow(DT), size=prop*nrow(DT)), , ]


#We first create the datafiles for JAGS

outpath <- "~//git_repositories//atus.git//data"
inpath <- "~//git_repositories//atus.git//code"

datalist.gam <- list('DIARYDAY'=DTS$TUDIARYDAY, 
                     'REGION'=DTS$GEREG, 
                     'HISPANIC'=DTS$PEHSPNON, 
                     'SEX'=DTS$TESEX, 
                     'RACE'=DTS$PTDTRACE, 
                     'ECON1'=DTS$ECON1,
                     'ECON2'=DTS$ECON2,
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

ss = coda.samples(model, c("alpha_diary","alpha_region","alpha_hispanic","alpha_sex",
                           "alpha_race", "beta_econ_1", "beta_econ_2"), 10000, thin=10)

ss_econ = coda.samples(model, c("beta_econ_1", "beta_econ_2"), 10000, thin=10)

summary(ss)

summary(ss_econ)
