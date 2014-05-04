##################################
# R source code file used to fit JAGS model
# Created by Maxime, April 29, 2014
# Updated April 29, 2014
# hosted on Github repo 'sahirbhatnagar/atus'
# NOTE:
##################################

#source("~/git_repositories//atus//code//data_cleaning.R")

#On kevisco's laptop
setwd("~/atus/data")
load("data.Rda")

library(data.table)
library(bit64)
library(plotrix)
library(rjags)


#Sample subset of survey data

prop = 0.1  #Proportion of DT to sample
samp = sample(1:length(DT$TUCASEID), floor(prop*length(DT$TUCASEID)))
DTS = DT[samp]  #Our sample

#We first create the datafiles for JAGS

outpath <- "~/atus/data"
inpath <- "~/atus/code"

datalist.log <- list('DIARYDAY'=DTS$TUDIARYDAY, 
                     'REGION'=DTS$GEREG, 
                     'HISPANIC'=DTS$PEHSPNON, 
                     'SEX'=DTS$TESEX, 
                     'RACE'=DTS$PTDTRACE, 
                     'ECON1'=DTS$ECON1,
                     'ECON2'=DTS$ECON2,
                     'QUARTER'=DTS$QUARTER,
                     'RESPONSE'=DTS$TVIND, 
                     'N'=nrow(DTS))
dput(datalist.log, file.path(outpath, "datalog.txt"))

datalist.gam <- list('DIARYDAY'=DTS$TUDIARYDAY, 
                     'REGION'=DTS$GEREG, 
                     'HISPANIC'=DTS$PEHSPNON, 
                     'SEX'=DTS$TESEX, 
                     'RACE'=DTS$PTDTRACE, 
                     'ECON1'=DTS$ECON1,
                     'ECON2'=DTS$ECON2,
                     'QUARTER'=DTS$QUARTER,
                     'RESPONSE'=DTS$TVTIME, 
                     'N'=nrow(DTS))
dput(datalist.gam, file.path(outpath, "datagam.txt"))


#Then we need initial values

initslist.gam <- list('alpha_diary'=c(rep(1,6),NA), 'alpha_region'=c(rep(1,3),NA), 'alpha_hispanic'=1, 
                      'alpha_sex'=1, 'alpha_race'=c(rep(1,4),NA), 'beta_econ_1'=1, 'beta_econ_2'=2)
dput(initslist.gam, file.path(outpath, "initsgam.txt"))

#Models

model <- jags.model(file.path(inpath, 'model.txt'), data=datalist.gam, 
                    inits=initslist.gam, n.chains=2, n.adapt=10000, quiet=FALSE)

ss = coda.samples(model, c("alpha_diary","alpha_region","alpha_hispanic","alpha_sex",
                           "alpha_race", "beta_econ_1", "beta_econ_2"), 10000, thin=10)

ss_econ = coda.samples(model, c("beta_econ_1", "beta_econ_2"), 10000, thin=10)

save(model, "")
