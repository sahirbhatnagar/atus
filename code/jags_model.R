##################################
# R source code file used to fit JAGS model
# Created by Maxime, April 29, 2014
# Updated April 29, 2014
# hosted on Github repo 'sahirbhatnagar/atus'
# NOTE:
##################################

#source("~/git_repositories//atus//code//data_cleaning.R")

#On kevisco's laptop ----
setwd("~/atus/data")
load("data.Rda")

library(data.table)
library(bit64)
library(plotrix)
library(rjags)


#Sample subset of survey data----

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
                     'YEAR'=DTS$TUYEAR-2003+1,
                     'MONTH'=DTS$TUMONTH,
                     'RESPONSE'=DTS$TVIND, 
                     'N'=nrow(DTS))
dput(datalist.log, file.path(outpath, "datalog.txt"))

#datalist.gam <- list('DIARYDAY'=DTS$TUDIARYDAY, 
#                     'REGION'=DTS$GEREG, 
#                     'HISPANIC'=DTS$PEHSPNON, 
#                     'SEX'=DTS$TESEX, 
#                     'RACE'=DTS$PTDTRACE, 
#                     'ECON1'=DTS$ECON1,
#                     'ECON2'=DTS$ECON2,
#                     'QUARTER'=DTS$QUARTER,
#                     'RESPONSE'=DTS$TVTIME, 
#                     'N'=nrow(DTS))
#dput(datalist.gam, file.path(outpath, "datagam.txt"))


#Then we need initial values ----

#GAMMA

#initslist.gam <- list('alpha_diary'=c(rep(1,6),NA), 'alpha_region'=c(rep(1,3),NA), 'alpha_hispanic'=1, 
#                      'alpha_sex'=1, 'alpha_race'=c(rep(1,4),NA), 'beta_econ_1'=1, 'beta_econ_2'=2)
#dput(initslist.gam, file.path(outpath, "initsgam.txt"))

#LOGIT autoregressive

initslist.log = list('alpha_diary'=c(rep(1,6),NA), 'alpha_region'=c(rep(1,3),NA), 'alpha_hispanic'=1, 
                     'alpha_sex'=1, 'alpha_race'=c(rep(1,4),NA), 'beta_econ_1'=c(rep(1,40)), 
                     'beta_econ_2'=c(rep(1,40)), 'phi1'=1, 'phi2'=1)

dput(initslist.log, file.path(outpath, "initslog.txt"))

#LOGIT second (new) model

initslist.new = list('alpha_diary'=c(rep(1,6),NA), 'alpha_region'=c(rep(1,3),NA),
                     'alpha_hispanic'=1, 'alpha_sex'=1, 'alpha_race'=c(rep(1,4),NA), 
                     'beta_econ1'= 1, 'beta_econ2'= 1,
                     'phi'=1, 'gamma' = matrix(1,12,10) )

dput(initslist.new, file.path(outpath, "initsnew.txt"))

#LOGIT nonautoregressive

#initslist.non = list('alpha_diary'=c(rep(1,6),NA), 'alpha_region'=c(rep(1,3),NA),
#                     'alpha_hispanic'=1, 'alpha_sex'=1, 'alpha_race'=c(rep(1,4),NA), 
#                     'beta_econ_1'=1, 'beta_econ_2'= 1, 'phi'=1, 'gamma'=c(rep(1,40)))

#dput(initslist.non, file.path(outpath, "initsnon.txt"))

#Models ----

#GAMMA

#model <- jags.model(file.path(inpath, 'model.txt'), data=datalist.gam, 
#                    inits=initslist.gam, n.chains=2, n.adapt=10000, quiet=FALSE)

#ss = coda.samples(model, c("alpha_diary","alpha_region","alpha_hispanic","alpha_sex",
#                           "alpha_race", "beta_econ_1", "beta_econ_2"), 10000, thin=10)

#ss_econ = coda.samples(model, c("beta_econ_1", "beta_econ_2"), 10000, thin=10)


#LOGIT autoregressive

model.log = jags.model(file.path(inpath, 'logit_model.txt'), data=datalist.log, inits=initslist.log,
                       n.chains=2, n.adapt=10000, quiet=FALSE)

#ss_log = coda.samples(model.log, c("alpha_diary","alpha_region","alpha_hispanic","alpha_sex",
#                                   "alpha_race", "beta_econ_1", "beta_econ_2"), 10000, thin=10)

ss_econ_log = coda.samples(model.log, c("beta_econ_1", "beta_econ_2"), 10000, thin=10)

boxplot(as.matrix(ss_econ_log))

save(ss_econ_log, file="autoreg_samples.Rda")

#LOGIT new model

model.new = jags.model(file.path(inpath, 'new_logit_model.txt'), data=datalist.log,
                       inits=initslist.new, n.chains=2, n.adapt=10000, quiet=FALSE)

ss_econ_new = coda.samples(model.new, c("beta_econ1","beta_econ2","gamma"), 10000, thin=10)

boxplot(as.matrix(ss_econ_new))

save(ss_econ_new, file="new_samples.Rda")


#LOGIT new model with no gamma

#model.new.no.gamma = jags.model(file.path(inpath, 'new_logit_model_no_gamma.txt'), data=datalist.log,
#                       inits=initslist.new, n.chains=2, n.adapt=10000, quiet=FALSE)

#ss_econ_new_no_gamma = coda.samples(model.new, c("beta_econ1","beta_econ2","gamma"), 10000, thin=10)

#boxplot(as.matrix(ss_econ_new_no_gamma))

#LOGIT nonautoregressive







