##################################
# R source code file used to fit JAGS model
# Created by Maxime, April 29, 2014
# Updated April 29, 2014
# hosted on Github repo 'sahirbhatnagar/atus'
# NOTE:
##################################

source("~/git_repositories//atus//code//data_cleaning.R")

library(data.table)
library(bit64)
library(plotrix)
library(rjags)

#We first create the datafiles for JAGS

outpath <- "~/git_repositories/atus/data"

datalist.log <- list('DIARYDAY'=DT$TUDIARYDAY, 
                     'REGION'=DT$GEREG, 
                     'HISPANIC'=DT$PEHSPNON, 
                     'SEX'=DT$TESEX, 
                     'RACE'=DT$PTDTRACE, 
                     'ECON1'=DT$ECON1,
                     'ECON2'=DT$ECON2,
                     'RESPONSE'=DT$TVIND, 
                     'N'=nrow(DT))
dput(datalist.log, file.path(outpath, "datalog.txt"))

datalist.gam <- list('DIARYDAY'=DT$TUDIARYDAY, 
                     'REGION'=DT$GEREG, 
                     'HISPANIC'=DT$PEHSPNON, 
                     'SEX'=DT$TESEX, 
                     'RACE'=DT$PTDTRACE, 
                     'ECON1'=DT$ECON1,
                     'ECON2'=DT$ECON2,
                     'RESPONSE'=DT$TVTIME, 
                     'N'=nrow(DT))
dput(datalist.gam, file.path(outpath, "datagam.txt"))

#Then we need initial values
