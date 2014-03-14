##################################
# R source code file used to create variables used for ATUS analysis
# Created by Maxime, March 13, 2014
# Updated March 14, 2014
# hosted on Github repo 'sahirbhatnagar/atus'
# NOTE: you need to change your working directory to where the data is
##################################

library(data.table)
library(bit64)
library(plotrix)

#Respondent data
DTresp <- fread("atusresp_0312.dat")
DTresp$TUCASEID <- as.character(DTresp$TUCASEID)

#Activity summary data
DTsum <- fread("atussum_0312.dat")
DTsum$TUCASEID <- as.character(DTresp$TUCASEID)

#to merge
setkey(DTsum, TUCASEID)
setkey(DTresp, TUCASEID)

#variables we decided to include
var.DTsum.cat <- read.table("DTsum_cat.txt")
var.DTsum.cat <- sapply(var.DTsum.cat, as.character)

var.DTsum.cont <- read.table("DTsum_cont.txt")
var.DTsum.cont <- sapply(var.DTsum.cont, as.character)

var.DTresp.cat <- read.table("DTresp_cat.txt")
var.DTresp.cat <- sapply(var.DTresp.cat, as.character)

var.DTresp.cont <- read.table("DTresp_cont.txt")
var.DTresp.cont <- sapply(var.DTresp.cont, as.character)

#We should also create a categorical variable for education (HS, college/uni; FT, PT)
#since it's now separated on several variables

EDUC <- rep(0, times=nrow(DTresp))

for(i in 1:nrow(DTresp)){
if(DTresp$TESCHENR[i]==-1) EDUC[i]= 0
if(DTresp$TESCHENR[i]==2) EDUC[i] = 0
if(DTresp$TESCHENR[i]==1) EDUC[i] = DTresp$TESCHFT[i]+DTresp$TESCHLVL[i]-1
}

#let's set up the covariate matrices. we'll have two: continuous and categorical
DT.cont <- subset(DTresp, select=(names(DTresp) %in% var.DTresp.cont))
DT.cont <- merge(DT.cont, subset(DTsum, select=(names(DTsum) %in% var.DTsum.cont)), by="TUCASEID")

DT.cat <- subset(DTresp, select=(names(DTresp) %in% var.DTresp.cat))
DT.cat <- merge(DT.cat, subset(DTsum, select=(names(DTsum) %in% var.DTsum.cat)), by="TUCASEID")
DT.cat[,EDUC:=EDUC]

#convert categorical data into factors
cat.names <- names(DT.cat)[-1]
DT.cat[,(cat.names):=lapply(.SD, as.factor),.SDcols=cat.names]

#data table of the response and id, sum the normal and religious tv times, add indicator variable
DT.y <- DTsum[,list(TUCASEID, TVtime=t120303+t120304)][,TVind:=as.numeric(TVtime!=0)]

#combine categorical and continuous data and response data
DT <- Reduce(merge,list(DT.y,DT.cat,DT.cont))
