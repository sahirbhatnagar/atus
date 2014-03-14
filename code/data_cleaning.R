library(data.table)
library(bit64)
library(plotrix)

#Respondent data
DTresp <- fread("atusresp_0312/atusresp_0312.dat")
DTresp$TUCASEID <- as.character(DTresp$TUCASEID)

#Activity summary data
DTsum <- fread("atussum_0312/atussum_0312.dat")
DTsum$TUCASEID <- as.character(DTresp$TUCASEID)

#to merge
setkey(DTsum, TUCASEID)
setkey(DTresp, TUCASEID)

#We don't need all variables in the respondent file, so we removed some of them
removed.var <- read.table("removed_variables.txt", header=TRUE)
removed.var <- sapply(removed.var, as.character)

`%ni%` <- Negate(`%in%`) 
not.needed <- names(DTresp) %ni% removed.var
DTresp.rm <- subset(DTresp, select=not.needed)

#Looking at missing values
missing.data<-apply(DTresp.rm, 2, function(X) sum(X<0))
missing.data<-missing.data[missing.data>100000]
missing.data
sort(apply(DTresp.rm, 2, function(X) sum(X<0)))

#Doing some manual imputation...

#variables related to child care
child.var <- names(apply(DTresp.rm, 2, function(X) sum(X<0))[apply(DTresp.rm, 2, function(X) sum(X<0))==20720])
child.col <- DTresp.rm[,names(DTresp.rm) %in% child.var, with=F]


for(k in 1:nrow(DTresp)){
  
  if(DTresp$TRCHILDNUM[k]==0) set(child.col, i=k, j=names(child.col), value=0)
  
}

DTresp.rm[, names(child.col):= child.col]
