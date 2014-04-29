##################################
# R source code file used to clean data
# Created by Maxime, March 12, 2014
# Updated April 29, 2014
# hosted on Github repo 'sahirbhatnagar/atus'
# NOTE:
##################################

library(data.table)
library(bit64)
library(plotrix)

#Respondent data
DTresp <- fread("atusresp_0312/atusresp_0312.dat")
DTresp$TUCASEID <- as.character(DTresp$TUCASEID)

#Activity summary data
DTsum <- fread("atussum_0312/atussum_0312.dat")
DTsum$TUCASEID <- as.character(DTresp$TUCASEID)

# http://www.bls.gov/tus/lexiconnoex0312.pdf
# t120303: Television and movies (not religious)
# t120304: Television (religious)

DTsum <- subset(DTsum, select=(colnames(DTsum) %in% c(colnames(DTsum)[1:24], "t120303", "t120304")))

#ATUS-CPS data
DTcps <- fread("atuscps_0312.dat")
DTcps$TUCASEID <- as.character(DTcps$TUCASEID)

#This dataset includes more people and we thus retrisct to ATUS resp.
DTcps <- subset(DTcps, DTcps$TUCASEID %in% DT$TUCASEID)
DTcps <- subset(DTcps, DTcps$TULINENO==1)


#We don't need all variables in the respondent file, so we removed some of them
sel.var <- read.table("selected_variables.txt", header=TRUE)
sel.var <- sapply(sel.var, as.character)

'%ni%' <- Negate(%in%)

DTcps <- subset(DTcps, select=(colnames(DTcps) %in% sel.var))
DTsum <- subset(DTsum, select=(colnames(DTsum) %in% sel.var & colnames(DTsum) %ni% colnames(DTcps)[-1]))
DTresp <- subset(DTresp, select=(colnames(DTresp) %in% sel.var & colnames(DTresp) %ni% colnames(DTcps)[-1] & colnames(DTresp) %ni% colnames(DTsum)[-1]))

#to merge
setkey(DTsum, TUCASEID)
setkey(DTresp, TUCASEID)
setkey(DTcps, TUCASEID)

DT <- DTresp[DTsum]
DT <- DT[DTcps]

#there are two family-income variables
DT$FAMINC <- pmax(DT$HUFAMINC, DT$HEFAMINC)
DT <- subset(DT, select=(colnames(DT) %ni% c("HUFAMINC", "HEFAMINC")))

#We will run the following loops in parallel
library(doParallel)
library(foreach)
library(plyr)
cl<-makeCluster(2)
registerDoParallel(cl) # register these 2 cores with the "foreach" package

#there are too many race codes; reducing to five

foreach(i=1:nrow(DT)) %dopar% {
  
  if(DT$PTDTRACE[i] %in% c(6,10,11,12,15,16,22,23,25,26)) DT$PTDTRACE[i]=2
  if(DT$PTDTRACE[i] %in% c(7,13,17,24))                   DT$PTDTRACE[i]=3
  if(DT$PTDTRACE[i] %in% c(8,19))                         DT$PTDTRACE[i]=4
  if(DT$PTDTRACE[i] %in% c(9,14,18,20,21))                DT$PTDTRACE[i]=5
  
}

#there are too many codes for level of education; reducing to five

foreach(i=1:nrow(DT)) %dopar% {
  
  if(DT$PEEDUCA[i] %in% 31:34) DT$PEEDUCA[i]=0
  if(DT$PEEDUCA[i] %in% 35:38) DT$PEEDUCA[i]=1
  if(DT$PEEDUCA[i] %in% 39:42) DT$PEEDUCA[i]=2
  if(DT$PEEDUCA[i] %in% 43:44) DT$PEEDUCA[i]=3
  if(DT$PEEDUCA[i] %in% 45:46) DT$PEEDUCA[i]=4
  
}

#the presence of spouse/unmarried partner should be the same, since
#marital status is encoded somewhere else

foreach(i=1:nrow(DT)) %dopar% {
  
  if(DT$TRSPPRES[i] == 3)     DT$TRSPPRES[i]=0
  if(DT$TRSPPRES[i] %in% 1:2) DT$TRSPPRES[i]=1
  
}

#We should also create a categorical variable for education (HS, college/uni; FT, PT)
#since it's now separated on several variables
#Code: 0: not at school; 1:HS Full-time; 2: HS Part-time
#3: College/university Full-time; 4: College/university Part-time

DT$EDUC <- rep(0, times=nrow(DTresp))

foreach(i=1:nrow(DT)) %dopar% {
  
  if(DT$TESCHENR[i] %in% c(-1,2)) DT$EDUC[i]= 0
  if(DT$TESCHENR[i]==1)           DT$EDUC[i] = DT$TESCHFT[i]+2*DT$TESCHLVL[i]-2
}

DT <- subset(DT, select=(colnames(DT) %ni% c("TESCHENR", "TESCHFT", "TESCHLVL")))

#weekly earnings; more than one job; full/part-time; total hours usually worked
#are all dependent of the value of TELFS
#we make them independent by replacing the missing values

foreach(i=1:nrow(DT)) %dopar% {
  
  if(DT$TELFS[i] %in% c(3,4,5)){
    
    DT$TRERNWA[i] =0
    DT$TEMJOT[i]  =0
    DT$TRDPFTPT[i]=0
    DT$TEHRUSLT[i]=0
  }
  
}

#The binary variables are coded 1:YES, 2:NO
#to simplify analysis we code them 1:YES, 0:NO

DT$TRHHCHILD  <- 2-DT$TRHHCHILD
DT$TRNHHCHILD <- 2-DT$TRNHHCHILD
DT$TROHHCHILD <- 2-DT$TROHHCHILD
DT$HETELHHD   <- 2-DT$HETELHHD
DT$PEAFNOW    <- 2-DT$PEAFNOW
DT$PEHSPNON   <- 2-DT$PEHSPNON
DT$TESEX      <- 2-DT$TESEX #1:male; 0:female

#Add the two TV use variables and create an indicator variable for whether
#the respondent watched TV

DT$TVTIME <- DT$t120303 + DT$t120304
DT$TVIND <- as.numeric(DT$TVTIME!=0)

DT <- subset(DT, select=(colnames(DT) %ni% c("t120303", "t120304")))

save(DT, file="data.Rda")