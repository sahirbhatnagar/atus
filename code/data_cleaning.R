##################################
# R source code file used to clean data
# Created by Maxime, March 12, 2014
# Updated May 1st, 2014
# hosted on Github repo 'sahirbhatnagar/atus'
# NOTE:
##################################

# directory where the data is located
setwd("~/Biostats PhD/SSC 2014 Case study/")

#Set directory for kevisco -- comment this out if you need to
#setwd("~/Documents/casestudy")

library(data.table)
library(bit64)
library(plotrix)

#Respondent data
DTresp <- fread("atusresp_0312.dat")
DTresp$TUCASEID <- as.character(DTresp$TUCASEID)

#Activity summary data
DTsum <- fread("atussum_0312.dat")
DTsum$TUCASEID <- as.character(DTresp$TUCASEID)

# http://www.bls.gov/tus/lexiconnoex0312.pdf
# t120303: Television and movies (not religious)
# t120304: Television (religious)

DTsum <- subset(DTsum, select=(colnames(DTsum) %in% c(colnames(DTsum)[1:24], "t120303", "t120304")))

#We don't need all variables in the respondent file, so we removed some of them
sel.var <- read.table("selected_variables.txt", header=TRUE)
sel.var <- sapply(sel.var, as.character)

#ATUS-CPS data
DTcps <- fread("atuscps_0312.dat", select=sel.var)
DTcps$TUCASEID <- as.character(DTcps$TUCASEID)

#This dataset includes more people and we thus retrisct to ATUS resp.
DTcps <- subset(DTcps, DTcps$TUCASEID %in% DTsum$TUCASEID)
DTcps <- subset(DTcps, DTcps$TULINENO==1)

#'%ni%' <- Negate(%in%)
library(Hmisc)

DTsum <- subset(DTsum, select=(colnames(DTsum) %in% sel.var & colnames(DTsum) %nin% colnames(DTcps)[-1]))
DTresp <- subset(DTresp, select=(colnames(DTresp) %in% sel.var & colnames(DTresp) %nin% colnames(DTcps)[-1] & colnames(DTresp) %nin% colnames(DTsum)[-1]))

#to merge
setkey(DTsum, TUCASEID)
setkey(DTresp, TUCASEID)
setkey(DTcps, TUCASEID)

DT <- DTresp[DTsum]
DT <- DT[DTcps]

rm(DTcps, DTsum, DTresp, sel.var)
DT <- subset(DT, select=(colnames(DT) != "TULINENO" ))

#there are two family-income variables
DT$FAMINC <- pmax(DT$HUFAMINC, DT$HEFAMINC)
DT <- subset(DT, select=(colnames(DT) %nin% c("HUFAMINC", "HEFAMINC")))

#there are too many race codes; reducing to five

DT$PTDTRACE <- 1*(DT$PTDTRACE == 1) +
               2*(DT$PTDTRACE %in% c(2,6,10,11,12,15,16,22,23,25,26)) +
               3*(DT$PTDTRACE %in% c(3,7,13,17,24)) +
               4*(DT$PTDTRACE %in% c(4,8,19)) +
               5*(DT$PTDTRACE %in% c(5,9,14,18,20,21))

#there are too many codes for level of education; reducing to five
  
DT$PEEDUCA <- 1*(DT$PEEDUCA %in% 31:34) +
              2*(DT$PEEDUCA %in% 35:38) +
              3*(DT$PEEDUCA %in% 39:42) +
              4*(DT$PEEDUCA %in% 43:44) +
              5*(DT$PEEDUCA %in% 45:46)

#the presence of spouse/unmarried partner should be the same, since
#marital status is encoded somewhere else

DT$TRSPPRES <- 0*(DT$TRSPPRES == 3) +
               1*(DT$TRSPPRES %in% 1:2)

#We should also create a categorical variable for education (HS, college/uni; FT, PT)
#since it's now separated on several variables
#Code: 1: not at school; 2:HS Full-time; 3: HS Part-time
#4: College/university Full-time; 5: College/university Part-time

DT$EDUC <- 1*(DT$TESCHENR %in% c(-1,-3,2)) +
           2*(DT$TESCHFT==1 & DT$TESCHLVL==1) +
           3*(DT$TESCHFT==2 & DT$TESCHLVL==1) +
           4*(DT$TESCHFT==1 & DT$TESCHLVL==2) +
           5*(DT$TESCHFT==2 & DT$TESCHLVL==2)

DT <- subset(DT, select=(colnames(DT) %nin% c("TESCHENR", "TESCHFT", "TESCHLVL")))

#weekly earnings; more than one job; full/part-time; total hours usually worked
#are all dependent of the value of TELFS
#we make them independent by replacing the missing values

DT$TRERNWA  <- as.numeric(DT$TELFS %nin% c(3,4,5))*DT$TRERNWA
DT$TEMJOT   <- as.numeric(DT$TELFS %nin% c(3,4,5))*DT$TEMJOT
DT$TRDPFTPT <- as.numeric(DT$TELFS %nin% c(3,4,5))*DT$TRDPFTPT
DT$TEHRUSLT <- as.numeric(DT$TELFS %nin% c(3,4,5))*DT$TEHRUSLT

#There are still some missing values with these variables
#Since they represent less than 1% of the data
#we will simply randomly assign them a value among the observations

DT$TRERNWA  <- sample(unique(DT$TRERNWA[DT$TRERNWA>-1]), size=nrow(DT), replace=TRUE)*as.numeric(DT$TRERNWA==-1) + 
               DT$TRERNWA*as.numeric(DT$TRERNWA!=-1)
DT$TEMJOT   <- 0*as.numeric(DT$TEMJOT==-1) + 
               DT$TEMJOT*as.numeric(DT$TEMJOT!=-1)
DT$TRDPFTPT <- 0*as.numeric(DT$TRDPFTPT==-1) + 
               DT$TRDPFTPT*as.numeric(DT$TRDPFTPT!=-1)
DT$TEHRUSLT <- sample(unique(DT$TEHRUSLT[DT$TEHRUSLT>-1]), size=nrow(DT), replace=TRUE)*as.numeric(DT$TEHRUSLT %in% c(-4,-1)) + 
               DT$TEHRUSLT*as.numeric(DT$TEHRUSLT %nin% c(-4,-1))

#We then shift TEMJOT and TRDPFTPT so that the baseline is 1

DT$TEMJOT   <- DT$TEMJOT + 1
DT$TRDPFTPT <- DT$TRDPFTPT + 1

#The binary variables are coded 1:YES, 2:NO
#to simplify analysis we code them 1:YES, 0:NO

DT$TRHHCHILD  <- 2-DT$TRHHCHILD
DT$TRNHHCHILD <- 2-DT$TRNHHCHILD
DT$TROHHCHILD <- 2-DT$TROHHCHILD
DT$HETELHHD   <- 2-DT$HETELHHD
DT$PEHSPNON   <- 2-DT$PEHSPNON
DT$TESEX      <- 2-DT$TESEX #1:male; 0:female

#Add the two TV use variables and create an indicator variable for whether
#the respondent watched TV

DT$TVTIME <- DT$t120303 + DT$t120304
DT$TVIND <- as.numeric(DT$TVTIME!=0)

DT <- subset(DT, select=(colnames(DT) %nin% c("t120303", "t120304")))

#Finally, we add the economic variable
source("~/git_repositories/atus.git/code/pca.R")

#On kevisco's laptop
#source("~/atus/code/pca.R")

ECON<-as.data.frame(ECON[,-3])
colnames(ECON) <- c("ECON1", "ECON2")

DT$ECON1 = rep(0, length(DT$TUYEAR))
DT$ECON2 = rep(0, length(DT$TUYEAR))
for(year in 2003:2012){
  
  lag <- 4*(year-2003)
  
  DT$ECON1 <- DT$ECON1 + ECON$ECON1[1+lag]*(DT$TUYEAR==year & DT$TUMONTH %in% 1:3) +
      ECON$ECON1[2+lag]*(DT$TUYEAR==year & DT$TUMONTH %in% 4:6) +
      ECON$ECON1[3+lag]*(DT$TUYEAR==year & DT$TUMONTH %in% 7:9) +
      ECON$ECON1[4+lag]*(DT$TUYEAR==year & DT$TUMONTH %in% 10:12)

  
  DT$ECON2 <- DT$ECON2 + ECON$ECON2[1+lag]*(DT$TUYEAR==year & DT$TUMONTH %in% 1:3) +
    ECON$ECON2[2+lag]*(DT$TUYEAR==year & DT$TUMONTH %in% 4:6) +
    ECON$ECON2[3+lag]*(DT$TUYEAR==year & DT$TUMONTH %in% 7:9) +
    ECON$ECON2[4+lag]*(DT$TUYEAR==year & DT$TUMONTH %in% 10:12)
}

#Including quarter... from 1 to 40
DT$QUARTER = rep(0, length(DT$TUYEAR))
DT$QUARTER = 4*(DT$TUYEAR-2003) + floor((DT$TUMONTH-1)/3) + 1


save(DT, file="data.Rda")

rm(ECON, dow_data, econ_measures, gdp_data, sp500_data, unemployment_data, count, dow_quart, dow_scale, employment_quart, employment_scale, gdp_quart, gdp_scale, pca_econ, sp500_quart, sp500_scale, unemployment_quart, year, i, lag)

