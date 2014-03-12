# Load libraries ----------------------------------------------------------

library(data.table)
#help("data.table")
library(ggplot2)
library(reshape2) # for melt
library(plyr) # for desc function of ordering

setwd("~/git_repositories/atus.git/data")
load("atusfinalDT.RData")

# Import data -------------------------------------------------------------

# The data file atusact_0312.dat (2003-12 Activity file) contains 29 variables 
# and 2,693,839 observations. 
#DTact <- fread("atusact_0312.dat")

#DTcps <- fread("atuscps_0312.dat")

#DTrost <- read.csv("atusrost_0312.dat")
#DTrost <- as.data.table(DTrost)

# The file atussum_0312.dat (2003-12 Activity Summary file) contains 455 
# variables and 136,960 observations. The 2003-12 Activity Summary file 
# contains data for the total number of minutes that each respondent spent 
# doing each 6-digit activity. Each record corresponds to a unique respondent,
# as indicated by a unique value of the variable TUCASEID.
DTsum <- read.csv("atussum_0312.dat")
DTsum <- as.data.table(DTsum)
DTsum$TUCASEID <- as.character(DTsum$TUCASEID)

# The data file atusresp_0312.dat (2003-12 Respondent file) contains 132 
# variables and 136,960 observations.
DTresp <- read.csv("atusresp_0312.dat")
DTresp <- as.data.table(DTresp)
DTresp$TUCASEID <- as.character(DTresp$TUCASEID)


# setkeys
setkey(DTsum, TUCASEID)
setkey(DTresp, TUCASEID)

#merge data
# i think we only need the respondent and summary datasets. the others ones
# dont seem to have anything useful for our purposes
DT <- DTresp[DTsum]

# reduced dataset to work with ggplot2 (full dataset crashes ggplot2 if I 
# make a coding error when using ggplot2)
set.seed(1234)
DTred <- DT[sample(x=1:nrow(DT), size=0.10*nrow(DT)), , ]

# Summary stats -----------------------------------------------------------

#summarising data
DT[, .N, by = TESEX]
DT[, .N, by = TRCHILDNUM]
DT[, .N, by = list(TUYEAR, TUMONTH)]
DT[ , mean(TRCHILDNUM), by = TESEX]
DT[ , TRCHILDNUM, by = TESEX]
DT[ , j = list(mean=mean(TRCHILDNUM), sd = sd(TRCHILDNUM)), by = TESEX]

#subsetting data
DT[1:10, "TUYEAR" , with=F]

# Watching TV stats -------------------------------------------------------

# http://www.bls.gov/tus/lexiconnoex0312.pdf
# t120303: Television and movies (not religious)
# t120304: Television (religious)
# t120307: Playing games
# t120308: Computer use for leisure (exc. Games)

DT[, mean(t120303), by = TESEX]
DT[, mean(t120304), by = TESEX]
DT[, mean(t120307), by = TESEX]
DT[, mean(t120308), by = TESEX]

DT[, mean(t120308), by = list(TUYEAR, TUMONTH)]


# Exploratory Plots -------------------------------------------------------

#TESEX=2 is women (see ATUSUser guide)

# Gender and number of children -------------------------------------------

p1 <- ggplot(DT,aes(x=TRCHILDNUM,fill=factor(TESEX)))
cols <- c("1" = "blue","2" = "red")
p1 + geom_bar(position = "dodge") + 
  scale_fill_manual(values = cols, labels=c("men", "women")) +
    guides(fill=guide_legend(title="gender"))


# Gender and Race --------------------------------------
p1 <- ggplot(DT,aes(x=PTDTRACE,fill=factor(TESEX)))
cols <- c("1" = "blue","2" = "red")
p1 + geom_bar(position = "dodge") +
  scale_fill_manual(values = cols, labels=c("men", "women")) +
    guides(fill=guide_legend(title="gender"))


# TV and time by gender ---------------------------------------------------

p <- ggplot(DTred, aes(x=t120303, fill=factor(TESEX)))
p  + geom_bar(position="dodge")

p <- ggplot(DTred, aes(x = factor(TUMONTH), y = t120303))
p + geom_bar(stat = "identity", position="dodge")

k <- ggplot(DTred, aes(t120303, fill=factor(TUYEAR)))
k + geom_bar() + xlim(0,600) 

d <- ggplot(DTred, aes(x = TUYEAR, y = TUMONTH))
d+ stat_sum() + scale_y_continuous(breaks=seq(1, 12, 1)) +
  scale_x_continuous(breaks=seq(2003, 2012, 1)) 

m <- ggplot(DT, aes(x = TUYEAR))
m + geom_density()+scale_x_continuous(breaks=seq(2003, 2012, 1)) 


m <- ggplot(DTred, aes(x = t120303))
m + geom_density(aes(fill=factor(TESEX)), size=2) + xlim(0,400)

qplot(t120303, ..count.., data=DTred, geom="density", fill=factor(TUYEAR), position="stack")+xlim(0,500)

qplot(t120303, ..density.., data=DTred, geom="density", fill=factor(TUYEAR), position="stack")+xlim(0,500)
