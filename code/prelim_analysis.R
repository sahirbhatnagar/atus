
# Load libraries ----------------------------------------------------------

library(data.table)
#help("data.table")
require(bit64) #for fread function
library(ggplot2)
library(reshape2) # for melt
library(plyr) # for desc function of ordering

setwd("~/git_repositories/atus/data")


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

# The data file atusresp_0312.dat (2003-12 Respondent file) contains 132 
# variables and 136,960 observations.
DTresp <- read.csv("atusresp_0312.dat")
DTresp <- as.data.table(DTresp)

# setkeys
setkey(DTsum, TUCASEID)
setkey(DTresp, TUCASEID)

#merge data
# i think we only need the respondent and summary datasets. the others ones
# dont seem to have anything useful for our purposes
DT <- DTresp[DTsum]


# Summary stats -----------------------------------------------------------

tables()
summary(DT)

#summarising data
DT[, .N, by = TESEX]
DT[, .N, by = TRCHILDNUM]
DT[, .N, by = list(TUYEAR)]

k <- as.data.frame(DT[, mean(t120303), by = list(TUYEAR)])
p1 <- ggplot(k, aes(x = factor(TUYEAR), y = V1))
p1 + geom_bar(stat = "identity")
plot(k$TUYEAR, k$V1, type="l")

DT[ , mean(TRCHILDNUM), by = TESEX]
DT[ , TRCHILDNUM, by = TESEX]
DT[ , j = list(mean=mean(TRCHILDNUM), sd = sd(TRCHILDNUM)), by = TESEX]

#subsetting data
DT[1:10, "TUYEAR" , with=F]
str(DT)

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

p1 <- ggplot(DT, aes(TUDIARYDATE,t120303 ))
p1 + geom_point()

setkey(DT,TUDIARYDATE)
setkey(DT, t120303)
setkey(DT,TUDIARYDATE)


DT[1:10,TUDIARYDATE,]

p <- ggplot(DT, aes(x=t120303, fill=factor(TESEX)))
p  + geom_bar(position="dodge")


p <- ggplot(DT, aes(x = factor(cyl), y = mmpg)) geom_bar(stat = "identity")


p <- ggplot(DT, aes(x=TUDIARYDATE, y=t120303, group=TESEX))
p + geom_line()


p <- ggplot(DT, aes(x = factor(TUMONTH), y = t120303))
p + geom_bar(stat = "identity", position="dodge")


DT[,sum(t120303), by=TUMONTH]

ggplot(DT, aes(t120303)) + geom_bar() +
  facet_wrap(~ TESEX)


ggplot(DT, aes(t120303)) +
  geom_freqpoly(aes(group = TUYEAR, colour = TUYEAR))

