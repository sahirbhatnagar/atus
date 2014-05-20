##################################
# R source code file used to fit gamma_model.txt for TVTIME > 0
# Created by Sy, May 3rd, 2014
# Updated May 6th, 2014
# hosted on Github repo 'sahirbhatnagar/atus'
# NOTE:
##################################

#source("~/git_repositories//atus//code//data_cleaning.R")

#On kevisco's laptop
# setwd("~/atus/data")
# On Sy's laptop
setwd("~//git_repositories/atus/data/")
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

xtabs(~DT$TUYEAR)

#DT with non zero indicators 
DT <- DT[TVIND!=0]
#Sample subset of survey data
set.seed(1234)
prop <- 0.10
DTS <- DT[sample(x=1:nrow(DT), size=prop*nrow(DT)), , ]
xtabs(~DTS$QUARTER)

#We first create the datafiles for JAGS
#sy's desktop
 outpath <- "~//git_repositories//atus.git//data"
 inpath <- "~//git_repositories//atus.git//code"

#sy's laptop
outpath <- "~//git_repositories//atus//data"
inpath <- "~//git_repositories//atus//code"


datalist.gam <- list('DIARYDAY'=DTS$TUDIARYDAY, 
                     'REGION'=DTS$GEREG, 
                     'HISPANIC'=DTS$PEHSPNON, 
                     'SEX'=DTS$TESEX, 
                     'RACE'=DTS$PTDTRACE, 
                     'ECON1'=DTS$ECON1,
                     'ECON2'=DTS$ECON2,
                     'YEAR'=DTS$TUYEAR-2003+1,
                     'MONTH'=DTS$TUMONTH,
                     'RESPONSE'=DTS$TVTIME, 
                     'N'=nrow(DTS))
dput(datalist.gam, file.path(outpath, "datagam.txt"))

# Then we need initial values (technically we dont need them, and since we 
# dont have empirical estimates, so we let JAGS decide)
# initslist.gam <- list('alpha_diary'=c(rep(1,6),NA), 'alpha_region'=c(rep(1,3),NA), 
#                       'alpha_hispanic'=1, 'alpha_sex'=1, 'alpha_race'=c(rep(1,4),NA), 
#                       'beta_econ_1'=1, 'beta_econ_2'=1)
# dput(initslist.gam, file.path(outpath, "initsgam.txt"))

# Models

# Gamma model with smooth time trend and 1 beta for econ1 1 beta for econ 2 ----------------------------------

model <- jags.model(file.path(inpath, 'testing'), data=datalist.gam, 
                    n.chains=2, n.adapt=10000, quiet=FALSE)

# ss = coda.samples(model, c("alpha_diary","alpha_region","alpha_hispanic","alpha_sex",
#                            "alpha_race", "beta_econ_1", "beta_econ_2"), 10000, thin=10)
#summary(ss)

ss_econ = coda.samples(model, c("beta_econ_1", "beta_econ_2", "gamma", "logRR_time"), 10000, thin=10)
ss_econ2 = coda.samples(model, c("shape", "rate"), 10000, thin=10)

dat2 <- as.matrix(ss_econ2)
dat2[1:5,10940:10945]
dim(dat2)

load("gamma_model_1.RData")
summary(ss_econ)
dat <- as.matrix(ss_econ)

#time trend
boxplot(dat[,3:122],use.cols=TRUE, 
        main=expression(paste(hat(gamma)[month_year])),xaxt="n")
axis(1,at=seq(1,120,by=3), 
     labels=c("Jan03","Apr03","Jul03","Oct03",
              "Jan04","Apr04","Jul04","Oct04",
              "Jan05","Apr05","Jul05","Oct05",
              "Jan06","Apr06","Jul06","Oct06",
              "Jan07","Apr07","Jul07","Oct07",
              "Jan08","Apr08","Jul08","Oct08",
              "Jan09","Apr09","Jul09","Oct09",
              "Jan10","Apr10","Jul10","Oct10",
              "Jan11","Apr11","Jul11","Oct11",
              "Jan12","Apr12","Jul12","Oct12")
     ,cex.axis=0.7, tck=-.01, las=3)
abline(h=0,col="red", pch=10)

#logRR time trend
boxplot(dat[,123:242],use.cols=TRUE, 
main=expression(paste(hat(gamma)[month_year]-hat(gamma)["jan03"])),xaxt="n")
axis(1,at=seq(1,120,by=3), 
     labels=c("Jan03","Apr03","Jul03","Oct03",
              "Jan04","Apr04","Jul04","Oct04",
              "Jan05","Apr05","Jul05","Oct05",
              "Jan06","Apr06","Jul06","Oct06",
              "Jan07","Apr07","Jul07","Oct07",
              "Jan08","Apr08","Jul08","Oct08",
              "Jan09","Apr09","Jul09","Oct09",
              "Jan10","Apr10","Jul10","Oct10",
              "Jan11","Apr11","Jul11","Oct11",
              "Jan12","Apr12","Jul12","Oct12")
     ,cex.axis=0.7, tck=-.01, las=3)
abline(h=0,col="red", lwd=3)

rm(dat2)
plot(density(dat[,2]))
pairs(dat[,1:2])

load("~/Dropbox/PhD/SSC case study/gamma1_no_interaction.RData")

save.image(file="~/Dropbox/PhD/SSC case study/gamma1_no_interaction.RData")
getwd()

mcmcChain = as.matrix( ss_econ )
chainLength = NROW(mcmcChain)

source("~/git_repositories/atus/code/plotPost.R")

#windows(width=10,height=3)
#layout( matrix(1:3,nrow=1) )
dev.off()
y <- DTS$TVTIME
# Data with superimposed posterior predictive gamma's:
hist( y , xlab="y"  , main="Data" , col="grey" , prob=T )
xComb=seq(min(y),max(y),length=501)
nPPC = 15
for ( idx in round(seq(1,chainLength,length=nPPC)) ) {
  if ( mType=="Mean" ) {
    m = mcmcChain[idx,"m"]
    sd = mcmcChain[idx,"sd"]
    sh = m^2 / sd^2
    ra = m   / sd^2
  }
  if ( mType=="Mode" ) {
    m = mcmcChain[idx,"m"]
    sd = mcmcChain[idx,"sd"]
    ra = ( m + sqrt( m^2 + 4*sd^2 ) ) / ( 2 * sd^2 )
    sh = 1 + m * ra
  }
  lines( xComb , dgamma( xComb , sh , ra ) , col="skyblue" , lwd=2 ) 
}
# Posterior estimates of parameters:
par( mar=c(3,1,2.5,0) , mgp=c(2,0.7,0) )
plotPost( mcmcChain[,"beta_econ_1"] , xlab="mType" , main="Posterior Est." , showMode=T )
plotPost( mcmcChain[,"beta_econ_2"] , xlab="sd" , main="Posterior Est." , showMode=T )
savePlot(file=paste(fileNameRoot,mType,sep=""),type="jpg")

xComb=seq(min(y),max(y),length=501)
hist( y , xlab="y"  , main="Data" , col="grey" , prob=T )

for (j in 1:10){
lines( xComb , dgamma( xComb , shape=mcmcChain[1,10941+j] , rate=mcmcChain[1,j] ) , col="red" , lwd=2 ) 
}
lines( xComb , dgamma( xComb , shape=mcmcChain[1,"shape[2]"] , rate=mcmcChain[1,"rate[2]"] ) , col="red" , lwd=2 ) 

mcmcChain[1:5,1:2]


qgamma(y[1],shape=mcmcChain[1,"shape[1]"], rate=mcmcChain[1,"rate[1]"])




# Gamma model with smooth time trend ----------------------------------

model_smooth <- jags.model(file.path(inpath, 'gamma_model_2.txt'), data=datalist.gam, 
                    n.chains=2, n.adapt=100, quiet=FALSE)

# ss = coda.samples(model, c("alpha_diary","alpha_region","alpha_hispanic","alpha_sex",
#                            "alpha_race", "beta_econ_1", "beta_econ_2"), 10000, thin=10)
#summary(ss)

ss_econ_smooth = coda.samples(model_smooth, c("beta_econ_1", "beta_econ_2", "gamma"), 10000, thin=10)


save.image(file="~/Dropbox/PhD/gamma_model_2.RData")

load("~/Dropbox/PhD/gamma_model_2.RData")
summary(ss_econ_smooth)
dat <- as.matrix(ss_econ_smooth)
#econ1
boxplot(dat[,1:40],use.cols=TRUE, main=expression(paste(hat(beta)[quarter], " for 40 quarters for econ1")),
        xaxt="n")
axis(1,at=1:40, 
     labels=c("Jan03","Apr03","Jul03","Oct03",
              "Jan04","Apr04","Jul04","Oct04",
              "Jan05","Apr05","Jul05","Oct05",
              "Jan06","Apr06","Jul06","Oct06",
              "Jan07","Apr07","Jul07","Oct07",
              "Jan08","Apr08","Jul08","Oct08",
              "Jan09","Apr09","Jul09","Oct09",
              "Jan10","Apr10","Jul10","Oct10",
              "Jan11","Apr11","Jul11","Oct11",
              "Jan12","Apr12","Jul12","Oct12")
     ,cex.axis=0.7, tck=-.01, las=3)
abline(h=0,col="red", pch=10)


#econ2
boxplot(dat[,41:80],use.cols=TRUE, main=expression(paste(hat(beta)[quarter], " for 40 quarters for econ2")),
        xaxt="n")
axis(1,at=1:40, 
     labels=c("Jan03","Apr03","Jul03","Oct03",
              "Jan04","Apr04","Jul04","Oct04",
              "Jan05","Apr05","Jul05","Oct05",
              "Jan06","Apr06","Jul06","Oct06",
              "Jan07","Apr07","Jul07","Oct07",
              "Jan08","Apr08","Jul08","Oct08",
              "Jan09","Apr09","Jul09","Oct09",
              "Jan10","Apr10","Jul10","Oct10",
              "Jan11","Apr11","Jul11","Oct11",
              "Jan12","Apr12","Jul12","Oct12")
     ,cex.axis=0.7, tck=-.01, las=3)
abline(h=0,col="red", pch=10)



#time trend
boxplot(dat[,81:200],use.cols=TRUE, 
        main=expression(paste(hat(gamma)[month_year], " for month,year")),xaxt="n")
axis(1,at=seq(1,120,by=3), 
     labels=c("Jan03","Apr03","Jul03","Oct03",
              "Jan04","Apr04","Jul04","Oct04",
              "Jan05","Apr05","Jul05","Oct05",
              "Jan06","Apr06","Jul06","Oct06",
              "Jan07","Apr07","Jul07","Oct07",
              "Jan08","Apr08","Jul08","Oct08",
              "Jan09","Apr09","Jul09","Oct09",
              "Jan10","Apr10","Jul10","Oct10",
              "Jan11","Apr11","Jul11","Oct11",
              "Jan12","Apr12","Jul12","Oct12")
     ,cex.axis=0.7, tck=-.01, las=3)
abline(h=0,col="red", pch=10)


save.image(file="gamma_model_2.RData")


# Sex Economy interaction -------------------------------------------------

model <- jags.model(file.path(inpath, 'gamma_econ_sex_model'), data=datalist.gam, 
                    n.chains=2, n.adapt=3000, quiet=FALSE)

ss_econ = coda.samples(model, c("beta_econ_1", "beta_econ_2", "logRR_time","beta_econ_1_sex",
                                "beta_econ_2_sex","P.res","P.res.new","C.res","resid","fit",
                                "fit.new","chisqp"), 10000, thin=10)



#time trend
boxplot(mcmcChain[,-1:-10945],use.cols=TRUE, 
        main=expression(paste(hat(gamma)[month_year], " for month,year")),xaxt="n")
axis(1,at=seq(1,120,by=3), 
     labels=c("Jan03","Apr03","Jul03","Oct03",
              "Jan04","Apr04","Jul04","Oct04",
              "Jan05","Apr05","Jul05","Oct05",
              "Jan06","Apr06","Jul06","Oct06",
              "Jan07","Apr07","Jul07","Oct07",
              "Jan08","Apr08","Jul08","Oct08",
              "Jan09","Apr09","Jul09","Oct09",
              "Jan10","Apr10","Jul10","Oct10",
              "Jan11","Apr11","Jul11","Oct11",
              "Jan12","Apr12","Jul12","Oct12")
     ,cex.axis=0.7, tck=-.01, las=3)
abline(h=0,col="red", pch=10)



# Econ Race interaction --------------------------------------------------------

load("~/Dropbox/PhD/SSC case study/gamma3_race_interaction.RData")

model <- jags.model(file.path(inpath, 'testing'), data=datalist.gam, 
                    n.chains=2, n.adapt=2000, quiet=FALSE)

ss_econ = coda.samples(model, c("beta_econ_1", "beta_econ_2", "logRR_time","beta_econ_race_1",
                                "beta_econ_race_2","P.res","P.res.new","C.res","resid","fit",
                                "fit.new","chisqp"), 10000, thin=10)

plot(ss_econ, ask=TRUE)



save.image(file="~/Dropbox/PhD/SSC case study/gamma3_race_interaction.RData")





# Observed vs expected -------------------------------------------------------


summary(ss_econ)
mcmcChain <- as.matrix(ss_econ)

head(mcmcChain[,1097:1098])
head(mcmcChain)
fitted <- mcmcChain[,1219:2312]
mean.fitted <- apply(fitted,2,mean)


plot(mean.fitted,DTS$TVTIME)


plot(mcmcChain[,"fit"], mcmcChain[,"fit.new"], main="Posterior predictive check \nfor sum of squared Pearson residuals", xlab="Discrepancy measure for actual data set", ylab="Discrepancy measure for perfect data sets", col="blue")
abline(0, 1, lwd=2, col = "black")

mean(mcmcChain[,"fit.new"]>mcmcChain[,"fit"])
hist(mcmcChain[,"fit"])


sim <- apply(mcmcChain[,1:1094],2,mean)



plot(pred.m$value,DTS$TVTIME)
plot(density(DTS$TVTIME))
lines(density(pred.m$value))

head(mcmcChain[,1097:1216])

boxplot(mcmcChain[,1097:1216],use.cols=TRUE, 
        main=expression(paste(hat(gamma)[month_year], " for month,year")),xaxt="n")

plot(ss_econ, ask=TRUE)




# Final Plots -------------------------------------------------------------

model <- jags.model(file.path(inpath, 'testing'), data=datalist.gam, 
                    n.chains=2, n.adapt=1000, quiet=FALSE)

ss_econ = coda.samples(model, c("chisq.p"), 1000, thin=10)

summary(ss_econ)
mcmcChain <- as.matrix(ss_econ)
head(mcmcChain[,1:5])


fitted <- mcmcChain[,1:1094]
residuals <- mcmcChain[,1095:2188]
fitted.m <- melt(fitted)
residuals.m <- melt(residuals)

plot(apply(fitted,2,mean),apply(residuals,2,mean))
hist(residuals.m$value)






mean.resid <- apply(pred,2,mean)
plot(DTS$ECON1,mean.resid)
plot(DTS$ECON2,mean.resid)
abline(h=0,col="red", pch=10)


# Observed vs fitted values -----------------------------------------------

library(reshape2)
library(ggplot2)

pred <- mcmcChain[,1:1094]
pred.m <- melt(pred)

m <- ggplot(DTS, aes(x=TVTIME))
m + geom_histogram(binwidth=25)+aes(y=..density..) + 
  geom_density(data=pred.m, aes(x=value), size=2)

