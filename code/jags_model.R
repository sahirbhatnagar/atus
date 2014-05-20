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
library(Hmisc)


#Sample subset of survey data----

prop = 0.1  #Proportion of DT to sample
samp = sample(1:length(DT$TUCASEID), floor(prop*length(DT$TUCASEID)))
DTS = DT[samp]  #Our sample

#Entering historical consumer price index to adjust weekly earnings variable. 2003-2012
cpi = c(181,185,190,198,202,211,211,216,220,226)

#Income adjusted for inflation
sampinc = DTS$TRERNWA * cpi[1]/cpi[DTS$TUYEAR-2003+1]


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
                     'INCOME'=sampinc,
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

#LOGIT model with interaction (SEX)

initslist.int = list('alpha_diary'=c(rep(1,6),NA), 'alpha_region'=c(rep(1,3),NA),
                     'alpha_hispanic'=1, 'alpha_sex'=1, 'alpha_race'=c(rep(1,4),NA), 
                     'beta_econ1'= 1, 'beta_econ2'= 1,
                     'phi'=1, 'gamma' = matrix(1,12,10) , 'beta_i1'=1, 'beta_i2'=1)

#LOGIT model with ALL interactions

initslist.more = list('alpha_diary'=c(rep(1,6),NA), 'alpha_region'=c(rep(1,3),NA),
                     'alpha_hispanic'=1, 'alpha_sex'=1, 'alpha_race'=c(rep(1,4),NA), 
                     'beta_econ1'= 1, 'beta_econ2'= 1,
                     'phi'=1, 'gamma' = matrix(1,12,10) , 'beta_i1'=1, 'beta_i2'=1,
                     'beta_i3'=c(1,1,1,NA), 'beta_i4'=c(1,1,1,NA), 'beta_i5'=c(1,1,1,1,NA), 
                     'beta_i6'=c(1,1,1,1,NA), 'beta_i7'=1, 'beta_i8'=1)

#LOGIT model with income interactions
initslist.inc = list('alpha_diary'=c(rep(1,6),NA), 'alpha_region'=c(rep(1,3),NA),
                      'alpha_hispanic'=1, 'alpha_sex'=1, 'alpha_race'=c(rep(1,4),NA), 
                      'beta_econ1'= 1, 'beta_econ2'= 1,
                      'phi'=1, 'gamma' = matrix(1,12,10), 'beta_income'=0.5,
                     'beta_i9'=0.5,'beta_i10'=0.5 )





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

#Boxplot
boxplot(as.matrix(ss_econ_new)[,3:122]-mean(as.matrix(ss_econ_new)[,3]),use.cols=TRUE,
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

#Plot with error bars
errbar(3:122, summary(ss_econ_new)$statistics[3:122,1],
        summary(ss_econ_new)$quantiles[3:122,5],summary(ss_econ_new)$quantiles[3:122,1])
abline(h=0, col='red')

save(ss_econ_new, file="new_samples.Rda")



#LOGIT model with interaction (SEX) ----


model.int = jags.model(file.path(inpath, 'interaction_logit_model.txt'), data=datalist.log,
                       inits=initslist.int, n.chains=2, n.adapt=10000, quiet=FALSE)

ss_econ_int = coda.samples(model.int, c("beta_econ1","beta_econ2", "beta_i1", 
                                        "beta_i2","gamma"), 10000, thin=10)

save(ss_econ_int, file="int_samples.Rda")


#LOGIT model with ALL interactions ----

model.more = jags.model(file.path(inpath, 'more_interaction_logit_model.txt'), data=datalist.log,
                       inits=initslist.more, n.chains=2, n.adapt=10000, quiet=FALSE)

ss_econ_more = coda.samples(model.more, c("beta_econ1","beta_econ2", "beta_i1", 
                                        "beta_i2","beta_i3","beta_i4", "beta_i5", 
                                        "beta_i6", "beta_i7", 
                                        "beta_i8","gamma"), 10000, thin=10)

save(ss_econ_more, file="more_samples.Rda")


#LOGIT model with only RACE interaction

model.race = jags.model(file.path(inpath, 'race_interaction_logit_model.txt'), data=datalist.log,
                        inits=initslist.more, n.chains=2, n.adapt=10000, quiet=FALSE)

ss_econ_race = coda.samples(model.race, c("beta_econ1","beta_econ2", "beta_i1", 
                                          "beta_i2","beta_i3","beta_i4", "beta_i5", 
                                          "beta_i6", "beta_i7", 
                                          "beta_i8","gamma"), 10000, thin=10)

save(ss_econ_race, file="race_samples.Rda")


#LOGIT model with only REGION interaction

model.reg = jags.model(file.path(inpath, 'region_interaction_logit_model.txt'), data=datalist.log,
                        inits=initslist.more, n.chains=2, n.adapt=10000, quiet=FALSE)

ss_econ_reg = coda.samples(model.reg, c("beta_econ1","beta_econ2",
                                        "beta_i3","beta_i4","gamma"), 10000, thin=10)

save(ss_econ_reg, file="reg_samples.Rda")  #Overwritten???

#LOGIT model with only HISPANIC interaction

model.his = jags.model(file.path(inpath, 'his_interaction_logit_model.txt'), data=datalist.log,
                       inits=initslist.more, n.chains=2, n.adapt=10000, quiet=FALSE)

ss_econ_his = coda.samples(model.his, c("beta_econ1","beta_econ2",
                                        "beta_i7","beta_i8","gamma"), 10000, thin=10)

save(ss_econ_his, file="his_samples.Rda")


#LOGIT model introducing income variable interaction

model.inc = jags.model(file.path(inpath, 'income_interaction_logit_model.txt'), data=datalist.log,
                        n.chains=2, n.adapt=10000, quiet=FALSE)

ss_econ_inc = coda.samples(model.inc, c("beta_econ1","beta_econ2",
                                        "beta_income","beta_i9","beta_i10","gamma"), 10000, thin=10)

save(ss_econ_inc, file="inc_samples.Rda")


#GAMMA model introducing income variable interaction

model.gaminc = jags.model(file.path(inpath, 'income_gamma_model.txt'), data=datalist.log,
                      inits=initslist.inc, n.chains=2, n.adapt=10000, quiet=FALSE)

ss_econ_gaminc = coda.samples(model.gaminc, c("beta_econ1","beta_econ2",
                                        "beta_income","beta_i9","beta_i10","gamma"), 10000, thin=10)

save(ss_econ_gaminc, file="gaminc_samples.Rda")




#Get estimates and credible intervals to plot

estimates = c(summary(ss_econ_new)$statistic[1:2,1],summary(ss_econ_int)$statistic[3:4,1], 
              summary(ss_econ_race)$statistic[3:6,1], summary(ss_econ_race)$statistic[8:11,1],
              summary(ss_econ_reg)$statistic[3:5,1], summary(ss_econ_reg)$statistic[7:9,1],
              summary(ss_econ_his)$statistic[3:4,1])

lower_ci = c(summary(ss_econ_new)$quantiles[1:2,1],summary(ss_econ_int)$quantiles[3:4,1], 
             summary(ss_econ_race)$quantiles[3:6,1], summary(ss_econ_race)$quantiles[8:11,1],
             summary(ss_econ_reg)$quantiles[3:5,1], summary(ss_econ_reg)$quantiles[7:9,1],
             summary(ss_econ_his)$quantiles[3:4,1])

upper_ci = c(summary(ss_econ_new)$quantiles[1:2,5],summary(ss_econ_int)$quantiles[3:4,5], 
             summary(ss_econ_race)$quantiles[3:6,5], summary(ss_econ_race)$quantiles[8:11,5],
             summary(ss_econ_reg)$quantiles[3:5,5], summary(ss_econ_reg)$quantiles[7:9,5],
             summary(ss_econ_his)$quantiles[3:4,5])


#Plot estimates with intervals

var.names = c("ECON1 Main", "ECON2 Main", "ECON1 * SEX (Male)", "ECON2 * SEX (Male)", "ECON1 * RACE (White)",
            "ECON2 * RACE (White)", "ECON1 * RACE (Black)", "ECON2 * RACE (Black)", "ECON1 * RACE (Nat Amer)", "ECON2 * RACE (Nat Amer)",
            "ECON1 * RACE (Asian)", "ECON2 * RACE (Asian)", "ECON1 * REGION (NE)", "ECON2 * REGION (NE)", 
            "ECON1 * REGION (MW)", "ECON2 * REGION (MW)", "ECON1 * REGION (South)", "ECON2 * REGION (South)", 
            "ECON1 * HISPANIC", "ECON2 * HISPANIC")

y.axis <- c(length(estimates):1)

pdf("~/atus/poster/logit_estimates.pdf")

par(mfrow=c(1,1))
par(mar=c(2, 10, 6, 2))

plot(estimates, y.axis, type = "p", axes = F, xlab = "", ylab = "", pch = 19, cex = .6,
     xlim = c(-1,3.5), xaxs = "r", main = "Estimates and credible intervals for parameter\n estimates in logistic model")

segments(0,0,0,20,lty=2,col='red')

segments(lower_ci, y.axis, upper_ci, y.axis, lwd =  2)

segments(lower_ci, y.axis -.1, lower_ci, y.axis +.1, lwd = 2)
segments(upper_ci, y.axis -.1, upper_ci, y.axis +.1, lwd = 2)

axis(1, at = seq(-2,3.5,by=.5), labels =  seq(-2,3.5,by=.5), tick = T,
     cex.axis = .8, mgp = c(2,.5,0))
axis(2, at = y.axis, label = var.names, las = 1, tick = T, 
     cex.axis = .8) 

dev.off()

##############################################################################################################

#LOGIT new model with no gamma

#model.new.no.gamma = jags.model(file.path(inpath, 'new_logit_model_no_gamma.txt'), data=datalist.log,
#                       inits=initslist.new, n.chains=2, n.adapt=10000, quiet=FALSE)

#ss_econ_new_no_gamma = coda.samples(model.new, c("beta_econ1","beta_econ2","gamma"), 10000, thin=10)

#boxplot(as.matrix(ss_econ_new_no_gamma))

#LOGIT nonautoregressive







