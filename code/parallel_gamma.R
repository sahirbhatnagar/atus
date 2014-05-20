source("~/Dropbox/PhD/SSC case study/data.R")

jinits <- function() {
  ### all the other params ###
  list(
  .Rng.name="lecuyer::RngStream",
  .Rng.seed=randomNumbers(n = 1, min = 1, max = 1e+06,col=1) )
}

# Helper function to combine multiple mcmc lists into a single one. 
mcmc.combine <- function( ... ) {
   return( as.mcmc.list( sapply( list( ... ), mcmc ) ) )
}

jags.parsamples <- foreach(i=1:getDoParWorkers(), .inorder = FALSE, 
                           .packages = c( 'rjags', 'random' ), .combine = "mcmc.combine", 
                           .multicombine = TRUE ) %dopar% {
  load.module( "lecuyer" )
  model.jags <- jags.model(file.path(inpath, 'main_effect_gamma'), data=datalist.gam, 
                      n.chains=1, n.adapt=1000, quiet=FALSE)
  result <- coda.samples(model.jags,
  variable.names=c("beta_econ_1", "beta_econ_2", "logRR_time","beta_econ_race_1",
               "beta_econ_race_2","P.res","P.res.new","C.new","resid","fit",
               "fit.new","chisq.p"), n.iter=5000, thin=10)
  return(result)
}

mcmcChain <- as.matrix(jags.parsamples)
#Beta economy
beta_econ_1 <- mcmcChain[,which(colnames(mcmcChain)=="beta_econ_1")]
beta_econ_2 <- mcmcChain[,which(colnames(mcmcChain)=="beta_econ_2")]
summary(beta_econ_1);summary(beta_econ_2)
quantile(beta_econ_1, probs=c(0.025,0.975))
quantile(beta_econ_2, probs=c(0.025,0.975))


#gamma time
log_RR_time <- mcmcChain[,which(colnames(mcmcChain)=="logRR_time[1,1]"):which(colnames(mcmcChain)=="logRR_time[12,10]")]

#time trend
boxplot(log_RR_time,use.cols=TRUE, 
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

#Pearson Residuals
p.res <- mcmcChain[,which(colnames(mcmcChain)=="P.res[1]"):which(colnames(mcmcChain)=="P.res[547]")]
p.res.new <- mcmcChain[,which(colnames(mcmcChain)=="P.res.new[1]"):which(colnames(mcmcChain)=="P.res.new[547]")]

plot(mcmcChain[,"fit"], mcmcChain[,"fit.new"], main="Posterior predictive check \nfor sum of squared Pearson residuals", xlab="Discrepancy measure for actual data set", ylab="Discrepancy measure for perfect data sets", col="blue")
abline(0, 1, lwd=2, col = "black")


#################################################################################
#                                       Plot 1                                  #
#################################################################################
#Mean residuals vs economy
residuals <- mcmcChain[,which(colnames(mcmcChain)=="resid[1]"):which(colnames(mcmcChain)=="resid[547]")]
residuals.m <- melt(residuals)
dat_resid_econ <- data.frame(econ1=DTS$ECON1,econ2=DTS$ECON2,residuals=apply(residuals,2,mean))
p <- ggplot(dat_resid_econ, aes(econ1, residuals))
resid_econ1 <- p + geom_point(colour = "red", size = 3) + geom_abline(slope=0,intercept = 0)+
  labs(list(title="Mean Residuals vs 1st Principal Component of Economy "), size=2)


#################################################################################
#                                       Plot 2                                  #
#################################################################################
#Mean residuals vs economy2
p <- ggplot(dat_resid_econ, aes(econ2, residuals))
resid_econ2 <- p + geom_point(colour = "red", size = 3) + geom_abline(slope=0,intercept = 0)+
  labs(list(title="Mean Residuals vs 2nd Principal Component of Economy "), size=2)


#################################################################################
#                                       Plot 3                                  #
#################################################################################
# Residuals vs Fitted values
fitted <- mcmcChain[,which(colnames(mcmcChain)=="C.new[1]"):which(colnames(mcmcChain)=="C.new[547]")]

#dat_resid_observed <- data.frame(tvtime=DTS$TVTIME,residuals=apply(residuals,2,mean))
dat_resid_fitted <- data.frame(simulated=apply(fitted,2,mean),residuals=apply(residuals,2,mean))

p <- ggplot(dat_resid_fitted, aes(simulated, residuals))
resid_fitted <- p + geom_point(colour = "red", size = 3) + geom_abline(slope=0,intercept = 0)+
  labs(list(title="Mean Residuals vs Mean Simulated TV Time use"), size=2) + xlab("mean simulated TV Time use")
  


#################################################################################
#                                       Plot 4                                  #
#################################################################################
#predicted values plotted over oberved histogram
m <- ggplot(DTS, aes(x=TVTIME))
fitted_hist <- m + geom_histogram(binwidth=25)+aes(y=..density..) + 
  geom_density(data=pred.m, aes(x=value), size=1.1, colour="red")+
  labs(list(title="Density of Fitted Values and Histogram of Observed TV Time use"), size=2)+
  ylab("density") + xlab("observed TV Time")



#################################################################################
#                                       Plot 5                                  #
#################################################################################
#Pearson residuals of predicted vs observed
pears <- data.frame(fit=mcmcChain[,"fit"],fit.new=mcmcChain[,"fit.new"])
m <- ggplot(pears, aes(fit,fit.new))
pearson <- m + geom_point(colour = "red", size = 3) + geom_abline(intercept = 0)+
  labs(list(title="Posterior predictive check for sum of squared Pearson residuals"), size=2) + xlab("Discrepancy measure for actual data set")+
ylab("Discrepancy measure for simulated data set")


source("~/Dropbox/Winter 2014/MATH 783/Assignments/A3/multiplot.R")
pdf("~/git_repositories/atus/poster/validation.pdf")
multiplot(resid_econ1, resid_fitted, resid_econ2, fitted_hist, cols=2)
dev.off()