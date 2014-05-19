##################################
# R source code file used to attempt PCA on economic data
# Created by Kevin, March 16, 2014
# Updated March 16, 2014
# hosted on Github repo 'sahirbhatnagar/atus'
# NOTE:
##################################

#setwd("~/git_repositories/atus.git/data")

#On kevisco's laptop
setwd("~/atus/data")

#Initial data read
gdp_data = read.table("real_gdp_data.txt", header=TRUE)
dow_data = read.table("dow_data.txt", header=TRUE)
sp500_data = read.table("sp500_data.txt", header=TRUE)
unemployment_data = read.table("unemployment_rate_data1.txt", header=TRUE)

#Trying naive method: averaging monthly observations to quarterly ones. GDP is already by quarter
gdp_quart = gdp_data$real_gdp
dow_quart = NULL; sp500_quart = NULL; unemployment_quart = NULL; count=1
for (i in seq(1, length(dow_data$AdjClose), by = 3)) {
  dow_quart[count] = mean(dow_data$AdjClose[i:(i+2)])
  sp500_quart[count] = mean(sp500_data$AdjClose[i:(i+2)])
  unemployment_quart[count] = mean(unemployment_data$rate[i:(i+2)])
  
  count = count + 1
}

#Looking at employment instead of unemployment
employment_quart = 100-unemployment_quart

#Merging into single data matrix in order to do PCA
econ_measures = cbind(gdp_quart, dow_quart, sp500_quart, employment_quart)

#Trying PCA. Setting parameter cor=TRUE as vectors have different units.
pca_econ = princomp(econ_measures, cor=TRUE)
summary(pca_econ)
pca_econ$loadings

#Scaling down and centering the different measures for simultaneous plot
gdp_scale = (gdp_quart-pca_econ$center[1])/pca_econ$scale[1]
dow_scale = (dow_quart-pca_econ$center[2])/pca_econ$scale[2]
sp500_scale = (sp500_quart-pca_econ$center[3])/pca_econ$scale[3]
employment_scale = (employment_quart-pca_econ$center[4])/pca_econ$scale[4]

#plotting
plot(-50, -50, xlim=c(0,length(gdp_scale)), ylim=c(min(gdp_scale),max(gdp_scale)))
lines(1:length(gdp_scale), gdp_scale, col='red')
lines(1:length(dow_scale), dow_scale, col='blue')
lines(1:length(sp500_scale), sp500_scale, col='green')
lines(1:length(employment_scale), employment_scale, col='purple')

#looking at negative of pca scores to better compare to data
lines(1:length(gdp_scale), -pca_econ$scores[,1], col=6, lty=2)
lines(1:length(gdp_scale), -pca_econ$scores[,2], col=9, lty=2)

#legend("bottomleft", c("gdp", "dow", "sp500", "employ", "PCA1", "PCA2"), 
#      col=c('red', 'blue', 'green', 'purple', 6, 9),
#       lty=c(1,1,1,1,2,2), xpd=TRUE, horiz=TRUE)



#Final data structure for economy and time by quarter
ECON = as.data.frame(cbind(-pca_econ$scores[,1], 
                           -pca_econ$scores[,2], 1:length(pca_econ$scores[,1])))
names(ECON) = c("pc1", "pc2", "quarter")

#Write to file...
#write(t(names(ECON)), file="~/atus/data/econ_data.txt", ncolumns=3)
#write(t(as.matrix(ECON)), file="~/atus/data/econ_data.txt", ncolumns=length(names(ECON)), append=TRUE)


#FINAL PLOT TO PUT ON POSTER ----

pdf("~/atus/poster/econ_measures.pdf")

plot(-50, -50, xlim=c(0,length(gdp_scale)), ylim=c(min(-pca_econ$scores[,1]),
     max(-pca_econ$scores[,1])),xaxt="n", xlab="Quarter",
     ylab="Centered and scaled measure", main="Economic measures by quarter 2003-2012")
lines(1:length(gdp_scale), gdp_scale, col='red', lwd=2.5)
lines(1:length(dow_scale), dow_scale, col='blue',lwd=2.5)
lines(1:length(sp500_scale), sp500_scale, col='green',lwd=2.5)
lines(1:length(employment_scale), employment_scale, col='brown',lwd=2.5)

lines(1:length(gdp_scale), -pca_econ$scores[,1], col=6, lty=2)
lines(1:length(gdp_scale), -pca_econ$scores[,2], col=9, lty=2)

axis(1,at=seq(1,40),
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

legend(30,-1.8,legend=c("GDP","DOW JONES","S&P 500","EMPLOYMENT RATE","PRINCIPAL COMP 1",
                        "PRINCIPAL COMP 2"), bty="n", cex=0.6, col=c('red','blue','green','brown',
                        'purple', 'black'), lty=c(1,1,1,1,2,2))

dev.off()

#Trying PCA without SP500 vector
#econ_reduce = cbind(gdp_quart, dow_quart, unemployment_quart)
#pca_reduce = princomp(econ_reduce, cor=TRUE)
#summary(pca_reduce)
#pca_reduce$loadings


#plotting again... no sp500
#plot(-50, -50, xlim=c(0,length(gdp_scale)), ylim=c(min(gdp_scale),max(gdp_scale)))
#lines(1:length(gdp_scale), gdp_scale, col='red')
#lines(1:length(dow_scale), dow_scale, col='blue')
#lines(1:length(sp500_scale), sp500_scale, col='green')
#lines(1:length(unemployment_scale), unemployment_scale, col='purple')

#lines(1:length(gdp_scale), pca_reduce$scores[,1], col=8)
#lines(1:length(gdp_scale), pca_reduce$scores[,2], col=9)




