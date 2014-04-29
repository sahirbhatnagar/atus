##################################
# R source code file used to attempt PCA on economic data
# Created by Kevin, March 16, 2014
# Updated March 16, 2014
# hosted on Github repo 'sahirbhatnagar/atus'
# NOTE:
##################################

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



#Final data structure for economy and time by month
ECON = cbind(-pca_econ$scores[,1], -pca_econ$scores[,2], 1:length(pca_econ$scores[,1]))







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




