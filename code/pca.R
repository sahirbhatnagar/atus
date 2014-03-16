##################################
# R source code file used to attempt PCA on economic data
# Created by Kevin, March 16, 2014
# Updated March 16, 2014
# hosted on Github repo 'sahirbhatnagar/atus'
# NOTE:
##################################

setwd("~/git_repositories/atus.git/data")

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

#Merging into single data matrix in order to do PCA
econ_measures = cbind(gdp_quart, dow_quart, sp500_quart, unemployment_quart)

#Trying PCA. Setting parameter cor=TRUE as vectors have different units.
pca_econ = princomp(econ_measures, cor=TRUE)
summary(pca_econ)
pca_econ$loadings
