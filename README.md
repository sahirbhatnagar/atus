SSC 2014 Case Study Competition
-------------------

This repository is set up for the data analysis that will done in `R`. 


Data Sources
-------------------

The data come from the [Bureau of Labor Statistics website](http://www.bls.gov/tus/datafiles_0312.htm). 

The two (possibly more) data files that we are using are the:

* [Respondent file](http://www.bls.gov/tus/special.requests/atusresp_0312.zip)
* [Activity Summary file](http://www.bls.gov/tus/special.requests/atussum_0312.zip)


Documentation
------------------

* [User's Guide](http://www.bls.gov/tus/atususersguide.pdf) has all the survey sampling methods, weighting justification, ect.
* [Frequently Used Variables](http://www.bls.gov/tus/freqvariables.pdf) has the codes of the most common variables found in the data
* [Activity codes](http://www.bls.gov/tus/lexiconnoex0312.pdf) is where we will find the time use activity codes of interest


Summary of meeting March 4th 2014 by Max
-------------------

### Main questions
* What effect does the economy have on the amount of time spent watching television and playing video games?
	* Does this vary by gender?
	* Does this vary according to labour force participation?
	* Does this vary across income?
	
* What are the strongest sociodemographic predictors of time spent watching television?
* **Exploratory question**: What activities have been replaced by increased time spent on television and video games?


### Challenges (discussion between Maxime and Sahir)
* Should we include the weighting method in our regression model (perhaps through the likelihood)?
* How can we measure the variable "economy"?
* Look for seasonal trends (year to year)
* How can we measure the effect of economy?
* How do we want to handle categorical variables?
* Is missing data/imputation an issue?
* Are the ATUS modules introducing variables that are only available for just a few years (and thus unusable for the 10-year period)?


###Possible solutions (meeting between Maxime, Sahir, Celia and Jiunping)
* Look into Generalized Additive Models (GAMs)
* Can we get data on households that did not respond? If yes, can we use it to check if non-response is introducing bias?
  * Also, check if CPS or ATUS published data on non-response (e.g. which factors are likely to influence it).
* For regularization techniques, does the weighting interact with the penalty term?
* Is the initiator of the activity the same as the respondent, and is this introducing bias?
* **Economy**: stock indices, unemployment rate, GDP (look up if there are standard economic indices)
  * There is of course correlation between these indices. If we use multiple indices, we may want to use something like PCA to reduce correlation.
	
