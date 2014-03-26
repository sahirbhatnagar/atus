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
	

Random thoughts March 6th 2014 by Celia
-------------------

Let yi be the number of minutes of TV watching for person i, and let Xi be a (vector) covariate like education or income or employment, for person i.

Assume yi = a(t) + b(t)Xi + ei. This allows the mean a to vary with time t and also the association with Xi to vary with time.

Ideally, it would be nice to add structure to a(t) and b(t) (think hierarchical models). For example: 
a(t) = a0 + a1t + a2t^2 + a3E(t) + ... + epsilon, where E(t) refers to an economic measure in year t. Given that the data span about 10 years there will be a limit on how many covariates can be included.

Similarly, b(t) = b0 + b1t + b2t^2 + b3E(t) + ... + epsilon. 

The question is how could this be estimated. It is a hierarchical model. Have you any experience with WINBUGS?
* Gibbs sampling, Bayesian models.
* Very flexible but challenging to get started
* Might choke on data of this magnitude

SAS should (might?) be capable with a careful study of something like Proc MIXED.


Room Bookings
----------------------------------

* HSSL - RM-19C, 1:00pm - 5:00pm Wednesday, March 19, 2014
* HSSL - RM-07F, 10:00am - 2:00pm Wednesday, March 26, 2014
* HSSL - M2-17B, 1:00pm - 5:00pm Wednesday, April 2, 2014


Meeting with Abbas March 12, 2014
----------------------------------

* Get started with multiple linear regression without weights. Start simple.
* Look into group LASSO for categorical variables, and varying coefficients
* Regularization techniques exist for varying coefficients


Meeting with Olli March 13, 2014
----------------------------------

* Weights are used to make your sample representative of the population. If you put in your model the factors which were used to compute the weights, then you are effectively correcting for the imbalances arising from the sampling (but note that the meaning of your coefficient for these variables changes -- you basically have two contributions: the true influence at the population level, and the influence of the sampling mechanism). In other words, how to include the weights in the analysis depends on the question you are asking.
* The hierarchical structure added to the model to take into account the time-dependence can be fitted trough Gibbs sampling (cf. JAGS).
* The first important step is to write down a model, then discuss possible adjustments and decide how to fit it to the data.
* There is a lot of zero values for TV use; our analysis should address this problem.

The model we came up with after this meeting is the following:

Yi = a(ti) + bXi + epsilon i,

where a(ti) = aE(ti) + espilon(ti).
We are thus left with the following questions:
* What kind of autoregressive structure do we want for a(ti), i.e. what is the distribution of the error term in year k given the error terms in previous years?
* How do we want to handle the zero-inflated structure? FMR? Or two separate analyses?
* How can we measure "economy"?
* How do we do variable selection with this model?


Kevin's thoughts on economic variables March 16, 2014
----------------------------------

* Should use Real GDP rather than GDP as it adjusts for inflation.
* GDP data is available only quarterly while data for unemployment and stock indices are available monthly.  Initial thought is just to average over every three months to get quarterly estimates for unemployment and stock indices.  Any possible problems with this???
* Big 3 Stock indices in USA: Dow Jones Industrial Average, S&P-500, and NASDAQ Composite.  First two seem fine as they're calculated based on US companies, however, third includes international companies and therefore was excluded from the analysis.
* For stock index data we should use the column AdjClose as it's adjusted for inflation.


Meeting March 19th (Max, Sy, KMAC)
---------------------------------

[Gamma Regression Slides and Exploratory Analysis Examples](https://www-m4.ma.tum.de/fileadmin/w00bdb/www/czado/lec8.pdf)

We discussed the following plan
* Work on EPI602 Assignment 4 to get more comfotable with JAGS, and keeping in mind our tentative hierarchical model
* Do more exploratory analyses/plots
* Think about hierarchical models more deeply. We should be able to explain why we use it


Meeting March 26th (Max Sy, Kevisco)
---------------------------------

[*A review of Bayesian variable selection methods: What, How and Which*](http://www.evolvedmicrobe.com/Literature/2009_Review%20of%20Bayesian%20Variable%20selection%20methods.pdf), by O'Hara and Sillanpaa (2009), with [code provided](http://ba.stat.cmu.edu/journal/2009/vol04/issue01/ohara/supplement.html)

[*The Bayesian Lasso*](http://www.stat.ufl.edu/archived/casella/Papers/Lasso.pdf), Park and Casella (2008)

[*Efﬁcient Empirical Bayes Variable Selection and Estimation in Linear Models*](http://pages.stat.wisc.edu/~myuan/papers/lasso.final.pdf), Yuan and Lin (2005)
