#####################################################################
# "R for Everyone", Jared P. Lander, (c) 2014, pp. 217-295          #
# Chapter 19: Regularization and Shrinkage                          #
# 19.1 Elastic Net                                                  #
#####################################################################

setwd("/Volumes/HD2/Users/pstessel/Documents/Git_Repos/bayesian_shrinkage")

# clear global environment
rm(list=ls(all = TRUE))

load("data/ideo.rdata")
head(ideo)

require(coefplot)
# require(caret)
# require(useful)
# require(glmnet)
# require(parallel)
# require(doParallel)
# require(reshape2)
# require(stringr)
# require(ROCR)

### To show the need for shrinkage, we fit a separate model for each election year and then display the resulting coefficients for the black level of Race.

## fit a bunch of models
# figure out the years we will be fitting the models on
theYears <- unique(ideo$Year)

# create an empty list
# as many elements as years
# hold it in the results
# preallocating the object makes the code run faster
results <- vector(mode="list", length=length(theYears))
# give good names to the list
names(results) <- theYears

## loop through the years
# fit a model on the subset of data for that year
for(i in theYears)
{
  results[[as.character(i)]] <- glm(Vote ~ Race + Income + Gender + Education,
                                    data=ideo, subset=Year==i,
                                    family=binomial(link="logit"))
}

# the the coefficient information
voteInfo <- multiplot(results, coefficients="Raceblack", plot=FALSE)
head(voteInfo)

# plot it restricting the window to (-20, 10)
multiplot(results, coefficients="Raceblack", secret.weapon = TRUE) +
  coord_flip(xlim=c(-20, 10))

### The plot shows the coefficient for the black level of Race for each model.
### The result of the model from 1964 is clearly different from the other
### models. The plot shows standard errors, which threw off the scale so much we
### had to restrict the plot window to still see variation in the other points.
### Fitting a series of models like this and then plotting the coefficients over
### time has been termed the "secret weapon" by Gelman due to its usefulness and
### simplicity.

### By comparing the model for 1964 to the other models, we can see that
### something is clearly wrong with the estimate. To fix this we put a prior on
### the coefficients in the model. The simplest way to do this is to use
### Gelman's bayesglm function in the arm package. By default it sets a Cauchy
### prior with scale 2.5. Because the arm package namespace interferes with the
### coefplot namespace, we do not load the package but rather just call the
### function using the :: operator.

resultsB <- vector(mode="list", length=length(theYears))
# give good names to the list
names(resultsB) <- theYears

## loop through the years
## fit a model on the subset of data for that year
for(i in theYears)
{
  # fit model with Cauchy priors with a scale of 2.5
  resultsB[[as.character(i)]] <-
    arm::bayesglm(Vote ~ Race + Income + Gender + Education,
                  data=ideo[ideo$Year == i, ],
                  family=binomial(link="logit"),
                  prior.scale=2.5, prior.df=1)
}

# build the coefficient plot
multiplot(resultsB, coefficients = "Raceblack", secret.weapon = TRUE)

### Simply adding the Cauchy priors dramatically shrinks both the estimate and the standard error of the coefficient, as seen in the above plot. Remember, the models were fitted independently, meaning that it was simply the prior that did the fix and not information from the other years. It turns out that the survey conducted in 1964 underrepresented black respondents, which led to a highly inaccurate measure.

### The default prior is a Cauchy with scale 2.5, which is the same as a t distribution with 1 degree of freedom. These arguments, prior.scale and prior.df, can be changed to represent a t distribution with any degrees of freedom. Setting both to infiniti (Inf) makes them normal priors, which is identical to running an ordinary glm.
