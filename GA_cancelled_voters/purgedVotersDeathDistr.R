#============================================================================================
#Code written by Nathan Yang to test goodness of fit for distributions of death cancellations
#from Georgia voter rolls using Pearson's Chi-squared
#============================================================================================

#change file path if necessary
setwd("~/GA_cancelled_voters")
library(readr)
death_cancellations <- read_csv("death_cancellations.csv")

deaths_purged <- death_cancellations

library(dplyr)
library(tibble)
library(tidyr)
library(tidyverse)
library(Rfast)

#year can be adjusted to any year 2012-2018
removed <- filter(deaths_purged, year==2012)
county <- removed$county
cancelled <- removed$death_cancellations
dead <- removed$deaths
population <- removed$pop
ratioR = cancelled/(population)
ratioD = dead/(population)
ratioRD = cancelled/dead

removed_ratios <- tibble(counties=county, removed_ratio=ratioR, 
                         dead_ratio=ratioD, removed_dead=ratioRD, 
                         pop=population)

#normal distribution
mew <- mean(removed_ratios$removed_ratio)
sdw <- sd(removed_ratios$removed_ratio)
shapew <- mew^2/sdw^2
scalew <- sdw^2/mew
db <- dnorm(removed_ratios$removed_ratio, mean=mew, sd=sdw)

#beta distribution
beta.mle(removed_ratios$removed_ratio)
alpha = beta.mle(removed_ratios$removed_ratio)$param[1]
beta = beta.mle(removed_ratios$removed_ratio)$param[2]
db1 <- dbeta(removed_ratios$removed_ratio, shape1=alpha, shape2=beta)

#gammamle(removed_ratios$removed_ratio)
#db2 <- dgamma(removed_ratios$removed_ratio, shape=shapew, scale=scalew)


#lb is lower bound of buckets
#up is upper bounds of buckets
#d contains the amount of data points in each bucket
#bucket bounds can be adjusted for each year for "good" bucket sizes
lb <- c(0, 0.004, 0.005, 0.006, 0.007, 0.008, 0.009, 0.01, 0.012)
up <- c(0.004, 0.005, 0.006, 0.007, 0.008, 0.009, 0.01, 0.012, 0.03)
d <- seq(1:length(lb))
for (i in c(1:length(lb))) {
  d[i] = table(removed_ratios$removed_ratio>lb[i] & 
                 removed_ratios$removed_ratio<=up[i])[2]
}
d

#d1 is the chi-squared test statistic to compare to chi-squared distr table
#test for normal distribution
d1 <- 0
for (i in c(1:length(lb))) {
  d1 = d1+((pnorm(up[i],mean=mew,sd=sdw)-pnorm(lb[i],mean=mew,sd=sdw))*159-d[i])^2/
    ((pnorm(up[i],mean=mew,sd=sdw)-pnorm(lb[i],mean=mew,sd=sdw))*159)
}
d1
#test for beta distribution
d1 <- 0
for (i in c(1:length(lb))) {
  d1 = d1+((pbeta(up[i],shape1=alpha,shape2=beta)-pbeta(lb[i],shape1=alpha,shape2=beta))*159-d[i])^2/
    ((pbeta(up[i],shape1=alpha,shape2=beta)-pbeta(lb[i],shape1=alpha,shape2=beta))*159)
}
d1



ggplot(removed_ratios, aes(x=removed_ratio)) + 
  geom_histogram(bins=20) + geom_line(aes(removed_ratios$removed_ratio,db1/7))
#ggplot(removed_ratios, aes(x=dead_ratio)) + 
#  geom_histogram(bins=20) 
#ggplot(removed_ratios, aes(x=removed_dead)) + 
#  geom_histogram(bins=20) 