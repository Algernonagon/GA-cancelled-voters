#====================================================================================
#Code written by Nathan Yang as part of larger study for research paper presented to 
#Fair Fight and Emory Dept of Math *All graphs best viewed as pdf files
#====================================================================================

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
library(ggplot2)


removed <- list()
for (i in c(2012:2018)) {
  removed[[i]] = filter(deaths_purged, year==i)
}

removed_ratios <- list()
for (i in c(2012:2018)) {
  county <- removed[[i]]$county
  cancelled <- removed[[i]]$death_cancellations
  dead <- removed[[i]]$deaths
  population <- removed[[i]]$pop
  ratioR = cancelled/(population)
  ratioD = dead/(population)
  ratioRD = cancelled/dead
  removed_ratios[[i]] <- tibble(counties=county, removed_ratio=ratioR, 
                           dead_ratio=ratioD, removed_dead=ratioRD, deaths=dead,
                           death_removals=cancelled, pop=population)
}

removed_ratios1 <- list()
for (i in c(2012:2018)) {
  removed_ratios1[[i]] <- filter(removed_ratios[[i]], counties!="DeKalb")
}



#====================================================================================
#Prob distr func for ratio of death cancellations to county population each year
#Distribution fit determined using Pearson's Chi-squared in purgedVotersDeathDistr.R
#====================================================================================


normpdf <- list()
for (i in c(2012:2018)) {
  mew <- mean(removed_ratios[[i]]$removed_ratio)
  sdw <- sd(removed_ratios[[i]]$removed_ratio)
  normpdf[[i]] <- dnorm(removed_ratios[[i]]$removed_ratio, mean=mew, sd=sdw)
}

betapdf <- list()
for (i in c(2012:2018)) {
  alpha <- beta.mle(removed_ratios[[i]]$removed_ratio)$param[1]
  beta <- beta.mle(removed_ratios[[i]]$removed_ratio)$param[2]
  betapdf[[i]] <- dbeta(removed_ratios[[i]]$removed_ratio, shape1=alpha, shape2=beta)
}


ggplot(removed_ratios[[2018]], aes(x=removed_ratio)) +
  geom_line(aes(removed_ratios[[2012]]$removed_ratio,betapdf[[2012]]), colour="red") +
  geom_line(aes(removed_ratios[[2013]]$removed_ratio,betapdf[[2013]]), colour="orange") +
  geom_line(aes(removed_ratios[[2014]]$removed_ratio,betapdf[[2014]]), colour="green") +
  geom_line(aes(removed_ratios[[2015]]$removed_ratio,normpdf[[2015]]), colour="brown") +
  geom_line(aes(removed_ratios[[2016]]$removed_ratio,normpdf[[2016]]), colour="blue") +
  geom_line(aes(removed_ratios[[2017]]$removed_ratio,normpdf[[2017]]), colour="purple") +
  geom_line(aes(removed_ratios[[2018]]$removed_ratio,normpdf[[2018]])) +
  labs(y=" ",x="Ratio of death cancellations to county population") + xlim(0, 0.025) +
  theme_grey(base_size = 22)



#====================================================================================
#Prob distr func for ratio of deaths to county population each year
#Distribution fit determined using Pearson's Chi-squared in purgedVotersDeathDistr.R
#*DeKalb outlier removed 
#(replace removed_ratios1 with removed_ratios to include DeKalb)
#====================================================================================


normpdf <- list()
for (i in c(2012:2018)) {
  mew = mean(removed_ratios1[[i]]$dead_ratio)
  sdw = sd(removed_ratios1[[i]]$dead_ratio)
  normpdf[[i]] <- dnorm(removed_ratios1[[i]]$dead_ratio, mean=mew, sd=sdw)
}

ggplot(removed_ratios1[[2018]], aes(x=dead_ratio)) +
  geom_line(aes(removed_ratios1[[2012]]$dead_ratio,normpdf[[2012]]), colour="red") +
  geom_line(aes(removed_ratios1[[2013]]$dead_ratio,normpdf[[2013]]), colour="orange") +
  geom_line(aes(removed_ratios1[[2014]]$dead_ratio,normpdf[[2014]]), colour="green") +
  geom_line(aes(removed_ratios1[[2015]]$dead_ratio,normpdf[[2015]]), colour="brown") +
  geom_line(aes(removed_ratios1[[2016]]$dead_ratio,normpdf[[2016]]), colour="blue") +
  geom_line(aes(removed_ratios1[[2017]]$dead_ratio,normpdf[[2017]]), colour="purple") +
  geom_line(aes(removed_ratios1[[2018]]$dead_ratio,normpdf[[2018]])) +
  labs(y=" ",x="Ratio of deaths to county population") + xlim(0, 0.025) +
  theme_grey(base_size = 22)



#===============================================================================
#Death cancellations plotted against death rates
#Dotted line is one to one reference line
#Blue line is linear regression
#===============================================================================


#use year1 and year2 to set desired time frame
year1 <- 2012
year2 <- 2018
avgdeathrate <- list()
for (i in c(1:158)) {
  sum = 0
  for (j in c(year1:year2)) 
    sum = sum + removed_ratios1[[j]][i,3] 
  avgdeathrate[[i]] <- sum/(year2-year1+1)
}
avgdeathrate <- unlist(avgdeathrate)

avgremovalrate <- list()
for (i in c(1:158)) {
  sum = 0
  for (j in c(year1:year2)) 
    sum = sum + removed_ratios1[[j]][i,2]
  avgremovalrate[[i]] <- sum/(year2-year1+1)
}
avgremovalrate <- unlist(avgremovalrate)

avgyear1_year2 <- tibble(county=removed_ratios1[[2012]]$counties, 
                       avg_death=avgdeathrate, avg_removal=avgremovalrate, 
                       pop=removed_ratios1[[year2]]$pop)

ggplot(avgyear1_year2, aes(x=avg_death*1000, y=avg_removal*1000)) + 
  geom_point(aes(size=pop, shape=1)) + scale_shape_identity() + 
  scale_size_continuous(range = c(2, 20)) + ggtitle(paste(year1, "-", year2)) +
  xlab("Average deaths per 1000 population") + 
  ylab("Average removals per 1000 population") + 
  xlim(0,17) + ylim(0,17) + coord_fixed() +
  geom_smooth(method = 'lm') +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  theme_grey(base_size = 22) + theme(legend.position = "none")



#=================================================================================
#Death cancellations plotted against raw deaths
#Dotted line is one to one reference line
#Blue line is linear regression
#=================================================================================


#use year1 and year2 to set desired time frame
year1 <- 2017
year2 <- 2018
totaldeaths <- list()
for (i in c(1:158)) {
  totaldeaths[[i]] <- 0
  for (j in c(year1:year2)) 
    totaldeaths[[i]] = totaldeaths[[i]] + removed_ratios1[[j]][i,5] 
}
totaldeaths <- unlist(totaldeaths)

totalremovals <- list()
for (i in c(1:158)) {
  totalremovals[[i]] <- 0
  for (j in c(year1:year2)) 
    totalremovals[[i]] = totalremovals[[i]] + removed_ratios1[[j]][i,6]
}
totalremovals <- unlist(totalremovals)

totalyear1_year2 <- tibble(county=removed_ratios1[[2012]]$counties, 
                         tot_death=totaldeaths, tot_removal=totalremovals, 
                         pop=removed_ratios1[[year2]]$pop)

ggplot(totalyear1_year2, aes(x=tot_death, y=tot_removal)) + 
  geom_point(aes(size=pop, shape=1)) + scale_shape_identity() + 
  scale_size_continuous(range = c(2, 20)) + ggtitle(paste(year1, "-", year2)) +
  xlab("Deaths") + ylab("Death removals") + 
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  theme_grey(base_size = 22) + theme(legend.position = "none")



#==============================================================================
#linear regression calculator
#*incomplete and unecessary
#==============================================================================


sum_x <- 0
for (i in c(1:158)) {
  sum_x <- sum_x + removed_ratios1[[year]][i,5]
}
sum_y <- 0
for (i in c(1:158)) {
  sum_x <- sum_x + removed_ratios1[[year]][i,6]
}
sum_x2 <- 0
for (i in c(1:158)) {
  sum_x <- sum_x + (removed_ratios1[[year]][i,5])^2
}
sum_xy <- 0
for (i in c(1:158)) {
  sum_xy <- sum_xy + (removed_ratios1[[year]][i,5]*removed_ratios1[[year]][i,6])
}

b <- 158  
  
  


