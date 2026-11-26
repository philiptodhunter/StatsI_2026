#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
lapply(c("car","stargazer"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Question 1: Economics
#In this question, use the prestige dataset in the car library.
#First, run the following commands:

#install.packages(car) #if not yet installed
library(car)
data(Prestige)
help(Prestige)

#We would like to study whether individuals with higher levels of income have 
#more prestigious jobs. Moreover, we would like to study whether professionals 
#have more prestigious jobs than blue and white collar workers.

options(scipen = 999)
#install.packages(car) #if not yet installed
library(car)
data(Prestige)
help(Prestige)
summary(Prestige)
head(Prestige)
str(Prestige)

#(1a) Create a new variable professional by recoding the variable type so that professionals
#are coded as 1, and blue and white collar workers are coded as 0 (Hint: ifelse).

# Create new variable:
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)
# Check that it worked:
table(Prestige$type, Prestige$professional)

summary(Prestige$type)

#(1b) Run a linear model with prestige as an outcome and income, professional, and the
#interaction of the two as predictors (Note: this is a continuous × dummy interaction.

model_1 <- lm(prestige ~ income + professional + income*professional, data = Prestige)
summary(model_1)

#(1c) Write the prediction equation based on the result.

#predicted prestige = Intercept + (Income coefficient x Income) + (Professional coefficient x professional) + (Interaction coefficient of professional and income x income x professional) 
#predicted prestige = 21.142 + (0.00317 x income) + (37.781 x professional) + (-0.00233 x income x professional)
#make a function to do this:
predicted_prestige <- function(income, professional) {
     model_1$coefficients[["(Intercept)"]] + 
    (model_1$coefficients[["income"]] * income) +
    (model_1$coefficients[["professional"]] * professional) +
    (model_1$coefficients[["income:professional"]] * income * professional)
}

#(1d) Interpret the coefficient for income.

#for blue collar and white collar occupations each additional dollar of income is associated with about a 0.00317 unit increase in prestige on average. Because the p-value is far below 0.05 we reject the null hypothesis that income has no relationship with prestige.

#(1e) Interpret the coefficient for professional

#When income is $0, professional occupations are associated with 37.781 more units of prestige than blue collar and white collar workers on average. Because the p-value is far below 0.05 we reject the null hypothesis that there is no difference in prestige between blue collar/white collar workers and professionals when income is $0.

#(1f) What is the effect of a $1,000 increase in income on prestige score for professional
#occupations? In other words, we are interested in the marginal effect of income when
#the variable professional takes the value of 1. Calculate the change in ˆy associated
#with a $1,000 increase in income based on your answer for (c).

#we take our prediction equation:
#predicted prestige = 21.142 + (0.00317 x income) + (37.781 x professional) + (-0.00233 x income x professional)
#set income to 1,000, set professional to 1, check difference when income is 0
#we take our function from 1c and check differences in income of 1000
predicted_prestige(1000, 1) - predicted_prestige(0, 1)
predicted_prestige(2000, 1) - predicted_prestige(1000, 1)

#by hand:
effect_increase_1000 <- 
  (21.142 + (0.00317 * 1000) + (37.781 * 1) + (-0.00233 * 1000 * 1)) -
  (21.142 + (0.00317 * 0) + (37.781 * 1) + (-0.00233 * 0 * 1))
effect_increase_1000  
  
#(g) What is the effect of changing one’s occupations from non-professional to professional
#when her income is $6,000? We are interested in the marginal effect of professional
#jobs when the variable income takes the value of 6, 000. Calculate the change in ˆy
#based on your answer for (c).

#set income to 6,000, set professional to 1, then change to 0, check difference
#we take our function from 1c
predicted_prestige(6000, 1) - predicted_prestige(6000, 0)
effect_increase_6000 <- 
  (21.142 + (0.00317 * 6000) + (37.781 * 1) + (-0.00233 * 6000 * 1)) -
  (21.142 + (0.00317 * 6000) + (37.781 * 0) + (-0.00233 * 6000 * 0))
effect_increase_6000

#(2a) Use the results from a linear regression to determine whether having these yard signs
#in a precinct affects vote share (e.g., conduct a hypothesis test with α = .05).

#get the s-stats and p-values
#first for the t-stat do TS <- (betas - 0)/SEs
TS_a <- (0.042 - 0)/0.016
TS_a
#then for the 2-sided p-values do p_values <- 2*pt(abs(TS), n-3, lower.tail = F)
p_values_a <- 2*pt(abs(TS_a), 131 - 3, lower.tail = F)
p_values_a

#(2b)

#first for the t-stat do TS <- (betas - 0)/SEs
TS_b <- (0.042 - 0)/0.013
TS_b
#then for the 2-sided p-values do p_values <- 2*pt(abs(TS), n-3, lower.tail = F)
p_values_b <- 2*pt(abs(TS_b), 131 - 3, lower.tail = F)
p_values_b


#(2c) 

#In precincts that were neither assigned lawn signs nor were they adjacent to lawn signs: Cuccinelli was predicted to receive 0.302 of the vote share. This is the group that the sign effects were compared against.

#(2d)

#The R squared value is 0.094 which tells us that the lawn sign vairables explain about 9.4% variation in the vote share measured across 131 precincts. The rest of the variation in vote share is due to other variables that are not included in the model. While the signs have statistically significant effects they do not explain the entire difference in vote share. We know that the effect of the signs is unlikely to be zero, and the R squared value tells us how much of the variation is caused by our variable. While there are likely other factors such as political partisanship which have a far higher correlation to vote share, this does not mean that the signs are an unimportant variable. As 4% percentage points can be very significant in a close election.
