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
# lapply(c("stringr"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/incumbents_subset.csv")

#Question 1
#We are interested in knowing how the difference in campaign spending between 
#incumbent and challenger affects the incumbent’s vote share.
options(scipen = 999)
summary(inc.sub)
head(inc.sub)
str(inc.sub)

#1. Run a regression where the outcome variable is voteshare and the 
#explanatory variable is difflog.
model1 <- lm(inc.sub$voteshare ~ inc.sub$difflog)
summary(model1)
#Model1 shows us that on average for every one unit increase in difflog, we see about a 0.042 unit increase in vote share. A very small p value less than 0.05 allows us to reject the null hypothesis that slope = 0.

#2. Make a scatterplot of the two variables and add the regression line.
plot(inc.sub$difflog, inc.sub$voteshare)
abline(model1, col = "red", lwd = 2)  
cor(inc.sub$difflog, inc.sub$voteshare)
#The scatterplot shows a moderate positive correlation between difflog and voteshare, which is supported by the correlation coefficient r = 0.61. This indicates that incumbents who outspend their challengers have a tendency to get a higher percentage of the vote.

#3. Save the residuals of the model in a separate object.
model1_resid <- resid(model1)
head(model1_resid)
#The residuals are the difference between the incumbents actual vote share and their predicted vote share. These show part of voteshare that difflog did not explain. 

#4. Write the prediction equation.
#predicted voteshare = 0.579 + 0.042 * difflog
predicted_outcome1 <- 
  coef(model1)[1] + (coef(model1)[2] * inc.sub$difflog)
head(predicted_outcome1)
#On average, an incumbent who spends the same amount of the challenger (y-intercept, difflog = 0) is expected to get about a 0.579 share of the vote, and each one unit increase in difflog is predicted to raise about 0.042 units of voteshare.

#Question 2
#We are interested in knowing how the difference between incumbent and 
#challenger’s spending and the vote share of the presidential candidate of the 
#incumbent’s party are related.

#1. Run a regression where the outcome variable is presvote and the explanatory 
#variable is difflog.
model2 <- lm(inc.sub$presvote ~ inc.sub$difflog)
summary(model2)
#Model2 shows us that on average for every one unit increase in difflog, we see about a 0.024 unit increase in presvote. A very small p value less than 0.05 allows us to reject the null hypothesis that slope = 0.

#2. Make a scatterplot of the two variables and add the regression line.
plot(inc.sub$difflog, inc.sub$presvote)
abline(model2, col = "red", lwd = 2) 
cor(inc.sub$difflog, inc.sub$presvote)
#The scatterplot shows a weak to moderate positive correlation between difflog and presvote, which is supported by the correlation coefficient r = 0.3. This indicates that incumbents and challengers who spend more have a tendency for their presidential party candidate to get a higher percentage of the vote share.

#3. Save the residuals of the model in a separate object.
model2_resid <- resid(model2)
head(model2_resid)
#The residuals are the difference between the presidential candidates actual vote share and their predicted vote share. These show part of presvote that difflog did not explain. 

#4. Write the prediction equation.
#predicted presvote = 0.51 + 0.024 * difflog
predicted_outcome2 <- 
  coef(model2)[1] + (coef(model2)[2] * inc.sub$difflog)
head(predicted_outcome2)
#On average, the party presidential candidate of an incumbent who spends the same amount of the challenger (y-intercept, difflog = 0) is expected to get about a 0.51 share of the vote, and each one unit increase in difflog is predicted to raise about 0.024 units of presvote.

#Question 3
#We are interested in knowing how the vote share of the presidential candidate 
#of the incumbent’s party is associated with the incumbent’s electoral success.

#1. Run a regression where the outcome variable is voteshare and the 
#explanatory variable is presvote.

model3 <- lm(inc.sub$voteshare ~ inc.sub$presvote)
summary(model3)
#Model3 shows us that on average for every one unit increase in presvote, we see about a 0.388 unit increase in voteshare. A very small p value less than 0.05 allows us to reject the null hypothesis that slope = 0.

#2. Make a scatterplot of the two variables and add the regression line.

plot(inc.sub$presvote, inc.sub$voteshare)
abline(model3, col = "red", lwd = 2) 
cor(inc.sub$voteshare, inc.sub$presvote)
#The scatterplot shows a moderate positive correlation between voteshare and presvote, which is supported by the correlation coefficient r = 0.454. This indicates that and incumbents presidential party candidate vote share has a positive correlation with the incumbants vote share.

#3. Write the prediction equation.
#voteshare = 0.441 + 0.388 * presvote
predicted_outcome3 <- 
  coef(model3)[1] + (coef(model3)[2] * inc.sub$presvote)
head(predicted_outcome3)
#On average, the voteshare of an incumbent whos party presidential candidate who hypothetically received none of the vote (y-intercept, presvote = 0) is expected to get about a 0.441 share of the vote, and each one unit increase in presvote is predicted to raise about 0.388 units of voteshare.

#Question 4
#The residuals from part (a) tell us how much of the variation in voteshare is 
#not explained by the difference in spending between incumbent and challenger. 
#The residuals in part (b) tell us how much of the variation in presvote is not 
#explained by the difference in spending between incumbent and challenger in 
#the district.

#1. Run a regression where the outcome variable is the residuals from 
#Question 1 and the explanatory variable is the residuals from Question 2.
options(scipen=0) #I prefer scipen=999, but it runs off the page with these values, so I will revert it after this regression.
model4 <- lm(model1_resid ~ model2_resid)
summary(model4)
options(scipen=999)
#Model4 shows us that on average for every one unit increase in model2 residuals, we see about a 0.257 unit increase in model1 residuals. A very small p value less than 0.05 allows us to reject the null hypothesis that slope = 0.
#2. Make a scatterplot of the two residuals and add the regression line.

plot(model2_resid, model1_resid)
abline(model4, col = "red", lwd = 2) 
cor(model2_resid, model1_resid)
#The scatterplot shows a weak to moderate positive correlation between model1 residuals and model2 residuals, which is supported by the correlation coefficient r = 0.36. This indicates that an increase in residuals for model1 is associated with an increase in residuals for model2.

#3. Write the prediction equation.
#model2 residuals = -0.000000000000000005934 + 0.257 * model1 residuals
predicted_outcome4 <- 
  coef(model4)[1] + (coef(model4)[2] * model2_resid)
head(predicted_outcome4)
#On average, the model1 residual when model2 has no residual (y-intercept, model2 residual = 0) is expected to be basically 0, and each one unit increase in model2 residuals is predicted to raise about 0.257 units of model1 residuals. 

#Question 5
#What if the incumbent’s vote share is affected by both the president’s 
#popularity and the difference in spending between incumbent and challenger?

#1. Run a regression where the outcome variable is the incumbent’s voteshare 
#and the explanatory variables are difflog and presvote.

model5 <- lm(inc.sub$voteshare ~ inc.sub$difflog + inc.sub$presvote)
summary(model5)
#Model5 shows us that on average for every one unit increase in presvote, we see about a 0.257 unit increase in voteshare holding everything else constant. A very small p value less than 0.05 allows us to reject the null hypothesis that slope = 0. Model5 also shows us that on average for every one unit increase in difflog, we see about a 0.036 unit increase in voteshare holding everything else constant. A very small p value less than 0.05 allows us to reject the null hypothesis that slope = 0.

#2. Write the prediction equation.
#voteshare = 0.449 + (0.036 * difflog) + (0.257 * presvote)
predicted_outcome5 <-
  coef(model5)[1] + 
  (coef(model5)[2]*inc.sub$difflog) + 
  (coef(model5)[3]*inc.sub$presvote)
head(predicted_outcome5)
#On average, the voteshare when the unit for difflog and presvote is zero (y-intercept, difflog = 0, presvote = 0) is expected to be about 0.449, and each one unit increase in difflog is predicted to raise about 0.0356 units of voteshare holding all else constant and each one unit increase in presvote is expected to raise voteshare by 0.257 units holding all else constant. We also see that both of these numbers are lower than they were without running the bivariate regression, suggesting that they may be confounding variables. 

#3. What is it in this output that is identical to the output in Question 4? 
#Why do you think this is the case?

summary(model4)
summary(model5)

all.equal(summary(model4)$coefficients['model2_resid', 'Estimate'],
  summary(model5)$coefficients['inc.sub$presvote', 'Estimate'])

#The coefficient on presidential vote share in Q5 is the same as the slope in Q4. 
#in Q4 we are comparing the residuals of a regression between the vote share of a 
#house candidate to the difference in campaign spending between the incumbent 
#and the challenger with the residuals of a regression between the the presidents 
#vote share in that district to the difference in campaign spending between 
#the incumbant and the challenger. in Q5 we are running a regression between the
#vote share of a house candidate and both the difference in campaign spending 
#between the incumbant and the challengers campaign as well as the the presidents
#vote share in that district. The reason that the coefficient on presvote in Q5 
#is the same as the slope in Q4 is because in Q4 we regressed both of the variables 
#on the difference in spending and the took the residuals, which are parts of 
# of our variables presidential voteshare and candidate voteshare that cannot be 
#explained by candidate spending, while in Q5 we use a multiple regression that already
#had the difference in spending as another variable and thus the coefficient on
#presidential voteshare is already holding the difference in spending constant, 
#which in effect means that these are doing the same calculation.

summary(model4)
summary(model5)

