# Remove objects
rm(list=ls())

# Detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# Load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# Load any necessary packages
lapply(c("readr", "ggplot2"),  pkgTest)

# Set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Check working directory
getwd()

#Question 1: Political Science
#The following table was created using the data from a study run in a major Latin 
#American city.1 As part of the experimental treatment in the study, one employee 
#of the research team was chosen to make illegal left turns across traffic to 
#draw the attention of the police officers on shift. Two employee drivers were 
#upper class, two were lower class drivers, and the identity of the driver was 
#randomly assigned per encounter. The researchers were interested in whether 
#officers were more or less likely to solicit a bribe from drivers depending on 
#their class (officers use phrases like, “We can solve this the easy way” to draw 
#a bribe). The table below shows the resulting data.

#(a) Calculate the χ2 test statistic by hand/manually 
#(even better if you can do ”by hand” in R).

#Fe = (row total/grand total) * column total
# X^2 = Sum(Fo -Fe)^2/Fe
f1 <- 14 #(col 1 row 1) 
f2 <- 7 #(col 1 row 2)
f3 <- 6 #(col 2 row 1)
f4 <- 7 #(col 2 row 2)
f5 <- 7 #(col 3 row 1)
f6 <- 1 #(col 3 row 2)

col1 <- sum(f1,f2) #21
col2 <- sum(f3,f4) #13
col3 <- sum(f5,f6) #8
row1 <- sum(f1,f3,f5) #27
row2 <- sum(f2,f4,f6) #15
total <-  sum(f1,f2,f3,f4,f5,f6) #42

x1 <-
  ((f1 - (row1/total)*col1)^2)/((row1/total)*col1)+
  ((f2 - (row2/total)*col1)^2)/((row2/total)*col1)+
  ((f3 - (row1/total)*col2)^2)/((row1/total)*col2)+
  ((f4 - (row2/total)*col2)^2)/((row2/total)*col2)+
  ((f5 - (row1/total)*col3)^2)/((row1/total)*col3)+
  ((f6 - (row2/total)*col3)^2)/((row2/total)*col3)
x1

x2 <-
  ((14 - (27/42)*21)^2)/((27/42)*21)+
  ((7 - (15/42)*21)^2)/((15/42)*21)+
  ((6 - (27/42)*13)^2)/((27/42)*13)+
  ((7 - (15/42)*13)^2)/((15/42)*13)+
  ((7 - (27/42)*8)^2)/((27/42)*8)+
  ((1 - (15/42)*8)^2)/((15/42)*8)
x2 #doing it "by hand" without setting variables and just plugging in numbers

x1 == x2 #checking to see the calculations match
#TRUE



#slides 12 and 13 week 3

#b
bribes <- matrix(c(14,7,6,7,7,1), ncol = 3, nrow = 2) #setting the matrix
chisq.test(bribes)$p.value #pulling pvalue out of the chi squared test

dof <- (2 - 1)*(3 - 1) # df = (rows - 1)(columns - 1)
pchisq(x1, dof, lower.tail = FALSE) ##pulling pvalue out of the chi squared test

 
#we cannot reject the null hypothesis because the pval is great than our alpha 0.1

 #slides 17 and 18 week 3


#c
#adjusted residual is
# (fo - fe)/se
#se = sqrt(fe(1 - (row total/total))*(1 - (column total/total)))
f1 <- 14 #(col 1 row 1) 
f2 <- 7 #(col 1 row 2)
f3 <- 6 #(col 2 row 1)
f4 <- 7 #(col 2 row 2)
f5 <- 7 #(col 3 row 1)
f6 <- 1 #(col 3 row 2)
col1 <- sum(f1,f2) #21
col2 <- sum(f3,f4) #13
col3 <- sum(f5,f6) #8
row1 <- sum(f1,f3,f5) #27
row2 <- sum(f2,f4,f6) #15
total <-  sum(f1,f2,f3,f4,f5,f6) #42

out_ar1 <- matrix(c( #creating a matrix of the standardized residuals with my variables
  (f1-((row1/total)*col1))/sqrt(((row1/total)*col1)*(1-(row1/total))*(1-(col1/total))),
  (f2-((row2/total)*col1))/sqrt(((row2/total)*col1)*(1-(row2/total))*(1-(col1/total))),
  (f3-((row1/total)*col2))/sqrt(((row1/total)*col2)*(1-(row1/total))*(1-(col2/total))),
  (f4-((row2/total)*col2))/sqrt(((row2/total)*col2)*(1-(row2/total))*(1-(col2/total))),
  (f5-((row1/total)*col3))/sqrt(((row1/total)*col3)*(1-(row1/total))*(1-(col3/total))),
  (f6-((row2/total)*col3))/sqrt(((row2/total)*col3)*(1-(row2/total))*(1-(col3/total)))
), nrow = 2, ncol = 3)
out_ar1 

out_ar2 <- matrix(c( #creating a matrix of the standardized residuals with the numbers, but this can be harder to read
  (14-((27/42)*21))/sqrt(((27/42)*21)*(1-(27/42))*(1-(21/42))),
  (7-((15/42)*21))/sqrt(((15/42)*21)*(1-(15/42))*(1-(21/42))),
  (6-((27/42)*13))/sqrt(((27/42)*13)*(1-(27/42))*(1-(13/42))),
  (7-((15/42)*13))/sqrt(((15/42)*13)*(1-(15/42))*(1-(13/42))),
  (7-((27/42)*8))/sqrt(((27/42)*8)*(1-(27/42))*(1-(8/42))),
  (1-((15/42)*8))/sqrt(((15/42)*8)*(1-(15/42))*(1-(8/42)))
), nrow = 2, ncol= 3)
out_ar2

out_ar1 == out_ar2 #checking to see if both my matrices are equal

chisq.test(bribes)$stdres #checking my answers by extracting the standardized residuals from a chi squared test
#           [,1]      [,2]      [,3]
#[1,]  0.3220306 -1.641957  1.523026
#[2,] -0.3220306  1.641957 -1.523026 

#slide 19

#D

#the residuals show us that the lower class drivers were asked for bribes more 
#often than we would have expected and the upper class drivers show the opposite.
#we can also see that the residuals for not stopped is by far the lowest which 
#indicates that class does not have much of an effect on whether or not you get 
#pulled over we also know that in a normal distribution most values will fall 
#between -1.96 and 1.96 with a failure to reject the null hypothesis. all of the 
#values we found do fall within those values, which is consistent with our 
#failure to reject the null hypothesis

#2
#a randomized policy experiment in India, where since the mid-1990s, 1/3 of 
#village council heads have been randomly reserved for women. A subset of the 
#data from West Bengal can be found at the following link:
GP_women <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv", header=T)

head(GP_women)
str(GP_women)
summary(GP_women)

#Each observation in the data set represents a village and there are two villages 
#associated with one GP (i.e. a level of government is called ”GP”). Figure 1 
#below shows the names and descriptions of the variables in the dataset. The 
#authors hypothesize that female politicians are more likely to support policies 
#female voters want. Researchers found that more women care about the quality of 
#drinking water than men. You need to estimate the effect of the reservation 
#policy on the number of new or repaired drinking water facilities in the villages

#GP is an identifier for Gram Pantchayat
#village is an identifier for each village
#reserved is a binary variable to indicate if the GP was reserved for women
#female is a binary variable to indicate if the GP had a female leader
#irrigation is a variable that measures the number of new or repaired irrigation
            #facilities since the reserve policy started
#water is a variable that measures the number of new or repaired drinking water
            #facilities since the reserve policy started

#A

#NULL Hypothesis: H0 Beta = 0
#The reserved seats for women in GP have no effect on the number of new or repaired 
#drinking water facilities.

#Alternative (two sided) hypothesis: HA Beta != 0
#The reserved seats for women in GP have an effect on the number of new or repaired 
#drinking water facilities.

#B

# Question: Is there a relationship between women in RP and new or repaired water?
# Model: water = b0 + b1 * reserved
# Save model as object
model <- lm(water ~ reserved, data = GP_women)
summary(model)

#mean0 <- mean(GP_women$water[GP_women$reserved == 0])
#mean1 <- mean(GP_women$water[GP_women$reserved == 1])
#means <- c(mean0, mean1)

# Output interpretation:
#  - Intercept (b0): expected new facilities when there is no reserved seat
#  - Slope (b1): average difference in facilities with reserved seats

# t-test for the slope of a regression line

#t valu#t valu#t value is greater than 2

# Residual plot
#model.res <- resid(model)

#plot(model.res ~ GP_women$reserved,
#     ylab = "Residuals",
#     xlab = "Reserved seats for women, 0 = no, 1 = yes
#            red = means")
#abline(0,0)
#points(1:2, means, col = "red", pch = 18)

# Confidence intervals 
#confint(model, level=0.95)
#confint(model, level=0.99)

#the p value is less than 0.05 so we reject the NULL hypothesis.


#C
#Interpret the coefficient estimate for reservation policy
options(scipen=999)
summary(model)$coefficients
#             Estimate Std. Error  t value     Pr(>|t|)
#(Intercept) 14.738318   2.286300 6.446363 0.0000000004216474
#reserved     9.252423   3.947746 2.343723 0.0197039791691662
            
#~14.738 is intercept, ~9.252 is the slope, ~0.0197 is the pval

#The villages with their GP reserved for a woman leader produced an increase of 
#about 9.25 drinking water facilities on average. about 24 vs 15. Because the 
#reservation effect was randomly assigned, it can be read that this is the effect
#of the reservation policy. The p-value of 0.197 supports the alternative hypothesis
#that reserved seats for women in GP would result in an increase in drinking water
#facilities. It is worth noting, however, that it appears
#that the values for the women reserved GPs are being significantly drawn upwards
#by a handful of villages with very large residuals. The median of the villages
#without reserved seats is noticeably higher than than those with reserved villages.
#The outliers, however, draw the mean of the reserved villages higher. As mentioned 
#previously, the random assignment makes it likely that the results are still causal, 
#however the result should be taken cautiously and not seen as conclusive evidence.
