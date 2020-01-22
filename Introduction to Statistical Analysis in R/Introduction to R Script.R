### Introduction to Statistical Analysis in R
### Melissa Whatley
### December 6, 2019

##BASIC MATH IN R##
#A#
-7*(2^3)

#B#
8/((8^2)+1)

#C#
sqrt(81)

#D#
log(4)

#setwd("C:/Users/mewhatle/Dropbox/Teaching Materials/Introduction to R/Workshop")
setwd("/Users/melwhat/Dropbox/Teaching Materials/Introduction to R/Workshop")
getwd()

##READING DATA##

#dataname<-read.csv("example_data.csv")

##INSTALLING PACKAGES##

install.packages("foreign")
library(foreign)

ipeds<-read.dta("ipeds_data.dta")

##VIEWING YOUR DATA##

#Full data frame (not recommended for large datasets!)
ipeds

#The first few observations
head(ipeds)

#The last few observations
tail(ipeds)

#A quick list of variable names
names(ipeds)



##SUMMARY STATISTICS AND BASIC REGRESSION FUNCTIONS##

# Use this following code to turn off scientific notation:
#options(scipen=999)

#Descriptive statistics of all variables
summary(ipeds)

#The psych package provides descriptives that, in my opinion, are easier to read
install.packages("psych")
library(psych)
describe(ipeds)

#Descriptive statistics of a single variable
#R's native command
summary(ipeds$GradRate)

#Psych package
describe(ipeds$GradRate)

# Pretty tables
install.packages("stargazer")
library(stargazer)

stargazer(ipeds, type="text")

# What predicts the amount that an institution receives in state appropriations?

ols.model<-lm(StateApprop1000~ GradRate+TuitFeeRev1000+PctWhite+PctPell+Rural+Town+Suburb+Size4999less, data=ipeds)
# Note that "City" is the comparison group for urbanicity
summary(ols.model)

# What predicts whether an institution has an above-average graduation rate?

logit.model<-glm(GradRateAboveAverage~StateApprop1000+TuitFeeRev1000+PctWhite+PctPell+Rural+Town+Suburb+Size4999less, data=ipeds, family=binomial(link="logit"))
summary(logit.model)

#Note that align=TRUE makes sure that the decimal points are aligned
stargazer(ols.model, logit.model, title="Example Results", align=TRUE, type="text")
