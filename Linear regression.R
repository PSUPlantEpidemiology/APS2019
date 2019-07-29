## Background: this is an R script that illustrates just the working code
# for the examples we will primarily run. See the respective .Rmd files 
# for further details.

## R packages

library(tidyverse)
library(Hmisc)
library(corrplot)
library(readr)
library(HH)
library(car)
library(tinytex)

## Data: clean grain and aflatoxin

clean <- c(99.97, 99.94, 99.86, 99.98, 99.93, 99.81, 99.98, 99.91, 99.88, 
           99.97, 99.97, 99.8, 99.96, 99.99, 99.86, 99.96, 99.93, 99.79, 99.96, 
           99.86, 99.82, 99.97, 99.99, 99.83, 99.89, 99.96, 99.72, 99.96, 99.91, 
           99.64, 99.98, 99.86, 99.66, 99.98)
alfatoxin <- c(3, 18.8, 46.8, 4.7, 18.9, 46.8, 8.3, 21.7, 58.1, 9.3, 21.9, 62.3, 
               9.9, 22.8, 70.6, 11, 24.2, 71.1, 12.3, 25.8, 71.3, 12.5, 30.6, 83.2, 
               12.6, 36.2, 83.6, 15.9, 39.8, 99.5, 16.7, 44.3, 111.2, 18.8)

peanut <- data.frame(clean, alfatoxin)
head(peanut)

## Exploratory analysis

mean(alfatoxin)
sd(alfatoxin)
sd(alfatoxin)/mean(alfatoxin)*100

mean(clean)
sd(clean)
sd(clean)/mean(clean)*100

cor(clean, alfatoxin)
rcorr(clean, alfatoxin)

## Linear regression

# Visualizing the relationship
with(peanut, plot(x=clean, y=alfatoxin, xlim=c(99.5,100), ylim=c(0,120), pch=10)) 

# We will use lm() = linear model, to run the regression

linreg <- with(peanut, lm(alfatoxin~clean)) #Format, Y <- X
anova(linreg) #ANOVA table to see how the model fit looks
summary(linreg) #Another way to see results of the model, with a few more details. 

### Example: let's say that we are interested in comparing the slope to a known value 
## of -220, which means that for every 1% change in the percentage of clean grain, 
## the concentration of alfatoxin will be reduced by 220 ug per kg

# First, we need to see and understand where the coefficients are located, 
# especially the intercept and slope
linreg$coef
linreg$coef[1]
linreg$coef[2]

# Furthermore, where are the errors associated with each parameter
coefs <- summary(linreg)
names(coefs)
coefs$coefficients

# We can see this directly as follows: 
coefs$coefficients[1,1]
coefs$coefficients[1,2]
coefs$coefficients[2,1]
coefs$coefficients[2,2]

# Now, we will define the test parameter value for the slope
B1 <- -220

# To realize the test, we need to define the parameter value and the appropriate error 
# term 
# abs = absolute value

test_b1<-abs((coefs$coefficients[2,1]-B1)/coefs$coefficients[2,2])
test_b1

## Test statistic (two-tailed) with 32 degrees of freedom (error term) 
2*pt(q=test_b1, df=32, lower.tail=FALSE) 

## Model assumptions

## What does a simple call to plot provide?
plot(linreg)

par(mfrow=c(1,1))
plot(linreg, which=1)
plot(linreg, which=2)
plot(linreg, which=3)
plot(linreg, which=4)
plot(linreg, which=5)
plot(linreg, which=6)

## Estimation and prediction

# One challenge with predict is the need to defien a data.frame, 
# even if just for a single value, like the following example where the % clean 
# grain is 99.68. 

observation <- data.frame(clean=99.68)

predict(object=linreg, newdata=observation, interval="confidence")
predict(object=linreg, newdata=observation, interval="predict")

# We can do the same for all values in the regression. 
intervals<-predict(linreg, interval="confidence")
intervals

predictions<-predict(linreg, interval="predict")
predictions

# If we are interested in just some select values, it is easy to accomplish 
# this going back to the original single value example:
observations <- data.frame(clean=c(99.5, 99.6, 99.7, 99.8))
predict(object=linreg, newdata=observations, interval="confidence")
predict(object=linreg, newdata=observations, interval="predict")

## Additional material using the HH package

# Let's examine the regression graphically
ci.plot(linreg)

# Tools to study the assumptions

# Method to look for outliers using a Bonferroni adjustment
outlierTest(linreg) 
# Quantile-quantile plot based on Student residuals
qqPlot(linreg) 
# Influence plot in which the size of the circle is proportion to Cook's distance
influencePlot(linreg) 
# Test of homoscedasticity 
ncvTest(linreg)
# Method to verify if there is dependency in the model, which means that a 
# transformation may be appropriate to model the relationship
spreadLevelPlot(linreg) 
# Method to verify if there is evidence that the relationship is not linear
crPlots(linreg)

## Example

weight <-c(14.7, 17.8, 19.6, 18.4, 20.5, 21.1, 17.2, 18.7, 20.2, 16.0, 17.8, 19.4)
lysine <-c(0.09, 0.14, 0.18, 0.15, 0.16, 0.23, 0.11, 0.19, 0.23, 0.13, 0.17, 0.21)



