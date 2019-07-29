## Regression modeling tools

## Packages

library(tidyverse)
library(Hmisc)
library(corrplot)
library(readr)
library(HH)
library(car)
library(scatterplot3d)
library(leaps)
library(olsrr)

## Data

lot <- c(1:34)
aphids <- c(61, 77, 87, 93, 98, 100, 104, 118, 102, 74, 63, 43, 27, 19, 14, 23, 
            30, 25, 67, 40, 6, 21, 18, 23, 42, 56, 60, 59, 82, 89, 77, 102, 108, 97)
temperature <- c(21, 24.8, 28.3, 26, 27.5, 27.1, 26.8, 29, 28.3, 34, 30.5, 28.3, 
                 30.8, 31, 33.6, 31.8, 31.3, 33.5, 33, 34.5, 34.3, 34.3, 33, 26.5, 
                 32, 27.3, 27.8, 25.8, 25, 18.5, 26, 19, 18, 16.3)
humidity <- c(57,48, 41.5, 56, 58, 31, 36.5, 41, 40, 25, 34, 13, 37, 19, 20, 17, 
              21, 18.5, 24.5, 16, 6, 26, 21, 26, 28, 24.5, 39, 29, 41, 53.5, 51, 
              48, 70, 79.5)

aphids_data <- data.frame(lot, aphids, temperature, humidity)

## Basic model

model_a <- with(aphids_data, lm(aphids ~ temperature + humidity))
anova(model_a) # both factors are significant
summary(model_a) #R^2 = 0.55

plot(model_a)
plot(model_a, which=4) 

## Full model

model_b <- with(aphids_data, lm(aphids ~ temperature + humidity + I(temperature^2) + 
                                  I(humidity^2) + temperature:humidity))
anova(model_b) # significant factors: temperature, humidity, I(temperature^2)
summary(model_b) #R^2 = 0.63, adjusted R^2 = 0.5617

plot(model_b)
plot(model_b, which=4)

## Model comparison

# anova(a,b), enables comparision between nested models, based on the number 
# of additional parameters

anova(model_a, model_b) 

# What the results indicates is that a full model does not explain better the 
# relationship, probably due to being an over-adjusted model. 
# This does not mean that they may not be a model that better explains the 
# relationship that is less complicated. 

## Modeling: three methods under consideration, manual, stepwise, and best subsets

# Manual, we will start with Model B in this situation and try to reduce the 
# complexity of the model. 

# Initial step: eliminate the factor, temperature:humidity
model_b2 <- update(model_b, .~.-temperature:humidity)
summary(model_b2)
anova(model_b, model_b2) ## temperature:humidity = non-significant

# From model b2, we will now eliminate the factor I(humidity^2)
model_b3 <- update(model_b2, .~.-I(humidity^2))
summary(model_b3) # it appears that all factors explain something

# Compare the models b2 and b3
anova(model_b2, model_b3) # I(humidity^2) = not signficant 
# Compare with baseline model
anova(model_a, model_b3) ## this model is close to p=0,05 and probably has 
# some predictive value

# From model b2 (no interaction), we will now remove temperature to 
# look at the humidity^2 term
model_b3t <- update(model_b2, .~.-I(temperature^2))
summary(model_b3t) # it appears that all factors explain something
anova(model_a, model_b3t)
anova(model_b2, model_b3t)

# From model b3, remove temperature^2 (can do the same with model_b3t)
model_b4 <- update(model_b3, .~.-I(temperature^2))
anova(model_b4)
summary(model_b4)
anova(model_b3, model_b4) 

# You can continue reducing the model, but we do know now that there is some 
# predictive value with the variables we have 

# For the moment and seeing the results, b3 may be the best of the models. Agree?

## Stepwise methods

# Null model
model_null <- lm(aphids~1, data=aphids_data)
model_full <- model_b

# Forward
forward <- step(model_null, scope=list(lower=model_null, upper=model_full, 
                                       direction="forward")) 

# Backwards
back <- step(model_null, scope=list(lower=model_null, upper=model_full, 
                                    direction="backward"))  

# Both directions 
back <- step(model_null, scope=list(lower=model_null, upper=model_full, 
                                    direction="both"))

# Let's go back to our manual approach and look just humidity 
model_b5<-with(aphids_data, lm(aphids~humidity+I(humidity^2)))
anova(model_b5)
summary(model_b5)

## Best subsets

# regsubsets = package *leaps*

# let's start by looking at the best 3 models per level
subsets <- regsubsets(aphids~temperature+humidity+I(temperature^2)+
                        I(humidity^2)+temperature:humidity, nbest=3, data=aphids_data)

plot(subsets, scale="adjr2")
plot(subsets, scale="bic")
plot(subsets, scale="Cp")

