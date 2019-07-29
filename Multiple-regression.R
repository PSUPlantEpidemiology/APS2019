## Multiple regression script

# Packages
library(tidyverse)
library(Hmisc)
library(corrplot)
library(readr)
library(HH)
library(car)
library(scatterplot3d)
library(olsrr)

## Data and exploratory analysis

lot <- c(1:34)
aphids <- c(61, 77, 87, 93, 98, 100, 104, 118, 102, 74, 63, 43, 27, 
            19, 14, 23, 30, 25, 67, 40, 6, 21, 18, 23, 42, 56, 60, 59, 
            82, 89, 77, 102, 108, 97)
temperature <- c(21, 24.8, 28.3, 26, 27.5, 27.1, 26.8, 29, 28.3, 34, 30.5, 
                 28.3, 30.8, 31, 33.6, 31.8, 31.3, 33.5, 33, 34.5, 34.3, 34.3, 
                 33, 26.5, 32, 27.3, 27.8, 25.8, 25, 18.5, 26, 19, 18, 16.3)
humidity <- c(57,48, 41.5, 56, 58, 31, 36.5, 41, 40, 25, 34, 13, 37, 19, 20, 
              17, 21, 18.5, 24.5, 16, 6, 26, 21, 26, 28, 24.5, 39, 29, 41, 53.5, 
              51, 48, 70, 79.5)

aphids_data <- data.frame(lot, aphids, temperature, humidity)

# Quick exploratory analysis
summary(aphids_data)
cor(aphids_data[,2:4])
plot(aphids_data[,2:4]) # Graphical matrix
pairs(aphids_data[,2:4]) # Gives us the same thing

## Linear regression

# Factor = temperature (X)
model1<-with(aphids_data, lm(aphids~temperature))
anova(model1)
summary(model1)
plot(model1)

# Assumptions = values, model1
rstudent(model1)
dfbetas(model1)
dffits(model1)
covratio(model1)
cooks.distance(model1)

# Factor = humedad (X)
model2<-with(aphids_data, lm(aphids~humidity))
anova(model2)
summary(model2)
plot(model2)

# Assumptions = values, model2
rstudent(model2)
dfbetas(model2)
dffits(model2)
covratio(model2)
cooks.distance(model2)

## Additive multiple regression
model3<-with(aphids_data, lm(aphids~temperature+humidity))
anova(model3)
summary(model3)
plot(model3)
vif(lm(aphids~temperature+humidity, data=aphids_data))

# Assumptions = values, model3
rstudent(model3)
dfbetas(model3)
dffits(model3)
covratio(model3)
cooks.distance(model3)

## Visualizing more complicated relationships

# Start with temperature, let's add to the graph infomration about the lots
temp <- ggplot(aphids_data, aes(x=temperature, y=aphids, label=lot))
temp + geom_point() + geom_text(hjust=0.5, nudge_y=3) 
#Have a look a few of the observations like 30, 32, 33, 34 and also 8 (maybe 9)


# Now let's consider RH and do the same thing
temp2 <- ggplot(aphids_data, aes(x=humidity, y=aphids, label=lot))
temp2 + geom_point() + geom_text(hjust=0.5, nudge_y=3) 
#Maybe a bit different grouping" 6-9, 33 and 34

# In 3-dimensiones? This example comes from the package *scatterplot3d*

with(aphids_data, scatterplot3d(temperature, humidity, aphids, angle=75)) 

## Multiple regression with interactions

model4 <- with(aphids_data, lm(aphids ~ temperature + humidity + temperature:humidity))

anova(model4)
summary(model4) 
plot(model4)

# Assumptions = values, model4
rstudent(model4)
dfbetas(model4)
dffits(model4)
covratio(model4)
cooks.distance(model4)

# Graphically
ols_plot_resid_stud(model4)
ols_plot_dfbetas(model4)
ols_plot_dffits(model4)
ols_plot_cooksd_chart(model4)

# Compare the different models
anova(model1, model3) # model 3 better
anova(model2, model3) # model 2 mejor (only RH)
anova(model2, model4) # the interaction improved the model?
anova(model3, model4) # the interaction improved the model

## Predictions
# Let's start by considering the average values for temperature and relative humidity
mean(temperature)
mean(humidity)

observation <- data.frame(temperature=mean(temperature), humidity=mean(humidity))

predict(object=model4, newdata=observation, interval="confidence")
predict(object=model4, newdata=observation, interval="predict")

# Looking at all observations in the database
intervals<-predict(model4, interval="confidence")
intervals

predictions<-predict(model4, interval="predict")
predictions
