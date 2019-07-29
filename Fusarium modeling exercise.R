## Modeling example: Fusarium and small grains

# Packages
library(tidyverse)
library(Hmisc)
library(corrplot)
library(readr)
library(HH)
library(car)
library(scatterplot3d)
library(leaps)
library(olsrr)

## Database contains the following: 
## lot = represents an individual plot level observation
## yield = kilograms per hectare 
## fdk = fusarium damaged kernels (%)
## incidence = incidence of Fusarium head blight (%)
## severity = severity of Fusarium head blight in heads (%)
## moisture = grain moisture (%)
## don = concentration of vomitoxin (ppm)

### Exploratory analysis

head(fusarium)
summary(fusarium)

correlations <- fusarium %>% 
  dplyr::select(-lot) %>%
  as.matrix() %>%
  cor(method = "pearson")

corrplot(correlations, method="number")
corrplot(correlations, method="circle")

### Define null and full models
model_null <- lm(yield~1, data=fusarium)

# For full model, please note that there are far too many parameters than observations
model_full <- lm(yield~fdk*incidence*severity*moisture*don, data=fusarium)

## Stepwise
forward <- step(model_null, scope=list(lower=model_null, upper=model_full, direction="forward"))
backward <- step(model_null, scope=list(lower=model_null, upper=model_full, direction="backward"))
both <- step(model_null, scope=list(lower=model_null, upper=model_full, direction="both"))

## Best subsets

subsets <- regsubsets(yield~fdk+incidence+severity+moisture+don,
                           nbest=2, data=fusarium)

plot(subsets, scale="adjr2")
plot(subsets, scale="bic")
plot(subsets, scale="Cp")
