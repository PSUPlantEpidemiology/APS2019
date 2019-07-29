## R package installation

# We have options to control the location where we download packages. 
# chooseCRANmirror() # Provides a list of mirror sites from which you select one by entering the number

chooseCRANmirror(ind=61) #USA (IA)

# Perspective: how many packages are there currently available with R?
ej <- available.packages()
head(rownames(ej), 10)
length(ej[,1]) 

install.packages("tidyverse")
install.packages("Hmisc")
install.packages("corrplot")
install.packages("readr")
install.packages("HH")
install.packages("car")
install.packages("scatterplot3d")
install.packages("leaps")
install.packages("purr")
install.packages("dplyr")
install.packages("magritter")
install.packages("readxl")
install.packages("drc")
install.packages("cowplot")
install.packages("olsrr")