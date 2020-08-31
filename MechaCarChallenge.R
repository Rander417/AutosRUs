library(jsonlite)
library(tidyverse)
library(ggplot2)

#import data
MechaCarData <- read.csv('MechaCar_mpg.csv',stringsAsFactors = F)
suspensionCoilData <- read.csv('Suspension_Coil.csv',stringsAsFactors = F)

# *************************
# PART I MPG Regression

# Correlation matrix for visibility of relationships in MechaCar_mpg data
MechaMatrix <- as.matrix(MechaCarData[,c("vehicle_length","vehicle_weight","spoiler_angle", "ground_clearance","AWD", "mpg")]) #convert data frame into numeric matrix
cor(MechaMatrix)

# Multiple linear regression predicting the mpg
Mecha_multLin <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=MechaCarData)
summary(Mecha_multLin)


# *************************
# PART II Suspension Coil Summary

#Summary table
coil_summary <- suspensionCoilData %>% group_by(Manufacturing_Lot) %>% summarize(Mean_PSI=mean(PSI), Median_PSI=median(PSI), Variance_PSI = var(PSI), Standard_Deviation = sd(PSI))


# *************************
# PART III Suspension Coil T-Test

# one-sample - determine if the suspension coil's pound-per-inch results are statistically different from
# the mean population results of 1,500 pounds per inch
t.test(suspensionCoilData$PSI, mu=1500)


