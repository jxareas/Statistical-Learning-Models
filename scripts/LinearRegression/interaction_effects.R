# Load Packages & Data ----------------------------------------------------

library(dplyr)
library(ggplot2)
library(ggthemes)
library(car)
library(effects)

df <- 
        as.data.frame(read.csv("./data/Carseats.csv"))


# Fitting a Linear Model with Interaction Effects -------------------------

linfit <-
        lm(
                formula = Sales ~ . + Income:Advertising  + Price:Age,
                data = df
        )

summaries <-
        S(linfit)


# Predictor Effects -------------------------------------------------------

plot(predictorEffects(mod = linfit, 
                      predictors = c("Income", "Price")
                      ))

