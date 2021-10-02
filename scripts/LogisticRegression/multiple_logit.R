# Load Packages & Data ----------------------------------------------------

library(dplyr)
library(ggplot2)
library(reshape2)

df <- 
        as.data.frame(read.csv("./data/Stocks.csv"))


# Checking for Multicollinearity -------------------------------------------

abs_corr_values <- 
        Filter(is.numeric, df) |> 
        cor(method = "pearson") |> 
        melt(value.name = "value", variable.name = c("x", "y")) |> 
        filter(value != 1) |> 
        distinct(value, .keep_all = T) |> 
        arrange(desc(abs(value)))

# Volume and Year have a Pearson Correlation Coefficient of ~ 0.53
abs_corr_values |> head(n = 5)


# Plotting Volume (Already Ordered Chronologically) -----------------------

with(df, plot(Volume))


# Fitting a Logistic Model ------------------------------------------------

# Setting the Reference Category to Down.
# The Logistic Model will predict if the market will go Up.
df <- df |> 
        within(Direction <- 
                       relevel(as.factor(Direction), ref = "Down")) # 

fit <-
        glm(
                formula = Direction ~ . - Today - Year,
                data = df,
                family = binomial(link = "logit")
        )


# Not a single predictor for the model is stastistically significant
logistic <-
        list(
                "fit" = fit,
                "summary" = S(fit),
                "probs" = predict(fit, type = "response")
        )

rm("fit")

# Prediction & Error Metrics --------------------------------------------------------------

prediction <-
        ifelse(
                with(logistic, probs) > 0.5,
                "Up",
                "Down"
        )

# Creating the Confusion Matrix, with the Fitted Value & the Response
confusion_matrix <-
        table(prediction,
              with(df, Direction)
              )

# Percentage of Correctly Predicted Values
mean(prediction == df$Direction)


