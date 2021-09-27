# Load Packages & Data ----------------------------------------------------

library(dplyr)
library(ggplot2)
library(ggthemes)
library(data.table)
library(car)

df <- 
        as.data.frame(read.csv("./data/Boston.csv"))

# Fitting a Simple Linear Regression Model --------------------------------

polyfit <-
        lm(
                formula = medv ~ lstat,
                data = df
        )

summaries <-
        summary(polyfit)



# Prediction Interval for each of the Fitted Values -----------------------


prediction <-
        data.frame(
                "response" = with(df, medv),
                "predictor" = with(df, lstat),
                Predict(polyfit, interval = "prediction", 
                        level = 0.95)
        ) |> 
        mutate(between_pred_int = between(response, lwr, upr))

# Scatterplot with Prediction Band: medv - lstat -----------------------------------------------

ggplot(data = prediction) +
        geom_ribbon(
                mapping = aes(x = predictor, y = response,
                              ymin = lwr, ymax = upr),
                alpha = 0.2, fill = "blue", color = "darkblue",
                linetype = "dashed"
        ) +
        geom_point(aes(predictor, response, col = response)) +
        geom_smooth(method = lm, aes(predictor, response),
                    formula = y  ~ x, color = "green", size = 1.5) +
        labs(
                title = "Scatterplot with Prediction Band",
                x = "% of Lower Income Population",
                y = "Median House Price (1000$)"
        ) +
        scale_color_gradient(low="blue", high="red") +
        theme_light() +
        theme(
                plot.title = element_text(face = "bold", hjust = .5),
                axis.title.x = element_text(color = "dimgrey"),
                axis.title.y = element_text(color = "dimgrey"),
                legend.position = "none"
        )


# Plotting Residuals vs Fitted Values -------------------------------------

ggplot(mapping = aes(x = fitted.values(polyfit), y = resid(polyfit))) +
        geom_point() +
        geom_hline(yintercept = 0, color = "red", size = 1.5) +
        labs(
                title = "Residuals vs Fits",
                x = "Fitted Values",
                y = "Residuals"
        ) +
        theme_stata() +
        theme(
                plot.title = element_text(face = "bold", hjust = .5)
        )


# Pearson Residuals -------------------------------------------------------

residualPlot(polyfit, pch = 20, quadratic = T, main = "Versus Fits",
             col = "black")
abline(h = 0, col = "red", lwd = 2.5)
