# Load Packages & Data ----------------------------------------------------

library(dplyr)
library(ggplot2)
library(ggthemes)
library(car)
library(gridExtra)

df <- 
        as.data.frame(read.csv("./data/Default.csv")) |> 
        mutate(dummy_default = as.integer(default == "Yes")) # Setting Default as the Dependent Variable


# Fitting & Plotting a Linear Model -------------------------------------------------------

lm_fit <-
        lm(
                formula = dummy_default ~ balance, 
                data = df
        )

lm_plot <-
        ggplot(data = df, mapping = aes(x = balance, y = dummy_default,
                                        col = default)) +
        geom_point() +
        geom_smooth(method = glm, formula = y ~ x, col = "magenta") +
        labs(
                title = "Linear Model",
                x = "Balance",
                y = "Default"
                
        ) +
        theme_stata() +
        theme(
                plot.title = element_text(face = "bold", hjust = .5),
                legend.position = "none"
        ) 

linear <-
        list(
                "fit" = lm_fit,
                "plot" = lm_plot,
                "summary" = S(lm_fit)
        )

rm(list=c("lm_fit", "lm_plot"))

# Binary Logistic Regression -------------------------------------------------------------------------

logit_fit <-
        glm(
                formula = dummy_default ~ balance,
                data = df,
                family = binomial(link = "logit")
        )

logit_plot <- 
        ggplot(data = df, mapping = aes(x = balance, y = dummy_default,
                                col = factor(dummy_default))) +
        geom_point() +
        geom_smooth(method = glm, formula =  y ~ x,
                    method.args = list(family = binomial), col = "magenta",
                    size = 1.2) +
        labs(
                title = "Logistic Model",
                x = "Balance",
                y = "Probability of Default"
                
        ) +
        theme_stata() +
        theme(
                plot.title = element_text(face = "bold", hjust = .5),
                legend.position = "none"
        ) 

logistic <- list("fit" = logit_fit,
                 "plot" = logit_plot,
                 "summary" = S(logit_fit)
                 )

rm(list=c("logit_fit", "logit_plot"))


# Comparing side-by-side fits --------------------------------------------------------

grid.arrange(linear$plot, logistic$plot, ncol = 2)


# Prediction with the Logit Model -----------------------------------------

# Predicting the Expected Value / Response Variable / Probability
with(logistic, fit) |> 
        predict.glm(type = "response")

# Predicting the Linear Predictor / Log-Odds
with(logistic, fit) |> 
        predict.glm(type = "link")


