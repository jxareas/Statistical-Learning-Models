# Load Packages & Data ----------------------------------------------------

library(dplyr)
library(ggplot2)
library(ggthemes)
library(car)
library(reshape2)

df <- 
        as.data.frame(read.csv("./data/Boston.csv"))


# Calculating Pearson's Correlation Coeff. between each pair of variables -------------------------------------

correl_matrix <-
        Filter(is.numeric, df) |> cor(method = "pearson")

correl_matrix[lower.tri(correl_matrix, diag = T)] <- NA

correl_values <- 
        melt(correl_matrix, na.rm = T, varnames = c("x", "y"),
             value.name = "pearson")


# Heatmap of the Correlation Matrix ---------------------------------------

ggplot(data = correl_values, mapping = aes(x, y, fill = pearson)) +
        geom_tile(linejoin = "round", col = "gray") +
        labs(
                title = "Correlation Heatmap",
                x = "",
                y = "",
                fill = "Pearson's r"
        ) +
        scale_fill_gradient2(
                low = "red", mid = "white", high = "blue",
                midpoint = 0, limits = c(-1, 1), n.breaks = 5
        ) +
        theme_pander() +
        theme(
                plot.title = element_text(face = "bold", hjust = .5),
                legend.title = element_text(face = "bold", color = "dimgrey",
                                            vjust = 3),
                legend.text = element_text(color = "dimgrey"),
                axis.text.x = element_text(angle = 50, face = "bold",
                                           color = "gray2"),
                axis.text.y = element_text(face = "bold", color = "gray2")
        )


# Fitting a Multiple Linear Regression ------------------------------------

linfit <-
        lm(
                formula = medv ~ crim + zn + chas + nox + rm +
                        tax  + lstat + dis,
                data = df
        )

summaries <-
        S(linfit)


# Variance Inflation Factor of the Model Predictors -----------------------------

vif(linfit)
