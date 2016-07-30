#### Shiyn global variables and helpers ####
library(shiny)
library(ggplot2)


# theme_shiny <- function(base_size = 12, base_family = ""){
#   theme_bw(base_size = base_size, base_family = base_family) +
#     theme(plot.background = element_rect(fill = "#f5f5f5"))
# }


# Creating the reactive values object to store attributes, probs etc
values                 <- reactiveValues()
values$attributes_id   <- 1:10
values$attributes_prob <- dnorm(1:10, mean = 6, sd = 1)
values$category_id     <- 1:8
values$category_prob   <- dexp(1:8, rate = 0.01)
