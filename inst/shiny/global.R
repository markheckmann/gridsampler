#### Shiyn global variables and helpers ####
library(shiny)
library(ggplot2)

text_to_vector <- function(txt){
  eval(parse(text = paste0("c(", txt, ")")))
}

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


# Default plot for 3,1

p_31 <- gridsampler::draw_n_person_sample(prob = dexp(1:8, 0.01),
                                       n = 10,
                                       a = 4:8,
                                       ap = dnorm(4:8, 6, 1)) +
  theme_bw() +
  theme(plot.background = element_rect(fill = "#f5f5f5"),
        panel.background = element_rect(fill = "#f5f5f5"))
