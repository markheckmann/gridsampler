#### Shiny global variables and helpers ####
library(shiny)
library(ggplot2)
library(gridsampler)
library(shinythemes)

text_to_vector <- function(txt){
  eval(parse(text = paste0("c(", txt, ")")))
}

# theme_shiny <- function(base_size = 12, base_family = ""){
#   theme_bw(base_size = base_size, base_family = base_family) +
#     theme(plot.background = element_rect(fill = "#f5f5f5"))
# }

# Plot presets
plot_bg  <- "#fafafa" # for 'plot.background'
panel_bg <- "#fcfcfc" # for 'panel.background'

# Creating the reactive values object to store attributes, probs etc
values                 <- reactiveValues()
values$attributes_id   <- 1:10
values$attributes_prob <- dnorm(1:10, mean = 6, sd = 1)
values$category_id     <- 1:15
values$category_prob   <- dexp(1:15, rate = 0.15)
values$simulations     <- NULL # Initialization for safety

# Default plot for 3,1

p_31 <- gridsampler::draw_n_person_sample(prob = dexp(1:8, 0.01),
                                       n = 10,
                                       a = 4:8,
                                       ap = dnorm(4:8, 6, 1)) +
  theme_bw() +
  theme(plot.background = element_rect(fill = plot_bg),
        panel.background = element_rect(fill = panel_bg))

# Placeholder plot for 3,2

p_32 <- ggplot(data = data.frame(x = 1:10, y = 1:10), aes(x = x, y = y)) +
          geom_line(color = panel_bg) +
          geom_text(label = "No simulations run yet.", y = 5, x = 5, size = 8) +
          theme_void() +
          theme(plot.background = element_rect(fill = plot_bg),
                panel.background = element_rect(fill = panel_bg))

