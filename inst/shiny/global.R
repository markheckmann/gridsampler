#### Shiny global variables and helpers ####
library(shiny)
library(ggplot2)
library(gridsampler)
library(shinythemes)
# Only load ggrepel if it's available to avoid hard dependency for minor tweak
if (!("ggrepel" %in% installed.packages())) {
  message("Package 'ggrepel' not installed, can't drav non-overlapping labels")
  plot_repel <- FALSE
} else {
  plot_repel <- TRUE
}

#### Convenience functions ####
# Convert textInput to a vector for inputs in column 3
text_to_vector <- function(txt){
  eval(parse(text = paste0("c(", txt, ")")))
}

# Prettier display of probabilities inside plots
prettify_probs <- function(x, round = 3) {
  decimals <- round(x, round)
  decimals <- substring(as.character(decimals), first = 2)
  return(decimals)
}

# This should have been a global ggplot2 theme object for all plots
# theme_shiny <- function(base_size = 12, base_family = ""){
#   theme_bw(base_size = base_size, base_family = base_family) +
#     theme(plot.background = element_rect(fill = "#f5f5f5"))
# }

##### Initializing vectors and default prob arguments ####
default_attributes_min <- 4
default_attributes_max <- 8
default_attributes_norm_mean <- 6
default_attributes_norm_sd   <- 1
default_attributes_lambda    <- 6
default_attributes_exp_rate  <- 0.1
default_attributes_probs     <- dnorm(default_attributes_min:default_attributes_max,
                                      mean = default_attributes_norm_mean,
                                      sd = default_attributes_norm_sd)
default_category_count     <- 15
default_category_exp_rate  <- 0.15
default_category_lambda    <- 6
default_category_norm_mean <- 5
default_category_norm_sd   <- 1
default_category_probs     <- dexp(seq_len(default_category_count), rate = default_category_exp_rate)

# Creating the reactive values object to store attributes, probs etc
values                 <- reactiveValues()
values$attributes_id   <- default_attributes_min:default_attributes_max
values$attributes_prob <- dnorm(default_attributes_min:default_attributes_max,
                                mean = default_attributes_norm_mean,
                                sd = default_attributes_norm_sd)
values$category_id     <- seq_len(default_category_count)
values$category_prob   <- default_category_probs
values$simulations     <- NULL # Initialization for safety

#### Plot presets ####
plot_bg   <- "#fafafa" # for 'plot.background'
panel_bg  <- "#fcfcfc" # for 'panel.background'
legend_bg <- plot_bg   # for 'legend.background'

# Default plot for 3,1
p_31 <- gridsampler::draw_n_person_sample(prob = default_category_probs,
                                          n = 10,
                                          a = default_attributes_min:default_attributes_max,
                                          ap = default_attributes_probs) +
  theme_bw() +
  theme(plot.background  = element_rect(fill = plot_bg),
        panel.background = element_rect(fill = panel_bg))

