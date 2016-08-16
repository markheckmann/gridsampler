#### Shiny global variables and helpers ####
library(shiny)
library(ggplot2)
library(gridsampler)
library(shinythemes)

# Current gridsampler version (displayed in header)
gridsampler_version <- paste0("Gridsampler v", packageVersion("gridsampler"))

#### Convenience functions ####
# Convert textInput to a vector for inputs in column 3
text_to_vector <- function(txt){
  if (grepl(":", txt)) {
    # If colon notation (e.g. '1:10') is used, use that directly
    txt <- eval(parse(text = txt))
  } else if (grepl("seq\\(\\.*\\d*, \\.*\\d*, \\.*\\d*\\)", txt, perl = T)) {
    # Detect if seq() is used
    txt <- eval(parse(text = txt))
  } else {
    # Extract substrings separated by ,
    txt <- strsplit(txt, ",")[[1]]
    # Replace whitespaces probably introduced in previous step
    txt <- sub(pattern = " ", replacement = "", x = txt)
  }
  # Coercion to numeric converts text to NA
  txt <- as.numeric(txt)
  # Remove NAs introduced by superfluous , or wrong input
  txt <- txt[!is.na(txt)]
  return(txt)
}

# Prettier display of probabilities inside plots
prettify_probs <- function(x, round = 3) {
  decimals <- round(x, round)
  decimals <- substring(as.character(decimals), first = 2)
  return(decimals)
}

# Linear probability function
p_linear <- function(k, p_k = 0) {
  slope <- (2 - 2 * k * p_k) / (k * (1 - k))
  intercept <- (p_k * (1 + k) - 2) / (1 - k)
  ret <- intercept + 1:k*slope
  return(ret)
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
default_attributes_probs     <- round(dnorm(default_attributes_min:default_attributes_max,
                                      mean = default_attributes_norm_mean,
                                      sd = default_attributes_norm_sd), 3)
default_category_count       <- 15
default_category_exp_rate    <- 0.15
default_category_lin_min     <- 0.001
default_category_probs       <- round(dexp(seq_len(default_category_count), rate = default_category_exp_rate), 3)

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

