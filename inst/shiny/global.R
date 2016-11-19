#### Shiny global variables and helpers ####
library(shiny)
library(ggplot2)
library(gridsampler)
library(shinythemes)
library(shinyBS)

#### UI variables defined here for convenience ####
# Current gridsampler version (displayed in header)
gridsampler_version <- paste0("gridsampler v", packageVersion("gridsampler"))

# Column descriptions
desc_col1 <- helpText("")
desc_col2 <- helpText("")
desc_col3 <- helpText("")

#### Convenience functions ####
# Convert textInput to a vector for inputs in column 3
string_numeric <- function(s) {
  # wrap in c to get numeric vector for vectors
  # seperated by comma e.g. 1:3, 3:4
  s <- paste0("c(", s, ")")

  # can string be evaluated without failing?
  v <- tryCatch(
    eval(parse(text = s)),
    error = function(e) simpleError("no valid R expression")
  )

  # evaluates to numeric vector?
  list(check = is.numeric(v) & is.vector(v),
       vector = v)
}

text_to_vector <- function(txt){

  check <- string_numeric(txt)

  if (!check$check) {
    # Return a non-breaking but obviously non-useful 1 on error
    return(0)
  } else {
    # Coercion to numeric converts text to NA
    vec <- as.numeric(check$vector)
    return(vec)
  }
}

# Prettier display of probabilities inside plots, 0.927724 -> '.928'
prettify_probs <- function(x, round = 3) {
  decimals <- round(x, round)
  decimals <- substring(as.character(decimals), first = 2)
  return(decimals)
}

# Linear probability function for panel 2, p_k defines the minimum probability allowed
p_linear <- function(k, p_k = 0) {
  slope     <- (2 - 2 * k * p_k) / (k * (1 - k))
  intercept <- (p_k * (1 + k) - 2) / (1 - k)
  ret       <- intercept + 1:k * slope
  return(ret)
}

# seq that handles NAs, used in column 3 for faulty input handling
seq_robust <- function(from = 1, to = 10, by = 1, ...) {
  if (is.na(from)) {
    from <- 4
  }
  if (is.na(to)) {
    to <- 8
  }
  s <- seq(from, to, by, ...)
  return(s)
}

# seq_len that handles NA, see seq_robust
seq_len_robust <- function(length.out = 10) {
  if (is.na(length.out)) {
   length.out <- 15
  }
  s <- seq_len(length.out)
  return(s)
}

##### Initializing vectors and default prob arguments ####
default_attributes_min <- 4
default_attributes_max <- 8
default_attributes     <- seq(default_attributes_min, default_attributes_max)
default_attributes_norm_mean <- 6
default_attributes_norm_sd   <- 1
default_attributes_lambda    <- 6
default_attributes_exp_rate  <- 0.1
default_attributes_probs     <- dnorm(default_attributes,
                                      mean = default_attributes_norm_mean,
                                      sd = default_attributes_norm_sd)
default_attributes_probs     <- default_attributes_probs / sum(default_attributes_probs)
default_category_count       <- 15
default_category_exp_rate    <- 0.15
default_category_lin_min     <- 0.001
default_category_probs       <- dexp(seq_len(default_category_count),
                                           rate = default_category_exp_rate)
default_category_probs       <- default_category_probs / sum(default_category_probs)

# Creating the reactive values object to store attributes, probs etc
values                 <- reactiveValues()
values$attributes_id   <- default_attributes
values$attributes_prob <- round(dnorm(default_attributes,
                                      mean = default_attributes_norm_mean,
                                      sd = default_attributes_norm_sd), 3)
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
                                          a = default_attributes,
                                          ap = default_attributes_probs) +
          theme_bw() +
          theme(plot.background  = element_rect(fill = plot_bg),
                panel.background = element_rect(fill = panel_bg))

#### Tooltips ####
# See tooltip options: http://www.w3schools.com/bootstrap/bootstrap_ref_js_tooltip.asp
tooltip_opts <- list(delay = list(show = 1000, hide = 100))
