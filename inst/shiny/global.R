#### Shiny global variables and helpers ####
library(shiny)
library(ggplot2)
library(gridsampler)
library(shinythemes)

#### UI variables defined here for convenience ####
# Current gridsampler version (displayed in header)
gridsampler_version <- paste0("gridsampler v", packageVersion("gridsampler"))

# Column descriptions
desc_col1 <- helpText("")
desc_col2 <- helpText("")
desc_col3 <- helpText("")

#### Convenience functions ####
# Convert textInput to a vector for inputs in column 3
text_to_vector <- function(txt){
  if (grepl(":", txt) & grepl(",", txt)) {
    txt <- strsplit(txt, ",")[[1]]
    txt1 <- text_to_vector(txt[grepl(pattern = ":", txt)])
    txt2 <- txt[!grepl(pattern = ":", txt)]
    txt <- c(txt1, txt2)
  } else if (grepl(":", txt)) {
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
default_attributes_probs     <- round(dnorm(default_attributes,
                                      mean = default_attributes_norm_mean,
                                      sd = default_attributes_norm_sd), 3)
default_category_count       <- 15
default_category_exp_rate    <- 0.15
default_category_lin_min     <- 0.001
default_category_probs       <- round(dexp(seq_len(default_category_count),
                                           rate = default_category_exp_rate), 3)

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

