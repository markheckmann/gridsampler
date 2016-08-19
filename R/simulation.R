#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#               FUNCTIONS TO SIMULATE SAMPLING CONSTRUCTS
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#' Simulate a single grid
#'
#' @param prob Probability to draw a construct from a certain category.
#' @param a Number of constructs to be sampled.
#' @export
#' @family Simulations
#' @examples
#' # draw from exponential distribution
#' p <- dexp(1:20, rate = .1)
#' sim_one_person(p, a = 10)
#'
sim_one_person <- function(prob, a = 10)
{
  if (a > sum(prob > 0))
    stop("The number of attributes 'a' must not exceed ",
         "the number of categories, i.e. length of 'prob'", call. = FALSE)
  # d.all <- NULL
  # for (i in 1:a) {
  #   prob <- prob / sum(prob)                # norm sum of probabilities to 1
  #   d <- stats::rmultinom(1, size = 1, prob = prob)  # draw from multinomial distribution
  #   d.index <- which(d == 1)
  #   prob[d.index] <- 0
  #   d.all <- cbind(d.all, d)
  # }
  # apply(d.all, 1, sum)

  # simpler code
  ii      <- sample(seq_along(prob), size = a, prob = prob, replace = FALSE)
  res     <- rep(0, length(prob))
  res[ii] <- 1
  res
}


#' Simulate n persons
#'
#' Function is a simple replicate wrapper around \code{sim_one_person}
#'
#' @inheritParams sim_one_person
#' @param a Possible number of attributes sampled from.
#' @param n Number of persons, i.e. grids to be sampled.
#' @param ap Attribute probabilities, i.e. for each number of attributes given
#'   in \code{a}.
#' @export
#' @family Simulations
#' @examples
#' sim_n_persons(dexp(1:30, .05), n = 2, a = 10)
#' sim_n_persons(dexp(1:30, .05), n = 2, a = c(1, 30))
#' sim_n_persons(dexp(1:30, .05), n = 2, a = c(1, 30), ap = c(1,4))
#' sim_n_persons(dexp(1:30, .05), n = 2, a = 1:5, ap = c(1,1,2,2,3))
#'
sim_n_persons <- function(prob, n, a=10, ap=rep(1/length(a), length(a)))
{
  if (length(a) != length(ap)) {
    stop("the number of attributes must match the attribute probabilites")
  }

  ap  <- ap/sum(ap)  # make sure sum of ap equals one (probability)
  sim <- replicate(n, sim_one_person(prob, a = sample_new(a, 1, prob = ap)) )
  apply(sim, 1, sum)
}


#' Produce graphic for a single sample of n persons
#' @inheritParams sim_n_persons
#' @export
#' @import ggplot2
#' @family Plotting
#' @examples
#' draw_n_person_sample(dexp(1:30, rate = .05), n = 100, a = 10)
#' draw_n_person_sample(dexp(1:30, rate = .05), n = 100, a = 1:5, ap = 5:1)
#
draw_n_person_sample <- function(prob, n, a = 10, ap = rep(1/length(a), length(a)))
{
  res <- sim_n_persons(prob, n, a, ap)
  df  <- data.frame(no = seq_along(res), cat = res)
  g   <- ggplot(df, aes_string(x = "no", y = "cat")) +
                geom_line() +
                geom_point() +
                scale_x_continuous(name = "Category") +
                scale_y_continuous(name = "Counts", limits = c(0, max(res)))
  return(g)
}


#' Complete simulation
#'
#' @param prob Probability to draw a construct from a certain category. Length
#'   of vector determines number of categories.
#' @param n Number of persons, i.e. grids to sample.
#' @param a Number of constructs to be sampled.
#' @param ap Probabilities for each number of attributes to be sampled.
#' @param times Number of times to repeat each simulation.
#' @param progress Type of progress bar shown during simulation.
#' @export
#' @family Simulations
#' @examples
#' \dontrun{
#' sim_n_persons_x_times(dexp(1:30, .05), n = 2, a = c(1,30), ap = 1:2, times = 100)
#' sim_n_persons_x_times(dexp(1:30, .05), n = 2, a = c(1,30), times = 200, progress = "tk")
#' }
sim_n_persons_x_times <- function(prob, n, a, ap = rep(1/length(a),
                                  length(a)), times = 100, progress = "text")
{
  if (!interactive()) {
    progress <- "none"
  }
  plyr::ldply(1L:times, function(x, prob, n, a, ap){
          sim_n_persons(prob, n, a, ap)
        }, prob = prob, n = n, a = a, ap = ap, .progress = progress)
}


#' Produce ggplot of percentiles for simulated frequencies
#'
#' @param r A dataframe. The result returned from \code{\link{sim_n_persons_x_times}}.
#' @return Draws a ggplot
#' @export
#' @import ggplot2
#' @importFrom stats quantile
#' @family Utilities
#' @examples
#' r <- sim_n_persons_x_times(dexp(1:30, rate = .05), n = 50, a = 5:7, ap = 1:3, 100)
#' expected_frequencies(r)
#'
expected_frequencies <- function(r)
{
  variable <- NULL   # declare to avoid causes CRAN check note
  co <- t(apply(r, 2, quantile, probs = c(.05, .25, .5, .75, .95)))
  df <- cbind(cat = 1L:nrow(co), as.data.frame(co))
  df.melted <- reshape2::melt(df, id.vars = "cat")
  df.melted$variable <- as.factor(df.melted$variable)
  mval <- max(df.melted$value)
  #s <- subset(df.melted, variable != "50%")  # avoid NSE as it causes a note in CRAN check
  s <- df.melted[df.melted$variable != "50%", , drop = FALSE]
  g <- ggplot(s,
              aes_string(x = "cat", y = "value", group = "variable", shape = "variable")) +
              geom_line() +
              geom_point() +
              geom_line(data = subset(df.melted, variable == "50%"), col = "blue") +
              geom_point(data = subset(df.melted, variable == "50%"), col = "blue") +
              scale_y_continuous("Counts", limits = c(0, mval)) +
              scale_x_continuous(name = "Category") +
              scale_shape_discrete(breaks = levels(s$variable), name = "Percentiles")
  return(g)
}


#' Probability for certain degree of saturation
#'
#' Calculate probability for getting certain proportion of categories with at
#' least m constructs
#'
#' @param r A dataframe. The result returned from \code{\link{sim_n_persons_x_times}}.
#' @param m minimal number of constructs in each category
#' @param min.prop Proportion of categores to contain at least m constructs.
#' @export
#' @family Utilities
#' @examples
#' r <- sim_n_persons_x_times(dexp(1:30, rate = .05), n = 50, a = 5:7, times = 100, progress = "none")
#' prob_categories(r, 4, min.prop = .9)
#'
prob_categories <- function(r, m, min.prop=1)
{
  s <- apply(r, 1, function(x, min.prop) {      # does the sample render more than
         (sum(x >= m) / length(x)) >= min.prop  # min.prop categories with >=  m attributes
       }, min.prop = min.prop)
  sum(s) / nrow(r)
}


#' Simulate for different n
#'
#' Creates simulation results for different n. Runs
#' \code{\link{sim_n_persons_x_times}} for different n.
#'
#' @inheritParams sim_n_persons_x_times
#' @return A result dataframe.
#' @export
#' @family Simulations
#' @examples
#' \dontrun{
#' r <- sim_n_persons_x_times_many_n(dexp(1:30, .05), a = 7, times = 100)
#' r <- sim_n_persons_x_times_many_n(dexp(1:30, .05), a = 5:7, ap = 1:3, times = 100)
#' }
sim_n_persons_x_times_many_n <- function(prob, n = seq(10, 80, by = 10), a = 7,
                               ap = rep(1/length(a), length(a)), times = 100,
                               progress = "text")
{
  if (!interactive()) {
    progress <- "none"
  }
  r <- list()
  for (i in seq_along(n))
    r[[i]] <- sim_n_persons_x_times(prob, n = n[i], a = a, ap = ap, times = times,
                                    progress = progress)
  r
}


#' Probability for certain degree of saturation
#'
#' Calculate probability for getting certain proportion of categories with at
#' least m constructs
#'
#' @param r A dataframe. The result returned from \code{\link{sim_n_persons_x_times_many_n}}.
#' @param n Vector of n for which to calculate probabilities.
#' @param ms minimal number of constructs in each category
#' @param min.props Proportion of categores to contain at least m constructs.
#' @export
#' @family Utilities
#' @examples
#' prob <-  dexp(1:30, .05)
#' n <- seq(10, 80, by = 20)
#' r <- sim_n_persons_x_times_many_n(prob, n, a = 7, times = 100)
#' dd <- calc_probabilities(r, n, ms=1:5, min.props = c(0.9, .95, 1))
#' head(dd)
#'
calc_probabilities <- function(r, n, ms, min.props = c(.9, .95, .99))
{
  res <- NULL
  for (m in ms) {
    for (min.prop in min.props) {
      probs  <- sapply(r, prob_categories, m = m, min = min.prop)
      res.df <- cbind(n = n, m = m, min.prop = min.prop, prob = probs)
      res    <- append(res, list(res.df))
    }
  }
  dd <- as.data.frame(do.call(rbind, res))
  dd
}


#' Draw and redraw results of simulation
#'
#' @param d A dataframe as returned by \code{\link{calc_probabilities}}.
#' @export
#' @family Plotting
#' @import ggplot2
#' @examples
#' ## simulate
#' prob <-  dexp(1:30, .05)      # probabilities for categories
#' N <- seq(10, 80, by = 10)     # smaple sizes to simulate
#' r <- sim_n_persons_x_times_many_n(prob, n = N, a = 7, times = 100, progress = "none")
#'
#' # calculate and draw
#' M <- 1:5                      # minimal number of categories to evaluate
#' p <- c(0.9, .95, 1)           # proportion of categories for which minimal m holds
#' d <- calc_probabilities(r, n = N, ms = M, min.props = p)
#' draw_multiple_n_persons_x_times(d)
#
draw_multiple_n_persons_x_times <- function(d)
{
 # dd <- calc_probabilities(r=res, n=n, ms=ms, min.props=min.props)
 d$m <- as.factor(d$m)
 d$min.prop.k <- paste0("C = ", d$min.prop)
 g <- ggplot(d, aes_string(x = "n", y = "prob", group = "m", shape = "m", color = "m")) +
   geom_line() +
   geom_point() +
   scale_y_continuous("Probability", limits = c(0, 1)) +
   scale_x_continuous("Sample Size (N)") +
   facet_grid(. ~ min.prop.k) +
   scale_color_discrete(name = "M: Min.\nCount") +
   scale_shape_discrete(name = "M: Min.\nCount")
 return(g)
}

