library(gridsampler)
library(BiasedUrn)

context("Sampling plausibility")


test_that("Correct number of attributes is returned", {
  p <- 10:1                               # arbitrary weights for attribute
  s1 <- sim_one_person(p, a=length(p))    # number of attributes is equal to number of categories
  expect_true(all(s1 == 1))               # hence, we should get one attribute from each category
})



# Drawing a fixed number of elements from a finite population without replacement,
# where a) the elements have unequal probabilities of being drawn, and b) the 
# drawing of one element affects the probability of the remaining, leads to the 
# multivariate Wallenius' noncentral hypergeometric distribution (MWNHD). 
# Here we test if our sampling works correctly and the results converge to 
# the theoretical distribution.

test_that("Converges to theoretical MWNHD means for fixed number of attributes", {
  
  # use ten random samples to check if our sampling works correctly
  set.seed(0)
  res <- NA
  for (i in 1:10) {
    
    # cat("\rrun: ", i)
    # flush.console()
    
    N <- sample(1:7, 1)                     # number of attributes
    n.cat <- sample(8:20, 1)                # number of categories
    P <- dexp(1L:n.cat, .1)                 # category probabilities
    f <- sim_n_persons(P, n = 1e5, a = N)   # frequency of each category
    h <- f / sum(f)                         # relative frequencies
    mu.sim <- h * N                         # times number of attributes drawn
    
    # theoretical
    M <- rep(1, length(P))                  # initial nuber of balls in urn 
    mu <- BiasedUrn::meanMWNCHypergeo(m = M, n=N, odds = P, precision = 0)  # Wallenius' means
    
    # compare
    res[i] <- all.equal.numeric(target = mu,
                                current = mu.sim,
                                check.attributes = FALSE, use.names = FALSE,
                                tolerance = 0.01)
  }
  
  expect_true(all(res))
  
})


# If the number of attributes is not fixed, but is itself a random variable,
# making the situation more complex. Hence, the resulting distribution is a
# compound distribution with the number of attributes being a random variable
# used as a hyper-parameter in the second sampling stage.
# Here we test for the special case, if our sampling works correctly and the
# results converge to the theoretical compound distribution.

test_that("Covergence to theoretical MWNHD means for random number of attributes", {

  skip_on_cran()
  
  # Use ten random samples to check if our sampling works correctly.
  # Calculating the weighted sum of estimated WNMHD means for each N gives.
  # stage 2 sampling
  
  set.seed(0)
  res <- NA
  for (i in 1:10) {
    
    # cat("\rrun: ", i)
    # flush.console()
    
    # simulate
    NN <- sample(1:10, sample(1:5, 1))  # number of attributes
    NN <- sort(NN)
    n.cat <- sample(10:20, 1)            # number of categories
    
    P <- dexp(1L:n.cat, .1)             # category weights
    P <- P / sum(P)                     # category probabilities
    M <- rep(1, length(P))              # initial number of balls in urn 
    
    aw <- runif(length(NN))             # attribute weights
    ap <- aw / sum(aw)                  # attribute probability distribution
    f <- sim_n_persons_x_times(P, n = 1e5, a = NN, ap = ap, times = 1, progress = "none")
    h <- f / sum(f)                     # relative frequencies
    h <- unlist(h)                      # convert to vector
    mu.sim <- h * sum(ap * NN)          # relative average number of attributes
    
    # theoretical compound distribution
    MU <- mapply(BiasedUrn::meanMWNCHypergeo, n=NN, 
                 MoreArgs = list(m = M, odds = P, precision = 0))
    MU <- t(MU)
    mu <- as.vector(ap %*% MU)         # calculate weights means
    
    # compare
    res <- all.equal.numeric(target = mu, 
                             current = mu.sim,
                             check.attributes = FALSE, use.names = FALSE,
                             tolerance = 0.01)
  }
  expect_true(all(res))
  
})


