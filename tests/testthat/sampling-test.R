library(gridsampler)
library(testthat)

context("Sampling plausibility")

test_that("test some things", {

  # # test if every construct is sampled if lengt prob == number of 
  # # sampled constructs
  # s1 <- sim_one_person(p, a=length(p))
  # expect_true(all(s1 == 1))
  # 
  # 
  # # see if sampling many times from one person converges towards
  # # input probability
  # p <- dexp(1:10, rate=.1)
  # p <- p / sum(p)
  # d <- t(replicate(1000000, sim_one_person(p, a=5)))
  # p.hat <- colMeans(d) 
  # p.hat <- p.hat / sum(p.hat)
  # p - p.hat 
  # expect_equal(e, elementinfo(x))
  # expect_equal(c, constructinfo(x))

})


