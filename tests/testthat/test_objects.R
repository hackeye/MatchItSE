library(MASS)
library(MatchIt)
data("lalonde")

context("Simple Tests")

test_that("Fail on wrong object type", {
  fit.acc  <- matchit(treat ~ educ + black, data = lalonde)
  fit.fail <- fit.acc ; fit.fail <- class("Nonsense")
  expect_error(att(obj = fit.acc, Y = lalonde$re78), NA)
  expect_error(att(obj = fit.fail, Y = lalonde$re78))
})