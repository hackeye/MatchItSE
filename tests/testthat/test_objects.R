library(MatchIt)
data("lalonde")

context("Simple Tests")

test_that("Fail on wron object type", {
  m.out  <- matchit(treat ~ educ + black, data = lalonde)
  expect_error(att(obj = m.out, Y = lalalonde$re78))
})