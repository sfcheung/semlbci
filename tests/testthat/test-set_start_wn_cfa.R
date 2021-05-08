skip("WIP: Not yet ready")

library(testthat)
library(semlbci)

library(lavaan)
data(cfa_two_factors)
dat <- cfa_two_factors
mod <- 
"
f1 =~ x1 + b*x2 + c*x3
f2 =~ x4 + d*x5 + c*x6
b == d
cmd := c - d
"
fit <- lavaan::sem(mod, dat)
ptable <- lavaan::parameterTable(fit)
est   <- lavaan::parameterEstimates(fit)

y_m_start_l <- set_start(2, fit, "lbound")
y_m_start_u <- set_start(2, fit, "ubound")
y_y_start_l <- set_start(4, fit, "lbound")
y_y_start_u <- set_start(4, fit, "ubound")
ab_start_l  <- set_start(6, fit, "lbound")
ab_start_u  <- set_start(6, fit, "ubound")
ainvb_start_l  <- set_start(7, fit, "lbound")
ainvb_start_u  <- set_start(7, fit, "ubound")

test_that("y_m lower: Equal to manually computed values", {
    expect_true(
        (est[, c("ci.lower")][2] == y_m_start_l$est[2]) &
        all(est[, c("ci.lower")][-2] != y_m_start_l$est[-2])
      )
  })
test_that("y_m upper: Equal to manually computed values", {
    expect_true(
        (est[, c("ci.upper")][2] == y_m_start_u$est[2]) &
        all(est[, c("ci.upper")][-2] != y_m_start_u$est[-2])
      )
  })

test_that("y_y lower: Equal to manually computed values", {
    expect_true(
        (est[, c("ci.lower")][4] == y_y_start_l$est[4]) &
        all(est[, c("ci.lower")][-4] != y_y_start_l$est[-4])
      )
  })
test_that("y_y upper: Equal to manually computed values", {
    expect_true(
        (est[, c("ci.upper")][4] == y_y_start_u$est[4]) &
        all(est[, c("ci.upper")][-4] != y_y_start_u$est[-4])
      )
  })

test_that("ab lower: Equal to manually computed values", {
    expect_true(
        all(est[, c("ci.lower")][1:2] == ab_start_l$est[1:2]) &
        all(est[, c("ci.lower")][-c(1, 2)] != ab_start_l$est[-c(1, 2)])
      )
  })
test_that("ab upper: Equal to manually computed values", {
    expect_true(
        all(est[, c("ci.upper")][1:2] == ab_start_u$est[1:2]) &
        all(est[, c("ci.upper")][-c(1, 2)] != ab_start_u$est[-c(1, 2)])
      )
  })

test_that("ainvb lower: Equal to manually computed values", {
    expect_true(
        all(c(est[, c("ci.lower")][1],
              est[, c("ci.upper")][2]) == ainvb_start_l$est[1:2]) &
        all(est[, c("ci.lower")][-c(1, 2)] != ainvb_start_l$est[-c(1, 2)])
      )
  })
test_that("ainvb upper: Equal to manually computed values", {
    expect_true(
        all(c(est[, c("ci.upper")][1],
              est[, c("ci.lower")][2]) == ainvb_start_u$est[1:2]) &
        all(est[, c("ci.lower")][-c(1, 2)] != ainvb_start_u$est[-c(1, 2)])
      )
  })
