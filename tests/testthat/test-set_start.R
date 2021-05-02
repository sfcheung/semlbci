library(testthat)
library(semlbci)

data(simple_med)
dat <- simple_med
mod <- 
"
m ~ a*x
y ~ b*m
ab:= a*b
ainvb := a/b
"
fit_med <- lavaan::sem(mod, simple_med, fixed.x = FALSE)
ptable <- lavaan::parameterTable(fit_med)
est   <- lavaan::parameterEstimates(fit_med)

y_m_start_l <- set_start(2, fit_med, "lbound")
y_m_start_u <- set_start(2, fit_med, "ubound")
y_y_start_l <- set_start(4, fit_med, "lbound")
y_y_start_u <- set_start(4, fit_med, "ubound")
ab_start_l  <- set_start(6, fit_med, "lbound")
ab_start_u  <- set_start(6, fit_med, "ubound")
ainvb_start_l  <- set_start(7, fit_med, "lbound")
ainvb_start_u  <- set_start(7, fit_med, "ubound")

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
