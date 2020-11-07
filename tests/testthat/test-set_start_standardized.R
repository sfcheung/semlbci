library(testthat)
library(semlbci)

context("Check ci_bound_i: Standardized parameters")

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
std   <- lavaan::standardizedSolution(fit_med)

y_m_start_l <- set_start(2, fit_med, "lbound", standardized = TRUE)
y_m_start_u <- set_start(2, fit_med, "ubound", standardized = TRUE)
y_y_start_l <- set_start(4, fit_med, "lbound", standardized = TRUE)
y_y_start_u <- set_start(4, fit_med, "ubound", standardized = TRUE)

test_that("y_m lower: Equal to manually computed values", {
    expect_true(
        all(est[, c("ci.lower")][c(1, 2, 3, 5)] == 
            y_m_start_l$est[c(1, 2, 3, 5)]) &
        all(est[, c("ci.upper")][4] ==
            y_m_start_l$est[4])
      )
  })
test_that("y_m upper: Equal to manually computed values", {
    expect_true(
        all(est[, c("ci.upper")][c(1, 2, 3, 5)] == 
            y_m_start_u$est[c(1, 2, 3, 5)]) &
        all(est[, c("ci.lower")][4] ==
            y_m_start_u$est[4])
      )
  })


test_that("y_y lower: Equal to manually computed values", {
    expect_true(
        all(est[, c("ci.lower")][4] == 
            y_y_start_l$est[4]) &
        all(est[, c("ci.upper")][c(1, 2, 3, 5)] ==
            y_y_start_l$est[c(1, 2, 3, 5)])
      )
  })
test_that("y_y upper: Equal to manually computed values", {
    expect_true(
        all(est[, c("ci.lower")][c(1, 2, 3, 5)] == 
            y_y_start_u$est[c(1, 2, 3, 5)]) &
        all(est[, c("ci.upper")][4] ==
            y_y_start_u$est[4])
      )
  })
