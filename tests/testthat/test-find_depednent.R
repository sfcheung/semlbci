library(testthat)
library(semlbci)

# context("Check find_depednent")

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

out <- lapply(1:7, find_dependent, sem_out = fit_med, standardized = FALSE)
out_std <- lapply(c(1,2, 3, 4, 6, 7), find_dependent, sem_out = fit_med, standardized = TRUE)

outs <- lapply(1:7, find_dependent, sem_out = fit_med, standardized = FALSE, signed = TRUE)
outs_std <- lapply(c(1,2, 3, 4, 6, 7), find_dependent, sem_out = fit_med, standardized = TRUE, signed = TRUE)

test_that("Unstandardized: As expected", {
    expect_equal(
      out,
      list(c(1), c(2), c(3), c(4), c(5), c(1, 2), c(1, 2))  
    )
  })

test_that("Standardized: As expected", {
    expect_equal(
      out_std,
      list(c(1, 3, 5), 
           c(1, 2, 3, 4, 5),
           c(1, 3, 5),
           c(1, 2, 3, 4, 5),
           c(1, 2, 3, 4, 5),
           c(1, 2, 3, 4, 5))  
    )
  })


test_that("Unstandardized, signed: As expected", {
    expect_equal(
      outs,
      list(c(1), c(1), c(1), c(1), c(1), c(1, 1), c(1, -1))  
    )
  })

test_that("Standardized, signed: As expected", {
    expect_equal(
      outs_std,
      list(c(1, -1, 1), 
           c(1, 1, 1, -1, 1),
           c(-1, 1, -1),
           c(-1, -1, -1, 1, -1),
           c(1, 1, -1,  -1, 1),
           c(1, -1, -1, 1, 1))  
    )
  })

