library(testthat)
library(stdmod)

context("Check find_free")

dat <- cfa_two_factors

mod <- 
"
f1 =~ x1 + x2 + a*x3
f2 =~ x4 + a*x5 + equal('f1=~x2')*x6
f1 ~~ 0*f2
asq := a^2
"

fit <- lavaan::sem(mod, dat)
ptable <- lavaan::parameterTable(fit)
pfree <- ptable$free > 0

test_that("Correct free parameters", {
    expect_equivalent(
        find_free(fit), pfree
      )
  })

test_that("Stop when the object is invalid", {
    expect_error(
        find_free(mod),
        class = "simpleError"
      )
  })
