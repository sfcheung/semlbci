skip("Test parallel processing: Test in interactive sections")

library(testthat)
library(semlbci)

# context("Check semlbci: No equality constraints, Neale-Miller-1997")

data(simple_med)
dat <- simple_med
mod <- 
"
m ~ x
y ~ m
"
fit <- lavaan::sem(mod, simple_med, fixed.x = FALSE)

lbci_med <- semlbci(fit, parallel = TRUE, ncpus = 5)

test_that("Equal to OpenMx LBCIs for free parameters", {
    expect_equal(
        as.numeric(unlist(lbci_med[c(1, 2), c("lbci_lb", "lbci_ub")])), 
        unlist(ci_OpenMx[c("a", "b"), c("lbound", "ubound")]) ,
        tolerance = 1e-5,
        ignore_attr = TRUE
      )
  })


