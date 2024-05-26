skip_on_cran()
library(testthat)
library(semlbci)

# Fit the model

library(lavaan)

data(simple_med)
dat <- simple_med
mod <-
"
m ~ x
y ~ m
"
fit <- lavaan::sem(mod, simple_med, fixed.x = FALSE)

# Find the LBCIs

ciperc <- .96

out1l <- ci_i_one(1, npar = 5, which = "lbound", sem_out = fit, method = "ur",
             ciperc = ciperc,
             standardized = TRUE,
             opts = list(use_callr = FALSE))

# Check with known results

test_that("Check with know results", {
    expect_equal(round(unname(out1l$bounds["lbound"]), 2), .13)
  })

