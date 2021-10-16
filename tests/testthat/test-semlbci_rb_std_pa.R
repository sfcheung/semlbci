skip("Skip due to speed or other issues")
# To be tested in interactive sessions only due to scoping or speed issues

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
fit <- lavaan::sem(mod, simple_med, fixed.x = FALSE, test = "satorra.bentler")

# Find the LBCIs

ciperc <- .96

lbci_fit <- semlbci(fit, ciperc = ciperc, pars = c(1, 2), method = "wn", robust = "satorra.2000", verbose = TRUE,
                    standardized = TRUE)

# Check with known results

test_that("Check with know results", {
    expect_equal(unname(unlist(lbci_fit[1, c("lbci_lb", "lbci_ub")])), c(0.1133241, 0.4050732), tolerance = 1e-4)
    expect_equal(unname(unlist(lbci_fit[2, c("lbci_lb", "lbci_ub")])), c(0.3447777, 0.5592939), tolerance = 1e-4)
  })

