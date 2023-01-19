skip_on_cran()
skip_if_not(Sys.getenv("SEMLBCI_TEST_COMPREHENSIVE") == "TRUE")

library(semlbci)
library(lavaan)
options(width = 132)

# Fit model

library(lavaan)

data(cfa_two_factors)
dat <- cfa_two_factors
mod <-
"
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
f1 ~ f2
"
fit <- lavaan::sem(mod, cfa_two_factors, test = "satorra.bentler")

# summary(fit)

# Find the scaling factor

system.time(sc1_lav <- scaling_factor3(fit, i = 2, standardized = TRUE, std_method = "lavaan"))
system.time(sc1_inv <- scaling_factor3(fit, i = 2, standardized = TRUE, std_method = "internal"))

# Check the results

test_that("Compare the two methods", {
    expect_equal(sc1_lav, sc1_inv, tolerance = 1e-6)
  })
