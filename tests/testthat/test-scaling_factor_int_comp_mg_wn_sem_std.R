skip("Skip due to speed or other issues")
# To be tested in interactive sessions only due to scoping or speed issues

library(semlbci)
library(lavaan)
options(width = 132)

# Fit model

library(lavaan)

data(cfa_two_factors_mg)
dat <- cfa_two_factors_mg
mod <-
"
f1 =~ 1*x1 + c(a1, a2)*x2 + x3
f2 =~ x4 + x5 + x6
f1 ~ f2
"
fit <- lavaan::cfa(mod, cfa_two_factors_mg, test = "satorra.bentler", group = "gp", meanstructure = TRUE)

# summary(fit)

# Find the scaling factor

system.time(sc1_lav <- scaling_factor2(fit, i = 2, standardized = TRUE, std_method = "lavaan"))
system.time(sc1_inv <- scaling_factor2(fit, i = 2, standardized = TRUE, std_method = "internal"))

# Check the results

test_that("Compare the two methods", {
    expect_equal(sc1_lav, sc1_inv, tolerance = 1e-6)
  })
