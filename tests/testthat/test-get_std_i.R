library(testthat)
library(semlbci)
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
tmp := semlbci:::get_std_i(i = 7)
"
fit <- lavaan::sem(mod, cfa_two_factors)

# summary(fit)

test_that("Check user computed standardized solution", {
    expect_equal(parameterEstimates(fit)[16, "est"],
                 standardizedSolution(fit, se = FALSE)[7, "est.std"])
  })



