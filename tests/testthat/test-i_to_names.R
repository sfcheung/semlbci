library(testthat)
library(semlbci)

# Fit the model

library(lavaan)

data(cfa_two_factors_mg)
dat <- cfa_two_factors_mg
mod <-
"
f1 =~ x1 + c(a1, a2)*x2 + x3
f2 =~ x4 + c(b1, b2)*x5 + x6
f1 ~ c(fr1, fr2)*f2
a1 == a2
asq := a1 * a2
"

fit <- lavaan::sem(mod, cfa_two_factors_mg, group = "gp")

test_that("Check names", {
    expect_equal(i_to_name(2, fit), "a1 (gp1)")
    expect_equal(i_to_name(3, fit), "f1=~x3 (gp1)")
    expect_equal(i_to_name(48, fit), "asq")
  })

mod_nogp <-
"
f1 =~ x1 + a2*x2 + x3
f2 =~ x4 + a1*x5 + x6
f1 ~ fr*f2
a1 == a2
asq := a1 * a2
"
fit_nogp <- lavaan::sem(mod_nogp, cfa_two_factors_mg)

test_that("Check names", {
    expect_equal(i_to_name(2, fit_nogp), "a2")
    expect_equal(i_to_name(3, fit_nogp), "f1=~x3")
    expect_equal(i_to_name(17, fit_nogp), "asq")
  })
