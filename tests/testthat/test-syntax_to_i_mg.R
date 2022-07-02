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

ptable <- parameterTable(fit)

pars_i <- syntax_to_i(c("a1 := ", "b2:="), fit)

test_that("Check parameter positions", {
    expect_equal(pars_i,
                 sort(c(2, 28)))
  })

pars_i <- syntax_to_i(c("f1 =~ x2", "f1 ~ f2"), fit)

test_that("Check parameter positions", {
    expect_equal(pars_i,
                 sort(c(2, 7, 25, 30)))
  })

pars_i <- syntax_to_i(c("asq := ", "f1 =~ x2", "f1 ~ f2", "b1 := "), fit)

test_that("Check parameter positions", {
    expect_equal(pars_i,
                 sort(c(2, 7, 25, 30, 48, 5)))
  })

pars_i <- syntax_to_i(c("asq := ", "f1 =~ 2*x2", "f1 ~ 1*f2", "b1 := "), fit)

test_that("Check parameter positions", {
    expect_equal(pars_i,
                 sort(c(48, 25, 7, 5)))
  })
