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
fr := fr1 + fr2
"

fit <- lavaan::sem(mod, cfa_two_factors_mg, group = "gp")

ptable <- parameterTable(fit)

pars1 <- c("f1 =~ x1", "~")
pars2 <- c("~~", "f1 =~ x1")
pars3 <- c("=~", "f1 ~ x2")
pars4 <- c("=~", "f1 ~ x2", ":=")

test_that("pars_op", {
expect_true("f1 ~ f2" %in% pars_op(pars1, sem_out = fit))
expect_true("f1 ~~ f1" %in% pars_op(pars2, sem_out = fit))
expect_true("f1 =~ x3" %in% pars_op(pars3, sem_out = fit))
expect_true(all(c("f1 =~ x3", "fr := ") %in% pars_op(pars4, sem_out = fit)))
})