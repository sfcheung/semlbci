library(testthat)
library(semlbci)

context("Check update_model")

dat <- cfa_two_factors

mod <- 
"
f1 =~ x1 + x2 + a*x3
f2 =~ x4 + a*x5 + equal('f1=~x2')*x6
f1 ~~ 0*f2
asq := a^2
"

fit <- lavaan::sem(mod, dat)

std <- lavaan::standardizedSolution(fit)
ptable0 <- lavaan::parameterTable(fit)
pfree <- ptable0$free > 0
ptable <- ptable0
ptable[3, "est"] <- .4
ptable[14, "est"] <- .3
fit2 <- fit
fit2@ParTable <- as.list(ptable)
fit_model0 <- fit@Model
fit_model <- update_model(fit_model0, ptable[pfree, "est"])
fit2@Model <- fit_model
std2 <- lavaan::standardizedSolution(fit2)[, 1:4]
std
std2

fit_check <- update(fit, do.fit = FALSE, start = ptable)
std_check <- lavaan::standardizedSolution(fit_check)
std_check

test_that("Check against standardized solution by update", {
    expect_equivalent(
        std2, std_check
      )
  })
