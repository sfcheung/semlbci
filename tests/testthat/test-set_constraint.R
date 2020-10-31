library(testthat)
library(semlbci)

context("Check set_constraint: No equality constraints")

data(simple_med)
dat <- simple_med
mod <- 
"
m ~ x
y ~ m
"
fit_med <- lavaan::sem(mod, simple_med, fixed.x = FALSE)

fn_constr0 <- set_constraint(fit_med)
out <- fn_constr0(lavaan::coef(fit_med), sem_out = fit_med)

test_that("Equal to lavaaan fmin", {
    expect_equivalent(
        out$objective, 
        lavaan::lavTech(fit_med, "optim")$fx,
        tolerance = 1e-5
      )
  })

test_that("Equal to lavaaan gradient", {
    expect_equivalent(
        as.numeric(out$gradient), 
        lavaan::lavTech(fit_med, "gradient"),
        tolerance = 1e-5
      )
  })

