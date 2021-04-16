library(testthat)
library(semlbci)

# context("Check set_constraint_nm: No equality constraints, Neale-Miller-1997")

data(simple_med)
dat <- simple_med
mod <- 
"
m ~ x
y ~ m
"
fit_med <- lavaan::sem(mod, simple_med, fixed.x = FALSE)

fn_constr0 <- set_constraint_nm(1, fit_med)
out <- fn_constr0(lavaan::coef(fit_med)[1], sem_out = fit_med)

test_that("Equal to lavaaan fmin", {
    expect_equal(
        out$objective, 
        lavaan::lavTech(fit_med, "optim")$fx,
        tolerance = 1e-5,
        ignore_attr = TRUE
      )
  })

test_that("Equal to lavaaan gradient", {
    expect_equal(
        as.numeric(out$gradient)[1], 
        lavaan::lavTech(fit_med, "gradient")[1],
        tolerance = 1e-5,
        ignore_attr = TRUE
      )
  })

