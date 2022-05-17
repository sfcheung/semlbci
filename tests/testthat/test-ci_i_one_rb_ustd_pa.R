
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

fn_constr0 <- set_constraint(fit, ciperc = ciperc)

# opts0 <- list(print_level = 3)
opts0 <- list()
opts0 <- list(ftol_abs = 1e-7,
              ftol_rel = 1e-7,
              xtol_abs = 1e-7,
              xtol_rel = 1e-7
              )
time1x <- system.time(out1x <- ci_i_one(1, npar = 5, which = "lbound",
                      sem_out = fit, f_constr = fn_constr0, method = "wn",
                      opts = opts0, verbose = TRUE, ciperc = ciperc,
                      robust = "satorra.2000"))
time2x <- system.time(out2x <- ci_i_one(2, npar = 5, which = "ubound",
                      sem_out = fit, f_constr = fn_constr0, method = "wn",
                      opts = opts0, verbose = TRUE, ciperc = ciperc,
                      robust = "satorra.2000"))

timexx <- rbind(time1x, time2x)
timexx

# Check with known results

test_that("Check with know results", {
    expect_equal(unname(out1x$bounds), c(0.6756703), tolerance = 1e-4)
    expect_equal(unname(out2x$bounds), c(0.6734225), tolerance = 1e-4)
  })
