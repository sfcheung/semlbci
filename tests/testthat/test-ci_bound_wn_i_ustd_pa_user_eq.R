library(testthat)
library(semlbci)

# Fit the model

library(lavaan)

data(simple_med)
dat <- simple_med
mod <-
"
m ~ a*x
y ~ a*m
ab:= a*a
"
fit <- lavaan::sem(mod, simple_med, fixed.x = FALSE)
lavaan::parameterTable(fit)

# Find the LBCIs

ciperc <- .96

fn_constr0 <- set_constraint(fit, ciperc = ciperc)

# opts0 <- list(print_level = 3)
opts0 <- list()
opts0 <- list(ftol_abs = 1e-7,
              # ftol_rel = 1e-7,
              xtol_abs = 1e-7,
              # xtol_rel = 1e-7,
              print_level = 0
              )
time1l <- system.time(out1l <- ci_bound_wn_i(2, 5, sem_out = fit, f_constr = fn_constr0, which = "lbound", opts = opts0, verbose = TRUE, ciperc = ciperc))
time2u <- system.time(out2u <- ci_bound_wn_i(6, 5, sem_out = fit, f_constr = fn_constr0, which = "ubound", opts = opts0, verbose = TRUE, ciperc = ciperc))

test_that("Check against precomputed answers", {
    expect_equal(out1l$bound, 0.4172761, tolerance = 1e-5)
    expect_equal(out2u$bound, 0.5131228, tolerance = 1e-5)
  })

skip("Run only if data changed")

# Check the results

modc0 <-
"
m ~ a*x
y ~ a*m
ab:= a*a
"

test_limit <- out1l
modc <- paste(modc0, "\na == ", test_limit$bound)
fitc <- lavaan::sem(modc, simple_med, fixed.x = FALSE, do.fit = FALSE)
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE,
                   baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = TRUE
                  #  optim.force.converged = TRUE,
                  #  optim.dx.tol = .01,
                  #  warn = FALSE,
                  #  control = list(
                  #     eval.max = 2,
                  #     iterations = 1,
                  #     control.outer = list(tol = 1e-02,
                  #                          itmax = 1)
                  # )
                )
fitc_out1l <- fitc

test_limit <- out2u
modc <- paste(modc0, "\nab == ", test_limit$bound)
fitc <- lavaan::sem(modc, simple_med, fixed.x = FALSE, do.fit = FALSE)
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE,
                   baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = TRUE
                  #  optim.force.converged = TRUE,
                  #  optim.dx.tol = .01,
                  #  warn = FALSE,
                  #  control = list(
                  #     eval.max = 2,
                  #     iterations = 1,
                  #     control.outer = list(tol = 1e-02,
                  #                          itmax = 1)
                  # )
                )
fitc_out2u <- fitc

test_that("Check p-value for the chi-square difference test", {
    expect_true(test_p(fitc_out1l, fit, ciperc = ciperc, tol = 1e-4))
    expect_true(test_p(fitc_out2u, fit, ciperc = ciperc, tol = 1e-4))
  })
