library(testthat)
library(semlbci)

# Fit the model

library(lavaan)

data(cfa_two_factors)
dat <- cfa_two_factors
mod <-
"
f1 =~ x1 + b*x2 + c*x3
f2 =~ x4 + d*x5 + e*x6
f1 ~~ fr*f2
ce := c*e
b == d
"
fit <- lavaan::cfa(mod, cfa_two_factors)

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
time1l <- system.time(out1l <- ci_bound_wn_i(16, 13, sem_out = fit, which = "lbound", opts = opts0, f_constr = fn_constr0, verbose = TRUE, ciperc = ciperc))
time1u <- system.time(out1u <- ci_bound_wn_i(16, 13, sem_out = fit, which = "ubound", opts = opts0, f_constr = fn_constr0, verbose = TRUE, ciperc = ciperc))

test_that("Check against precomputed answers", {
    expect_equal(out1l$bound, 0.6211464, tolerance = 1e-5)
    expect_equal(out1u$bound, 1.230061, tolerance = 1e-5)
  })

skip("Run only if data changed")

# Check the results

modc0 <-
"
f1 =~ x1 + b*x2 + c*x3
f2 =~ x4 + d*x5 + e*x6
f1 ~~ fr*f2
ce := c*e
b == d
"

test_limit <- out1l
modc <- paste(modc0, "\nce == ", test_limit$bound)
fitc <- lavaan::sem(modc, cfa_two_factors, fixed.x = FALSE, do.fit = FALSE)
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE,
                   baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = TRUE,
                   check.start = FALSE
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

test_limit <- out1u
modc <- paste(modc0, "\nce == ", test_limit$bound)
fitc <- lavaan::sem(modc, cfa_two_factors, fixed.x = FALSE, do.fit = FALSE)
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE,
                   baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = TRUE,
                   check.start = FALSE
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
fitc_out1u <- fitc

test_that("Check p-value for the chi-square difference test", {
    expect_true(test_p(fitc_out1l, fit, ciperc = ciperc, tol = 1e-4))
    expect_true(test_p(fitc_out1u, fit, ciperc = ciperc, tol = 1e-4))
  })



# test_out1l <- test_constr(fit = fit, dat = cfa_two_factors, ciperc = ciperc, parc = "ce == ", modc0 = modc0, ci_out = out1l, semfct = lavaan::sem, tol = 1e-4, fixed.x = FALSE)
# test_out1u <- test_constr(fit = fit, dat = cfa_two_factors, ciperc = ciperc, parc = "ce == ", modc0 = modc0, ci_out = out1u, semfct = lavaan::sem, tol = 1e-4, fixed.x = FALSE)

# test_that("Check p-value for the chi-square difference test", {
#     expect_true(test_out1l)
#     expect_true(test_out1u)
#   })

