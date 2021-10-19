
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

# Find the scaling factors

# sf1 <- scaling_factor3(fit, 1)
# sf2 <- scaling_factor3(fit, 2)

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
time1x <- system.time(out1x <- ci_i(1, npar = 5, sem_out = fit, f_constr = fn_constr0, method = "wn", opts = opts0, verbose = TRUE, ciperc = ciperc, robust = "satorra.2000"))
time2x <- system.time(out2x <- ci_i(2, npar = 5, sem_out = fit, f_constr = fn_constr0, method = "wn", opts = opts0, verbose = TRUE, ciperc = ciperc, robust = "satorra.2000"))

timexx <- rbind(time1x, time2x)
timexx

out1l <- list(bound = out1x$bounds["lbound"], diag = out1x$diags$lb_diag)
out1u <- list(bound = out1x$bounds["ubound"], diag = out1x$diags$ub_diag)
out2l <- list(bound = out2x$bounds["lbound"], diag = out2x$diags$lb_diag)
out2u <- list(bound = out2x$bounds["ubound"], diag = out2x$diags$ub_diag)

# Check the results

test_p <- function(fit0, fit1, ciperc, tol) {
    out <- lavTestLRT(fit0, fit1, method = "satorra.2000", A.method = "exact")
    abs(out[2, "Pr(>Chisq)"] - (1 - ciperc)) < tol
  }

modc0 <-
"
m ~ a*x
y ~ b*m
"

test_limit <- out1l
modc <- paste(modc0, "\na == ", test_limit$bound)
fitc <- lavaan::sem(modc, simple_med, fixed.x = FALSE, do.fit = FALSE, test = "satorra.bentler")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE,
                   baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE
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
modc <- paste(modc0, "\na == ", test_limit$bound)
fitc <- lavaan::sem(modc, simple_med, fixed.x = FALSE, do.fit = FALSE, test = "satorra.bentler")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE,
                   baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE
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


test_limit <- out2l
modc <- paste(modc0, "\nb == ", test_limit$bound)
fitc <- lavaan::sem(modc, simple_med, fixed.x = FALSE, do.fit = FALSE, test = "satorra.bentler")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE,
                   baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE
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
fitc_out2l <- fitc

test_limit <- out2u
modc <- paste(modc0, "\nb == ", test_limit$bound)
fitc <- lavaan::sem(modc, simple_med, fixed.x = FALSE, do.fit = FALSE, test = "satorra.bentler")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE,
                   baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE
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
    expect_true(test_p(fitc_out1u, fit, ciperc = ciperc, tol = 1e-4))
    expect_true(test_p(fitc_out2l, fit, ciperc = ciperc, tol = 1e-4))
    expect_true(test_p(fitc_out2u, fit, ciperc = ciperc, tol = 1e-4))
  })

fit_ml <- update(fit, test = "standard")
test_out1l <- test_constr(fit = fit_ml, dat = simple_med, ciperc = ciperc, parc = "a == ", modc0 = modc0, ci_out = out1l, semfct = lavaan::sem, tol = 1e-4, fixed.x = FALSE)
test_out1u <- test_constr(fit = fit_ml, dat = simple_med, ciperc = ciperc, parc = "a == ", modc0 = modc0, ci_out = out1u, semfct = lavaan::sem, tol = 1e-4, fixed.x = FALSE)
test_out2l <- test_constr(fit = fit_ml, dat = simple_med, ciperc = ciperc, parc = "b == ", modc0 = modc0, ci_out = out2l, semfct = lavaan::sem, tol = 1e-4, fixed.x = FALSE)
test_out2u <- test_constr(fit = fit_ml, dat = simple_med, ciperc = ciperc, parc = "b == ", modc0 = modc0, ci_out = out2u, semfct = lavaan::sem, tol = 1e-4, fixed.x = FALSE)

test_that("The limit should be different from those using ML", {
    expect_false(test_out1l)
    expect_false(test_out1u)
    expect_false(test_out2l)
    expect_false(test_out2u)
  })
