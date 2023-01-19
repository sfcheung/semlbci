
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

system.time(
    lbci_fit <- semlbci(fit,
                        ciperc = ciperc,
                        pars = c(1, 2),
                        method = "wn",
                        robust = "satorra.2000",
                        verbose = TRUE,
                        opts = list(ftol_rel = 1e-4))
  )

# print(lbci_fit)

out1l <- list(bound = lbci_fit[1, "lbci_lb"], diag = attr(lbci_fit, "lb_diag")[[1]])
out1u <- list(bound = lbci_fit[1, "lbci_ub"], diag = attr(lbci_fit, "ub_diag")[[1]])
out2l <- list(bound = lbci_fit[2, "lbci_lb"], diag = attr(lbci_fit, "lb_diag")[[2]])
out2u <- list(bound = lbci_fit[2, "lbci_ub"], diag = attr(lbci_fit, "ub_diag")[[2]])

# Check the results

test_p <- function(fit0, fit1, ciperc, tol, debug = FALSE) {
    out <- lavTestLRT(fit0, fit1, method = "satorra.2000", A.method = "exact")
    if(debug) print(out)
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
# print(ptable)
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
# summary(fitc)
fitc_out1l <- fitc

test_limit <- out1u
modc <- paste(modc0, "\na == ", test_limit$bound)
fitc <- lavaan::sem(modc, simple_med, fixed.x = FALSE, do.fit = FALSE, test = "satorra.bentler")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
# print(ptable)
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
# summary(fitc)
fitc_out1u <- fitc


test_limit <- out2l
modc <- paste(modc0, "\nb == ", test_limit$bound)
fitc <- lavaan::sem(modc, simple_med, fixed.x = FALSE, do.fit = FALSE, test = "satorra.bentler")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
# print(ptable)
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
# summary(fitc)
fitc_out2l <- fitc

test_limit <- out2u
modc <- paste(modc0, "\nb == ", test_limit$bound)
fitc <- lavaan::sem(modc, simple_med, fixed.x = FALSE, do.fit = FALSE, test = "satorra.bentler")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
# print(ptable)
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
# summary(fitc)
fitc_out2u <- fitc

# test_p(fitc_out1l, fit, ciperc = ciperc, tol = 1e-4, debug = TRUE)
# test_p(fitc_out1u, fit, ciperc = ciperc, tol = 1e-4, debug = TRUE)
# test_p(fitc_out2l, fit, ciperc = ciperc, tol = 1e-4, debug = TRUE)
# test_p(fitc_out2u, fit, ciperc = ciperc, tol = 1e-4, debug = TRUE)

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
