skip_on_cran()
skip("To be run in an interactive session")

library(testthat)
library(semlbci)

# context("Check semlbci: No equality constraints")

# Fit the model

library(lavaan)

data(simple_med)
dat <- simple_med
mod <-
"
m ~ x
y ~ m
"
fit <- lavaan::sem(mod, simple_med, fixed.x = FALSE)

# Fit by semlbci

ciperc <- .96

opts0 <- list()
opts0 <- list(#ftol_abs = 1e-7,
              ftol_rel = 1e-4
              # xtol_abs = 1e-3,
              # xtol_rel = 1e-3,
              # tol_constraints_eq = 1e-3
              )

system.time(
   lbci_fit <- semlbci(fit,
                       pars = c("m~x","y~m"),
                       ciperc = ciperc,
                       standardized = TRUE,
                       opts0 = opts0,
                       verbose = TRUE)
  )

out1l <- list(bound = lbci_fit$lbci_lb[1],
              diag = (list(history = attr(lbci_fit, "lb_diag")[[1]]$history)))
out1u <- list(bound = lbci_fit$lbci_ub[1],
              diag = (list(history = attr(lbci_fit, "ub_diag")[[1]]$history)))
out2l <- list(bound = lbci_fit$lbci_lb[2],
              diag = (list(history = attr(lbci_fit, "lb_diag")[[2]]$history)))
out2u <- list(bound = lbci_fit$lbci_ub[2],
              diag = (list(history = attr(lbci_fit, "ub_diag")[[2]]$history)))

# Check the results

# Not yet have a way to find how to make test_constr work in standardized solution

test_p <- function(fit0, fit1, ciperc, tol) {
    abs(anova(fit0, fit1)[2, "Pr(>Chisq)"] - (1 - ciperc)) < tol
  }

geteststd <- get_std_genfct(fit = fit, i = 1)

modc0 <-
"
m ~ a*x
y ~ b*m
astd := geteststd()
"

test_limit <- out1l
modc <- paste(modc0, "\nastd == ", test_limit$bound, "\n0 < 1")
fitc <- lavaan::sem(modc, simple_med, do.fit = FALSE, fixed.x = FALSE)
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <-  test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE, optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02)))
fitc_out1l <- fitc

test_limit <- out1u
modc <- paste(modc0, "\nastd == ", test_limit$bound, "\n0 < 1")
fitc <- lavaan::sem(modc, simple_med, do.fit = FALSE, fixed.x = FALSE)
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <-  test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE, optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02)))
fitc_out1u <- fitc

geteststd <- get_std_genfct(fit = fit, i = 2)

modc0 <-
"
m ~ a*x
y ~ b*m
bstd := geteststd()
"

test_limit <- out2l
modc <- paste(modc0, "\nbstd == ", test_limit$bound, "\n0 < 1")
fitc <- lavaan::sem(modc, simple_med, do.fit = FALSE, fixed.x = FALSE)
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <-  test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE, optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02)))
fitc_out2l <- fitc

test_limit <- out2u
modc <- paste(modc0, "\nbstd == ", test_limit$bound, "\n0 < 1")
fitc <- lavaan::sem(modc, simple_med, do.fit = FALSE, fixed.x = FALSE)
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <-  test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE, optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02)))
fitc_out2u <- fitc

test_that("Check p-value for the chi-square difference test", {
    expect_true(test_p(fitc_out1l, fit, ciperc = ciperc, tol = 1e-4))
    expect_true(test_p(fitc_out1u, fit, ciperc = ciperc, tol = 1e-4))
    expect_true(test_p(fitc_out2l, fit, ciperc = ciperc, tol = 1e-4))
    expect_true(test_p(fitc_out2u, fit, ciperc = ciperc, tol = 1e-4))
  })

