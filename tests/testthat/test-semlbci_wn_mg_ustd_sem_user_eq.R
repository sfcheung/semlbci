skip_on_cran()
library(testthat)
library(semlbci)


# Fit the model

library(lavaan)

data(cfa_two_factors_mg)
dat <- cfa_two_factors_mg
mod <-
"
f1 =~ x1 + c(a1, a2)*x2 + c(c1, c2)*x3
f2 =~ x4 + c(b1, b2)*x5 + c(d1, d2)*x6
f1 ~ c(fr1, fr2)*f2
ab := a1 * b2
c1 == c2
"
fit <- lavaan::sem(mod, cfa_two_factors_mg, group = "gp")

# Find the LBCIs

pars <- c("c2 :=",
          "f1 ~ f2",
          "ab :=")
pars_i <- syntax_to_i(pars, fit)
system.time(
    lbci_fit <- semlbci(fit,
                        pars = pars,
                        method = "wn",
                        verbose = TRUE,
                        opts = list(ftol_rel = 1e-6))
  )

# Check the results

ciperc <- .95

modc0 <-
"
f1 =~ x1 + c(a1, a2)*x2 + c(c1, c2)*x3
f2 =~ x4 + c(b1, b2)*x5 + c(d1, d2)*x6
f1 ~ c(fr1, fr2)*f2
ab := a1 * b2
c1 == c2
"
pars_i <- syntax_to_i(pars, fit)
out1l <- list(bound = lbci_fit[7, "lbci_lb"], diag = attr(lbci_fit, "lb_diag")[[pars_i[1]]])
out1u <- list(bound = lbci_fit[7, "lbci_ub"], diag = attr(lbci_fit, "ub_diag")[[pars_i[1]]])
out2l <- list(bound = lbci_fit[26, "lbci_lb"], diag = attr(lbci_fit, "lb_diag")[[pars_i[2]]])
out2u <- list(bound = lbci_fit[26, "lbci_ub"], diag = attr(lbci_fit, "ub_diag")[[pars_i[2]]])
out3l <- list(bound = lbci_fit[47, "lbci_lb"], diag = attr(lbci_fit, "lb_diag")[[pars_i[4]]])
out3u <- list(bound = lbci_fit[47, "lbci_ub"], diag = attr(lbci_fit, "ub_diag")[[pars_i[4]]])

test_out1l <- test_constr(fit = fit, dat = cfa_two_factors_mg, ciperc = ciperc, parc = "fr1 == ", modc0 = modc0, ci_out = out1l, semfct = lavaan::sem, tol = 1e-4, group = "gp")
test_out1u <- test_constr(fit = fit, dat = cfa_two_factors_mg, ciperc = ciperc, parc = "fr1 == ", modc0 = modc0, ci_out = out1u, semfct = lavaan::sem, tol = 1e-4, group = "gp")
test_out2l <- test_constr(fit = fit, dat = cfa_two_factors_mg, ciperc = ciperc, parc = "c2 == ", modc0 = modc0, ci_out = out2l, semfct = lavaan::sem, tol = 1e-4, group = "gp")
test_out2u <- test_constr(fit = fit, dat = cfa_two_factors_mg, ciperc = ciperc, parc = "c2 == ", modc0 = modc0, ci_out = out2u, semfct = lavaan::sem, tol = 1e-4, group = "gp")
test_out3l <- test_constr(fit = fit, dat = cfa_two_factors_mg, ciperc = ciperc, parc = "ab == ", modc0 = modc0, ci_out = out3l, semfct = lavaan::sem, tol = 1e-4, group = "gp")
test_out3u <- test_constr(fit = fit, dat = cfa_two_factors_mg, ciperc = ciperc, parc = "ab == ", modc0 = modc0, ci_out = out3u, semfct = lavaan::sem, tol = 1e-4, group = "gp")

test_that("Check p-values", {
    expect_true(test_out1l)
    expect_true(test_out1u)
    expect_true(test_out2l)
    expect_true(test_out2u)
    expect_true(test_out3l)
    expect_true(test_out3u)
  })
