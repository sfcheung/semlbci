library(testthat)
library(semlbci)

# Fit the model

library(lavaan)

data(cfa_two_factors_mg)
dat <- cfa_two_factors_mg
mod <- 
"
f1 =~ x1 + c(b1, b2)*x2 + c(c1, c1)*x3
f2 =~ x4 + x5 + c(e1, e1)*x6
f1 ~~ c(fr1, fr2)*f2
ce := c1*e1
b1 == c1
"
fit <- lavaan::cfa(mod, cfa_two_factors_mg, group = "gp")

# Find the LBCIs

pars <- c("b1 :=",
          "f2 =~ x5",
          "ce :=")
pars_i <- syntax_to_i(pars, fit)
lbci_fit <- semlbci(fit, pars = pars,
                    method = "wn",
                    verbose = TRUE)

# Check the results

ciperc <- .95

modc0 <- 
"
f1 =~ x1 + c(b1, b2)*x2 + c(c1, c1)*x3
f2 =~ x4 + c(d1, d2)*x5 + c(e1, e1)*x6
f1 ~~ c(fr1, fr2)*f2
ce := c1*e1
b1 == c1
"

out1l <- list(bound = lbci_fit[2, "lbci_lb"], diag = attr(lbci_fit, "lb_diag")[[1]])
out1u <- list(bound = lbci_fit[2, "lbci_ub"], diag = attr(lbci_fit, "ub_diag")[[1]])
out2l <- list(bound = lbci_fit[28, "lbci_lb"], diag = attr(lbci_fit, "lb_diag")[[3]])
out2u <- list(bound = lbci_fit[28, "lbci_ub"], diag = attr(lbci_fit, "ub_diag")[[3]])
out3l <- list(bound = lbci_fit[47, "lbci_lb"], diag = attr(lbci_fit, "lb_diag")[[4]])
out3u <- list(bound = lbci_fit[47, "lbci_ub"], diag = attr(lbci_fit, "ub_diag")[[4]])

test_out1l <- test_constr(fit = fit, dat = cfa_two_factors_mg, ciperc = ciperc, parc = "b1 == ", modc0 = modc0, ci_out = out1l, semfct = lavaan::cfa, tol = 1e-4, group = "gp")
test_out1u <- test_constr(fit = fit, dat = cfa_two_factors_mg, ciperc = ciperc, parc = "b1 == ", modc0 = modc0, ci_out = out1u, semfct = lavaan::cfa, tol = 1e-4, group = "gp")
test_out2l <- test_constr(fit = fit, dat = cfa_two_factors_mg, ciperc = ciperc, parc = "d2 == ", modc0 = modc0, ci_out = out2l, semfct = lavaan::cfa, tol = 1e-4, group = "gp")
test_out2u <- test_constr(fit = fit, dat = cfa_two_factors_mg, ciperc = ciperc, parc = "d2 == ", modc0 = modc0, ci_out = out2u, semfct = lavaan::cfa, tol = 1e-4, group = "gp")
test_out3l <- test_constr(fit = fit, dat = cfa_two_factors_mg, ciperc = ciperc, parc = "ce == ", modc0 = modc0, ci_out = out3l, semfct = lavaan::cfa, tol = 1e-4, group = "gp")
test_out3u <- test_constr(fit = fit, dat = cfa_two_factors_mg, ciperc = ciperc, parc = "ce == ", modc0 = modc0, ci_out = out3u, semfct = lavaan::cfa, tol = 1e-4, group = "gp")

test_that("Check p-values", {
    expect_true(test_out1l)
    expect_true(test_out1u)
    expect_true(test_out2l)
    expect_true(test_out2u)
    expect_true(test_out3l)
    expect_true(test_out3u)
  })

