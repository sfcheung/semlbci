library(testthat)
library(semlbci)

# Fit the model

library(lavaan)
data(simple_med_mg)
dat <- simple_med_mg
mod <- 
"
m ~ c(a, a)*x
y ~ c(b1, b2)*m
ab:= a*b1
"
fit <- lavaan::sem(mod, simple_med_mg, fixed.x = FALSE, group = "gp")
lavaan::parameterTable(fit)

# Find the LBCIs

pars <- c("a :=",
          "y ~ m",
          "ab :=")
pars_i <- syntax_to_i(pars, fit)
system.time(lbci_fit <- semlbci(fit,
                                pars = pars,
                                method = "wn",
                                verbose = TRUE))

# Check the results

ciperc <- .95

modc0 <- 
"
m ~ c(a, a)*x
y ~ c(b1, b2)*m
ab:= a*b1
"

out1l <- list(bound = lbci_fit[1, "lbci_lb"], diag = attr(lbci_fit, "lb_diag")[[1]])
out1u <- list(bound = lbci_fit[1, "lbci_ub"], diag = attr(lbci_fit, "ub_diag")[[1]])
out2l <- list(bound = lbci_fit[10, "lbci_lb"], diag = attr(lbci_fit, "lb_diag")[[2]])
out2u <- list(bound = lbci_fit[10, "lbci_ub"], diag = attr(lbci_fit, "ub_diag")[[2]])
out3l <- list(bound = lbci_fit[17, "lbci_lb"], diag = attr(lbci_fit, "lb_diag")[[4]])
out3u <- list(bound = lbci_fit[17, "lbci_ub"], diag = attr(lbci_fit, "ub_diag")[[4]])


test_out1l <- test_constr(fit = fit, dat = simple_med_mg, ciperc = ciperc, parc = "a == ", modc0 = modc0, ci_out = out1l, semfct = lavaan::sem, tol = 1e-4, fixed.x = FALSE, group = "gp")
test_out1u <- test_constr(fit = fit, dat = simple_med_mg, ciperc = ciperc, parc = "a == ", modc0 = modc0, ci_out = out1u, semfct = lavaan::sem, tol = 1e-4, fixed.x = FALSE, group = "gp")
test_out2l <- test_constr(fit = fit, dat = simple_med_mg, ciperc = ciperc, parc = "b2 == ", modc0 = modc0, ci_out = out2l, semfct = lavaan::sem, tol = 1e-4, fixed.x = FALSE, group = "gp")
test_out2u <- test_constr(fit = fit, dat = simple_med_mg, ciperc = ciperc, parc = "b2 == ", modc0 = modc0, ci_out = out2u, semfct = lavaan::sem, tol = 1e-4, fixed.x = FALSE, group = "gp")
test_out3l <- test_constr(fit = fit, dat = simple_med_mg, ciperc = ciperc, parc = "ab == ", modc0 = modc0, ci_out = out3l, semfct = lavaan::sem, tol = 1e-4, fixed.x = FALSE, group = "gp")
test_out3u <- test_constr(fit = fit, dat = simple_med_mg, ciperc = ciperc, parc = "ab == ", modc0 = modc0, ci_out = out3u, semfct = lavaan::sem, tol = 1e-4, fixed.x = FALSE, group = "gp")

test_that("Check p-values", {
    expect_true(test_out1l)
    expect_true(test_out1u)
    expect_true(test_out2l)
    expect_true(test_out2u)
    expect_true(test_out3l)
    expect_true(test_out3u)
  })

