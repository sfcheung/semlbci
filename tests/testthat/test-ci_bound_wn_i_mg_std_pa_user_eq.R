skip("WIP")

skip_if(Sys.getenv("SEMLBCI_TEST_SLOW") == "",
        "Skip due to speed or other issues")
# To be tested in interactive sessions only due to scoping or speed issues

library(testthat)
library(semlbci)

# Fit the model

library(lavaan)

data(simple_med_mg)
dat <- simple_med_mg
mod <- 
"
m ~ c(a1, a2)*x
y ~ c(b1, b2)*m
ab := a2*b2
a1 == a2
"
fit <- lavaan::sem(mod, simple_med_mg, fixed.x = FALSE, group = "gp")

# Find the LBCIs

ciperc <- .96

fn_constr0 <- set_constraint(fit, ciperc = ciperc)

# opts0 <- list(print_level = 3)
opts0 <- list()
opts0 <- list(ftol_abs = 1e-7,
              ftol_rel = 1e-7,
              xtol_abs = 1e-7,
              xtol_rel = 1e-7
              # tol_constraints_eq = 1e-7
              )
time1l <- system.time(out1l <- ci_bound_wn_i(17, 16, sem_out = fit, f_constr = fn_constr0, which = "lbound", standardized = TRUE, ciperc = ciperc, opts = opts0, verbose = TRUE))
time1u <- system.time(out1u <- ci_bound_wn_i(17, 16, sem_out = fit, f_constr = fn_constr0, which = "ubound", standardized = TRUE, ciperc = ciperc, opts = opts0, verbose = TRUE))
time2l <- system.time(out2l <- ci_bound_wn_i(1, 16, sem_out = fit, f_constr = fn_constr0, which = "lbound", standardized = TRUE, ciperc = ciperc, opts = opts0, verbose = TRUE))
time2u <- system.time(out2u <- ci_bound_wn_i(1, 16, sem_out = fit, f_constr = fn_constr0, which = "ubound", standardized = TRUE, ciperc = ciperc, opts = opts0, verbose = TRUE))

timexx <- rbind(time1l, time1u, time2l, time2u)
timexx


# Check the results

# Not yet have a way to find how to make test_constr work in standardized solution

test_p <- function(fit0, fit1, ciperc, tol) {
    abs(anova(fit0, fit1)[2, "Pr(>Chisq)"] - (1 - ciperc)) < tol
  }

geteststd <- get_std_genfct(fit = fit, i = 17)

modc0 <- 
"
m ~ c(a1, a2)*x
y ~ c(b1, b2)*m
ab := a2*b2
a1 == a2
abstd := geteststd()
"

test_limit <- out1l
modc <- paste(modc0, "\nabstd == ", test_limit$bound, "\n0 < 1")
fitc <- lavaan::sem(modc, simple_med_mg, do.fit = FALSE, fixed.x = FALSE, group = "gp")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <-  test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE, optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02)))
fitc_out1l <- fitc

test_limit <- out1u
modc <- paste(modc0, "\nabstd == ", test_limit$bound, "\n0 < 1")
fitc <- lavaan::sem(modc, simple_med_mg, do.fit = FALSE, fixed.x = FALSE, group = "gp")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <-  test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE, optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02)))
fitc_out1u <- fitc

geteststd <- get_std_genfct(fit = fit, i = 1)

modc0 <- 
"
m ~ c(a1, a2)*x
y ~ c(b1, b2)*m
ab := a2*b2
a1 == a2
astd := geteststd()
"

test_limit <- out2l
modc <- paste(modc0, "\nastd == ", test_limit$bound, "\n0 < 1")
fitc <- lavaan::sem(modc, simple_med_mg, do.fit = FALSE, fixed.x = FALSE, group = "gp")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <-  test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE, optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02)))
fitc_out2l <- fitc

test_limit <- out2u
modc <- paste(modc0, "\nastd == ", test_limit$bound, "\n0 < 1")
fitc <- lavaan::sem(modc, simple_med_mg, do.fit = FALSE, fixed.x = FALSE, group = "gp")
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
