skip_on_cran()
library(testthat)
library(semlbci)

# Fit the model

library(lavaan)

data(simple_med)
dat <- simple_med
mod <- 
"
m ~ a*x
y ~ b*m
ab := a*b
"
fit <- lavaan::sem(mod, simple_med, fixed.x = FALSE)


# Find the LBCIs

ciperc <- .96

fn_constr0 <- set_constraint(fit, ciperc = ciperc)

opts0 <- list(#ftol_abs = 1e-7,
              ftol_rel = 1e-7
              # xtol_abs = 1e-3,
              # xtol_rel = 1e-3,
              # tol_constraints_eq = 1e-3
              )
time1l <- system.time(out1l <- ci_bound_wn_i(6, 5, sem_out = fit, f_constr = fn_constr0, which = "lbound", standardized = TRUE, ciperc = ciperc, opts = opts0, verbose = TRUE, wald_ci_start = FALSE, std_method = "internal"))
time1u <- system.time(out1u <- ci_bound_wn_i(6, 5, sem_out = fit, f_constr = fn_constr0, which = "ubound", standardized = TRUE, ciperc = ciperc, opts = opts0, verbose = TRUE, wald_ci_start = FALSE, std_method = "internal"))
time2l <- system.time(out2l <- ci_bound_wn_i(1, 5, sem_out = fit, f_constr = fn_constr0, which = "lbound", standardized = TRUE, ciperc = ciperc, opts = opts0, verbose = TRUE, wald_ci_start = FALSE, std_method = "internal"))
time2u <- system.time(out2u <- ci_bound_wn_i(1, 5, sem_out = fit, f_constr = fn_constr0, which = "ubound", standardized = TRUE, ciperc = ciperc, opts = opts0, verbose = TRUE, wald_ci_start = FALSE, std_method = "internal"))

timexx <- rbind(time1l, time1u, time2l, time2u)
timexx
colSums(timexx)

# Check the results

# Not yet have a way to find how to make test_constr work in standardized solution

test_p <- function(fit0, fit1, ciperc, tol) {
    abs(anova(fit0, fit1)[2, "Pr(>Chisq)"] - (1 - ciperc)) < tol
  }

gen_test_data <- FALSE
if (gen_test_data) {

geteststd <- get_std_genfct(fit = fit, i = 6)

modc0 <- 
"
m ~ a*x
y ~ b*m
ab := a*b
abstd := geteststd()
"

test_limit <- out1l
modc <- paste(modc0, "\nabstd == ", test_limit$bound, "\n0 < 1")
fitc <- lavaan::sem(modc, simple_med, do.fit = FALSE, fixed.x = FALSE)
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <-  test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE, optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02)))
fitc_out1l <- fitc

test_limit <- out1u
modc <- paste(modc0, "\nabstd == ", test_limit$bound, "\n0 < 1")
fitc <- lavaan::sem(modc, simple_med, do.fit = FALSE, fixed.x = FALSE)
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <-  test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE, optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02)))
fitc_out1u <- fitc

geteststd <- get_std_genfct(fit = fit, i = 1)

modc0 <- 
"
m ~ a*x
y ~ b*m
ab := a*b
astd := geteststd()
"

test_limit <- out2l
modc <- paste(modc0, "\nastd == ", test_limit$bound, "\n0 < 1")
fitc <- lavaan::sem(modc, simple_med, do.fit = FALSE, fixed.x = FALSE)
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <-  test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE, optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02)))
fitc_out2l <- fitc

test_limit <- out2u
modc <- paste(modc0, "\nastd == ", test_limit$bound, "\n0 < 1")
fitc <- lavaan::sem(modc, simple_med, do.fit = FALSE, fixed.x = FALSE)
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <-  test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE, optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02)))
fitc_out2u <- fitc

save(fitc_out1l, fitc_out1u,
     fitc_out2l, fitc_out2u,
     file = "inst/testdata/test-ci_bound_wn_i_std_pa_user.RData",
     compress = "xz",
     compression_level = 9)
}

load(system.file("testdata", "test-ci_bound_wn_i_std_pa_user.RData",
                  package = "semlbci"))

test_that("Check p-value for the chi-square difference test", {
    expect_true(test_p(fitc_out1l, fit, ciperc = ciperc, tol = 1e-4))
    expect_true(test_p(fitc_out1u, fit, ciperc = ciperc, tol = 1e-4))
    expect_true(test_p(fitc_out2l, fit, ciperc = ciperc, tol = 1e-4))
    expect_true(test_p(fitc_out2u, fit, ciperc = ciperc, tol = 1e-4))
  })

