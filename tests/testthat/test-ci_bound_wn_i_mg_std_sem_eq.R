skip("Skip due to speed or other issues")
# To be tested in interactive sessions only due to scoping or speed issues
# To fix: Do not use saved data

library(testthat)
library(semlbci)

# Fit the model

library(lavaan)

data(cfa_two_factors_mg)
dat <- cfa_two_factors_mg
mod <- 
"
f1 =~ x1 + c(b1, b2)*x2 + c(c1, c2)*x3
f2 =~ x4 + c(d1, d2)*x5 + c(e1, e2)*x6
f1 ~ c(g1, g2)*f2
b1 == b2
"

fit <- lavaan::sem(mod, cfa_two_factors_mg, group = "gp")

# Find the LBCIs

ciperc <- .96

fn_constr0 <- set_constraint(fit, ciperc = ciperc)

# opts0 <- list(print_level = 3)
opts0 <- list()
opts0 <- list(#ftol_abs = 1e-7,
              ftol_rel = 1e-4
              # xtol_abs = 1e-7,
              # xtol_rel = 1e-7,
              # tol_constraints_eq = 1e-10
              )
time1l <- system.time(out1l <- ci_bound_wn_i(25, 38, sem_out = fit, which = "lbound", opts = opts0, f_constr = fn_constr0, verbose = TRUE, ciperc = ciperc, standardized = TRUE, std_method = "internal"))
time1u <- system.time(out1u <- ci_bound_wn_i(25, 38, sem_out = fit, which = "ubound", opts = opts0, f_constr = fn_constr0, verbose = TRUE, ciperc = ciperc, standardized = TRUE, std_method = "internal"))
time2l <- system.time(out2l <- ci_bound_wn_i(30, 38, sem_out = fit, which = "lbound", opts = opts0, f_constr = fn_constr0, verbose = TRUE, ciperc = ciperc, standardized = TRUE, std_method = "internal"))
time2u <- system.time(out2u <- ci_bound_wn_i(30, 38, sem_out = fit, which = "ubound", opts = opts0, f_constr = fn_constr0, verbose = TRUE, ciperc = ciperc, standardized = TRUE, std_method = "internal"))

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

modc0 <- 
"
f1 =~ x1 + c(b1, b2)*x2 + c(c1, c2)*x3
f2 =~ x4 + c(d1, d2)*x5 + c(e1, e2)*x6
f1 ~ c(g1, g2)*f2
b1 == b2
tstd := geteststd()
"

geteststd <- get_std_genfct(fit = fit, i = 25)

test_limit <- out1l
modc <- paste(modc0, "\ntstd == ", test_limit$bound, "\n0 < 1")
fitc <- lavaan::sem(modc, dat, do.fit = FALSE, group = "gp")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <-  test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE, optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02)))
fitc_out1l <- fitc

test_limit <- out1u
modc <- paste(modc0, "\ntstd == ", test_limit$bound, "\n0 < 1")
fitc <- lavaan::sem(modc, dat, do.fit = FALSE, group = "gp")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <-  test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE, optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02)))
fitc_out1u <- fitc


geteststd <- get_std_genfct(fit = fit, i = 30)

test_limit <- out2l
modc <- paste(modc0, "\ntstd == ", test_limit$bound, "\n0 < 1")
fitc <- lavaan::sem(modc, dat, do.fit = FALSE, group = "gp")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <-  test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE, optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02)))
fitc_out2l <- fitc

test_limit <- out2u
modc <- paste(modc0, "\ntstd == ", test_limit$bound, "\n0 < 1")
fitc <- lavaan::sem(modc, dat, do.fit = FALSE, group = "gp")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <-  test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE, optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02)))
fitc_out2u <- fitc

save(fitc_out1l, fitc_out1u,
     fitc_out2l, fitc_out2u,
     file = "inst/testdata/test-ci_bound_wn_i_mg_std_sem_eq.RData",
     compress = "xz",
     compression_level = 9)
}

load(system.file("testdata", "test-ci_bound_wn_i_mg_std_sem_eq.RData",
                  package = "semlbci"))

test_that("Check p-value for the chi-square difference test", {
    expect_true(test_p(fitc_out1l, fit, ciperc = ciperc, tol = 1e-4))
    expect_true(test_p(fitc_out1u, fit, ciperc = ciperc, tol = 1e-4))
    expect_true(test_p(fitc_out2l, fit, ciperc = ciperc, tol = 1e-4))
    expect_true(test_p(fitc_out2u, fit, ciperc = ciperc, tol = 1e-4))
  })
