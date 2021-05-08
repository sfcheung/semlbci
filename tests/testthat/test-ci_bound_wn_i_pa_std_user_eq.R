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
a == b
"
fit <- lavaan::sem(mod, simple_med, fixed.x = FALSE)

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
time1l <- system.time(out1l <- ci_bound_wn_i(6, 5, sem_out = fit, f_constr = fn_constr0, which = "lbound", standardized = TRUE, opts = opts0, verbose = TRUE))
time1u <- system.time(out1u <- ci_bound_wn_i(6, 5, sem_out = fit, f_constr = fn_constr0, which = "ubound", standardized = TRUE, opts = opts0, verbose = TRUE))
time2l <- system.time(out2l <- ci_bound_wn_i(1, 5, sem_out = fit, f_constr = fn_constr0, which = "lbound", standardized = TRUE, opts = opts0, verbose = TRUE))
time2u <- system.time(out2u <- ci_bound_wn_i(1, 5, sem_out = fit, f_constr = fn_constr0, which = "ubound", standardized = TRUE, opts = opts0, verbose = TRUE))

timexx <- rbind(time1l, time1u, time2l, time2u)
timexx


# Check the results

# Not yet have a way to find how to make test_constr work in standardized solution

test_p <- function(fit0, fit1, ciperc, tol) {
    abs(anova(fit0, fit1)[2, "Pr(>Chisq)"] - (1 - ciperc)) < tol
  }

geteststd <- get_std_genfct(fit = fit, i = 6)

modc0 <- 
"
m ~ a*x
y ~ b*m
ab := a*b
a == b
abstd := geteststd()
"

test_limit <- out1l
modc <- paste(modc0, "\nabstd == ", test_limit, "\n0 < 1")
fitc <- lavaan::sem(modc, dat, do.fit = FALSE, fixed.x = FALSE)
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <-  attr(test_limit, "diag")$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE, optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02)))
fitc_out1l <- fitc

test_limit <- out1u
modc <- paste(modc0, "\nabstd == ", test_limit, "\n0 < 1")
fitc <- lavaan::sem(modc, dat, do.fit = FALSE, fixed.x = FALSE)
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <-  attr(test_limit, "diag")$history$solution
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
a == b
astd := geteststd()
"

test_limit <- out2l
modc <- paste(modc0, "\nastd == ", test_limit, "\n0 < 1")
fitc <- lavaan::sem(modc, dat, do.fit = FALSE, fixed.x = FALSE)
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <-  attr(test_limit, "diag")$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE, optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02)))
fitc_out2l <- fitc

test_limit <- out2u
modc <- paste(modc0, "\nastd == ", test_limit, "\n0 < 1")
fitc <- lavaan::sem(modc, dat, do.fit = FALSE, fixed.x = FALSE)
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <-  attr(test_limit, "diag")$history$solution
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











#

library(OpenMx)
cov_dat <- cov(dat)
n <- nrow(dat)
manifest_vars <- c("x", "m", "y")
mod_mx <- mxModel("Mediation", type = "RAM", 
    manifestVars = manifest_vars,
    mxPath(from = "x", to = "m", arrows = 1, free = TRUE, values = 1,
                  labels = "a"),
    mxPath(from = "m", to = "y", arrows = 1, free = TRUE, values = 1,
                  labels = "a"),
    mxPath(from = "x", arrows = 2, free = TRUE, values = 1,
                  labels = "var_x"),
    mxPath(from = "m", arrows = 2, free = TRUE, values = 1,
                  labels = "evar_m"),
    mxPath(from = "y", arrows = 2, free = TRUE, values = 1,
                  labels = "evar_y"),
    mxAlgebra(a * a , name = "ab"),
    mxAlgebra((a^2) * var_x + evar_m, name = "var_m"),
    mxAlgebra((a^2) * var_m + evar_y, name = "var_y"),
    mxAlgebra(a * sqrt(var_x) / sqrt(var_m), name = "a_std"),
    mxAlgebra(a * sqrt(var_m) / sqrt(var_y) , name = "b_std"),
    mxAlgebra(a * a * sqrt(var_x) / sqrt(var_y) , name = "ab_std"),
    mxCI(reference = c("a", "ab", "a_std", "b_std", "ab_std"), 
                       interval = .95, type = "both"),
    mxData(observed = cov_dat, type = "cov", numObs = n)
  )

# To replicate OpenMx 2.18.1 with SLSQP
mod_mx <- mxOption(mod_mx, "Feasibility tolerance", "1e-6")

fit_OpenMx <- mxRun(mod_mx, silent = TRUE, intervals = TRUE)
ci_OpenMx <- summary(fit_OpenMx)$CI

#ci_semlbci <- c(out1l, out2l, out1u, out2u)
ci_semlbci <- c(out1l, out2l, out1u, out2u)

test_that("Equal to OpenMx LBCI", {
    expect_equal(
        ci_semlbci, 
        unlist(ci_OpenMx[c(3, 5), c("lbound", "ubound")]),
        tolerance = 1e-3,
        ignore_attr = TRUE
      )
  })
