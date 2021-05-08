skip("WIP: ci_bound_nm_i on hold")

library(testthat)
library(semlbci)
library(lavaan)

# context("Check ci_bound_nm_i: With equality constraints, test_generic = TRUE")

data(simple_med)
dat <- simple_med
mod <- 
"
m ~ a*x
y ~ a*m
asq := a^2
"
fit_med <- lavaan::sem(mod, simple_med, fixed.x = FALSE)

# opts0 <- list(print_level = 3)
opts0 <- list()
opts0 <- list(ftol_abs = 1e-7,
              # ftol_rel = 1e-7,
              xtol_abs = 1e-7,
              # xtol_rel = 1e-7,
              tol_constraints_eq = 1e-10,
              print_level = 0
              )
system.time(out1l0 <-  ci_bound_nm_i(1, 5, sem_out = fit_med, which = "lbound", opts = opts0, history = FALSE))
system.time(out1u0 <-  ci_bound_nm_i(1, 5, sem_out = fit_med, which = "ubound", opts = opts0, history = TRUE))
system.time(out1lc <- ci_bound_nmc_i(1, sem_out = fit_med, which = "lbound", opts = opts0, history = FALSE))
system.time(out1uc <- ci_bound_nmc_i(1, sem_out = fit_med, which = "ubound", opts = opts0, history = TRUE))
system.time(out2lc <- ci_bound_nmc_i(2, sem_out = fit_med, which = "lbound", opts = opts0, history = FALSE))
system.time(out2uc <- ci_bound_nmc_i(2, sem_out = fit_med, which = "ubound", opts = opts0, history = TRUE))
system.time(out6lc <- ci_bound_nmc_i(6, 5, sem_out = fit_med, which = "lbound", opts = opts0, test_generic = TRUE))
system.time(out6uc <- ci_bound_nmc_i(6, 5, sem_out = fit_med, which = "ubound", opts = opts0, test_generic = TRUE))

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
    mxAlgebra(a * a , name = "asq"),
    mxCI(reference = c("a", "asq"), 
                       interval = .95, type = "both"),
    mxData(observed = cov_dat, type = "cov", numObs = n)
  )

# To replicate OpenMx 2.18.1 with SLSQP
mod_mx <- mxOption(mod_mx, "Feasibility tolerance", "1e-6")

fit_med_OpenMx <- mxRun(mod_mx, silent = TRUE, intervals = TRUE)
ci_OpenMx <- summary(fit_med_OpenMx)$CI
ci_semlbci <- c(out1lc, out6lc, out1uc, out6uc)

test_that("Equal to OpenMx LBCI", {
    expect_equal(
        ci_semlbci, 
         unlist(ci_OpenMx[, c("lbound", "ubound")]),
        tolerance = 1e-6,
        ignore_attr = TRUE
      )
  })
