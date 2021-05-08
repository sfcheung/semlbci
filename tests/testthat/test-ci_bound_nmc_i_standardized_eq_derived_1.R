skip("WIP: ci_bound_nmc_i on hold")

skip("WIP: Tests not passed or tests not ready. To fix")

library(testthat)
library(semlbci)

data(simple_med)
dat <- simple_med
mod <- 
"
m ~ a*x
y ~ b*m
ab := a*b
a == b
"
fit_med <- lavaan::sem(mod, simple_med, fixed.x = FALSE)
lavaan::parameterTable(fit_med)
lavaan::standardizedSolution(fit_med)

# opts0 <- list(print_level = 3)
opts0 <- list()
opts0 <- list(ftol_abs = 1e-4,
              ftol_rel = 1e-4,
              xtol_abs = 1e-4,
              xtol_rel = 1e-4
              # tol_constraints_eq = 1e-7
              )
system.time(out1l <- ci_bound_nmc_i(1, 5, sem_out = fit_med, which = "lbound", standardized = TRUE, opts = opts0))
system.time(out1u <- ci_bound_nmc_i(1, 5, sem_out = fit_med, which = "ubound", standardized = TRUE, opts = opts0))
system.time(out2l <- ci_bound_nmc_i(6, 5, sem_out = fit_med, which = "lbound", standardized = TRUE, opts = opts0))
system.time(out2u <- ci_bound_nmc_i(6, 5, sem_out = fit_med, which = "ubound", standardized = TRUE, opts = opts0))


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

fit_med_OpenMx <- mxRun(mod_mx, silent = TRUE, intervals = TRUE)
ci_OpenMx <- summary(fit_med_OpenMx)$CI

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
