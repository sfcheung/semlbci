skip("WIP: Tests not passed or tests not ready. To fix")

library(testthat)
library(semlbci)

# context("Check ci_bound_wn_i: Derived parameters")

data(simple_med)
dat <- simple_med
mod <- 
"
m ~ a*x
y ~ a*m
ab:= a*a
"
fit_med <- lavaan::sem(mod, simple_med, fixed.x = FALSE)
lavaan::parameterTable(fit_med)

system.time(out2l <- ci_bound_nmc_i(2, 5, sem_out = fit_med, which = "lbound"))
system.time(out2u <- ci_bound_nmc_i(2, 5, sem_out = fit_med, which = "ubound"))
system.time(out6l <- ci_bound_nmc_i(6, 5, sem_out = fit_med, which = "lbound"))
system.time(out6u <- ci_bound_nmc_i(6, 5, sem_out = fit_med, which = "ubound"))

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
    mxCI(reference = c("a", "ab"), 
                       interval = .95, type = "both"),
    mxData(observed = cov_dat, type = "cov", numObs = n)
  )

# To replicate OpenMx 2.18.1 with SLSQP
mod_mx <- mxOption(mod_mx, "Feasibility tolerance", "1e-6")

fit_med_OpenMx <- mxRun(mod_mx, silent = TRUE, intervals = TRUE)
ci_OpenMx <- summary(fit_med_OpenMx)$CI

ci_semlbci <- c(out2l, out6l, out2u, out6u)

test_that("Equal to OpenMx LBCI", {
    expect_equal(
        ci_semlbci, 
        unlist(ci_OpenMx[, c("lbound", "ubound")]),
        tolerance = 1e-6,
        ignore_attr = TRUE
      )
  })
