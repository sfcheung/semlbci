library(testthat)
library(semlbci)

context("Check ci_bound_i: Standardized, No equality constraints")

data(simple_med)
dat <- simple_med
mod <- 
"
m ~ x
y ~ m
"
fit_med <- lavaan::sem(mod, simple_med, fixed.x = FALSE)

fn_constr0 <- set_constraint(fit_med)

system.time(out1l <- ci_bound_i(1, 5, sem_out = fit_med, f_constr = fn_constr0, which = "lbound", standardized = TRUE))
system.time(out1u <- ci_bound_i(1, 5, sem_out = fit_med, f_constr = fn_constr0, which = "ubound", standardized = TRUE))
system.time(out2l <- ci_bound_i(2, 5, sem_out = fit_med, f_constr = fn_constr0, which = "lbound", standardized = TRUE))
system.time(out2u <- ci_bound_i(2, 5, sem_out = fit_med, f_constr = fn_constr0, which = "ubound", standardized = TRUE))


library(OpenMx)
cov_dat <- cov(dat)
n <- nrow(dat)
manifest_vars <- c("x", "m", "y")
mod_mx <- mxModel("Mediation", type = "RAM", 
    manifestVars = manifest_vars,
    mxPath(from = "x", to = "m", arrows = 1, free = TRUE, values = 1,
                  labels = "a"),
    mxPath(from = "m", to = "y", arrows = 1, free = TRUE, values = 1,
                  labels = "b"),
    mxPath(from = "x", arrows = 2, free = TRUE, values = 1,
                  labels = "var_x"),
    mxPath(from = "m", arrows = 2, free = TRUE, values = 1,
                  labels = "evar_m"),
    mxPath(from = "y", arrows = 2, free = TRUE, values = 1,
                  labels = "evar_y"),
    mxAlgebra(a * b , name = "ab"),
    mxAlgebra((a^2) * var_x + evar_m, name = "var_m"),
    mxAlgebra((b^2) * var_m + evar_y, name = "var_y"),
    mxAlgebra(a * sqrt(var_x) / sqrt(var_m), name = "a_std"),
    mxAlgebra(b * sqrt(var_m) / sqrt(var_y) , name = "b_std"),
    mxCI(reference = c("a", "b", "ab", "a_std", "b_std"), 
                       interval = .95, type = "both"),
    mxData(observed = cov_dat, type = "cov", numObs = n)
  )
fit_med_OpenMx <- mxRun(mod_mx, silent = TRUE, intervals = TRUE)
ci_OpenMx <- summary(fit_med_OpenMx)$CI

ci_semlbci <- c(out1l, out2l, out1u, out2u)

test_that("Equal to OpenMx LBCI", {
    expect_equivalent(
        ci_semlbci, 
        unlist(ci_OpenMx[c(4, 5), c("lbound", "ubound")]),
        tolerance = 1e-3
      )
  })
