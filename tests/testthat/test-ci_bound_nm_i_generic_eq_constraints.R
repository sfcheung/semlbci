library(testthat)
library(semlbci)

context("Check ci_bound_nm_i: With equality constraints, test_generic = TRUE")

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
              print_level = 3
              )
system.time(out1l <- ci_bound_nm_i(1, 5, sem_out = fit_med, which = "lbound", opts = opts0, test_generic = TRUE, history = FALSE))
system.time(out1u <- ci_bound_nm_i(1, 5, sem_out = fit_med, which = "ubound", opts = opts0, test_generic = TRUE, history = TRUE))
system.time(out2l <- ci_bound_nm_i(2, 5, sem_out = fit_med, which = "lbound", opts = opts0, test_generic = TRUE))
system.time(out2u <- ci_bound_nm_i(2, 5, sem_out = fit_med, which = "ubound", opts = opts0, test_generic = TRUE))
system.time(out3l <- ci_bound_nm_i(6, 5, sem_out = fit_med, which = "lbound", opts = opts0, test_generic = TRUE))
system.time(out3u <- ci_bound_nm_i(6, 5, sem_out = fit_med, which = "ubound", opts = opts0, test_generic = TRUE))

system.time(out1sl <- ci_bound_nm_i(3, 5, sem_out = fit_med, which = "lbound", opts = opts0, test_generic = TRUE, history = TRUE, standardized = TRUE))
system.time(out1su <- ci_bound_nm_i(3, 5, sem_out = fit_med, which = "ubound", opts = opts0, test_generic = TRUE, history = TRUE, standardized = TRUE))

# (ci_semlbci <- c(out1l, out2l, out1u, out2u)) -
#   unlist(ci_OpenMx[c("a", "b"), c("lbound", "ubound")])

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
fit_med_OpenMx <- mxRun(mod_mx, silent = TRUE, intervals = TRUE)
ci_OpenMx <- summary(fit_med_OpenMx)$CI
ci_semlbci <- c(out1l, out2l, out3l, out1u, out2u, out3u)

test_that("Equal to OpenMx LBCI", {
    expect_equivalent(
        ci_semlbci, 
         unlist(ci_OpenMx[c(1, 1, 2), c("lbound", "ubound")]),
        tolerance = 1e-2
      )
  })
