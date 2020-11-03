library(testthat)
library(semlbci)

context("Check ci_bound_i: Derived parameters")

data(simple_med)
dat <- simple_med
mod <- 
"
m ~ a*x
y ~ b*m
ab:= a*b
asq:= a^2
"
fit_med <- lavaan::sem(mod, simple_med, fixed.x = FALSE)
lavaan::parameterTable(fit_med)

fn_constr0 <- set_constraint(fit_med)

out1l <- ci_bound_i(6, 5, sem_out = fit_med, f_constr = fn_constr0, which = "lbound")
out1u <- ci_bound_i(6, 5, sem_out = fit_med, f_constr = fn_constr0, which = "ubound")
out2l <- ci_bound_i(7, 5, sem_out = fit_med, f_constr = fn_constr0, which = "lbound")
out2u <- ci_bound_i(7, 5, sem_out = fit_med, f_constr = fn_constr0, which = "ubound")


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
    mxAlgebra(a ^ 2 , name = "asq"),
    mxCI(reference = c("a", "b", "ab", "asq"), 
                       interval = .95, type = "both"),
    mxData(observed = cov_dat, type = "cov", numObs = n)
  )
fit_med_OpenMx <- mxRun(mod_mx, silent = TRUE, intervals = TRUE)
ci_OpenMx <- summary(fit_med_OpenMx)$CI

ci_semlbci <- c(out1l, out2l, out1u, out2u)

test_that("Equal to OpenMx LBCI", {
    expect_equivalent(
        ci_semlbci, 
        unlist(ci_OpenMx[c(3, 4), c("lbound", "ubound")]),
        tolerance = 1e-4
      )
  })
