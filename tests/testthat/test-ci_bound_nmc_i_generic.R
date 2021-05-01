skip("WIP")

library(testthat)
library(semlbci)

# context("Check ci_bound_nmc_i: No equality constraints)

# Named generic for backward compatibility
# The argument test_generic is no longer used.


data(simple_med)
dat <- simple_med
mod <- 
"
m ~ a*x
y ~ b*m
ab:= a*b
asq:= a^2
abc:= ab*asq
am:= mean(c(ab, asq, abc))
"
fit_med <- lavaan::sem(mod, simple_med, fixed.x = FALSE)
lavaan::parameterTable(fit_med)

# opts0 <- list(print_level = 3)
opts0 <- list()
opts0 <- list(ftol_abs = 1e-7,
              ftol_rel = 1e-7,
              xtol_abs = 1e-7,
              xtol_rel = 1e-7,
              tol_constraints_eq = 1e-10
              )
# system.time(out1l0 <- ci_bound_nm_i(3, 5, sem_out = fit_med, which = "lbound", opts = opts0))
system.time(out2lc <- ci_bound_nmc_i(2, sem_out = fit_med, which = "lbound", opts = opts0))
system.time(out2uc <- ci_bound_nmc_i(2, sem_out = fit_med, which = "ubound", opts = opts0))
system.time(out3lc <- ci_bound_nmc_i(3, sem_out = fit_med, which = "lbound", opts = opts0))
system.time(out3uc <- ci_bound_nmc_i(3, sem_out = fit_med, which = "ubound", opts = opts0))
# (ci_semlbci <- c(out1l, out2l, out1u, out2u)) -
#   unlist(ci_OpenMx[c("a", "b"), c("lbound", "ubound")])

# mod0 <- 
# "
# m ~ a*x
# y ~ b*m
# m ~~ c*m
# ab:= a*b
# asq:= a^2
# abc:= ab*asq
# am:= mean(c(ab, asq, abc))
# "
# mod0 <- paste(mod0, "\nc == ", out1l0)
# fit_med0 <- lavaan::sem(mod0, simple_med, fixed.x = FALSE)
# anova(fit_med0, fit_med)

modc0 <- 
"
m ~ a*x
y ~ b*m
m ~~ c*m
ab:= a*b
asq:= a^2
abc:= ab*asq
am:= mean(c(ab, asq, abc))
"
modc <- paste(modc0, "\nb == ", out2lc)
fit_medc <- lavaan::sem(modc, simple_med, fixed.x = FALSE)
anova(fit_medc, fit_med)
modc <- paste(modc0, "\nb == ", out2uc)
fit_medc <- lavaan::sem(modc, simple_med, fixed.x = FALSE)
anova(fit_medc, fit_med)
modc <- paste(modc0, "\nc == ", out3lc)
fit_medc <- lavaan::sem(modc, simple_med, fixed.x = FALSE)
anova(fit_medc, fit_med)
modc <- paste(modc0, "\nc == ", out3uc)
fit_medc <- lavaan::sem(modc, simple_med, fixed.x = FALSE)
anova(fit_medc, fit_med)

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
    mxCI(reference = c("a", "b", "ab", "evar_m"), 
                       interval = .95, type = "both"),
    mxData(observed = cov_dat, type = "cov", numObs = n)
  )

# To replicate OpenMx 2.18.1 with SLSQP
mod_mx <- mxOption(mod_mx, "Feasibility tolerance", "1e-6")

fit_med_OpenMx <- mxRun(mod_mx, silent = TRUE, intervals = TRUE)
ci_OpenMx <- summary(fit_med_OpenMx)$CI[c(2, 4), ]
ci_semlbci <- c(out2lc, out3lc, out2uc, out3uc)

test_that("Equal to OpenMx LBCI", {
    expect_equal(
        ci_semlbci, 
         unlist(ci_OpenMx[, c("lbound", "ubound")]),
        tolerance = 1e-5,
        ignore_attr = TRUE
      )
  })
