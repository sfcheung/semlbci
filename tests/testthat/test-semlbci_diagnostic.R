# WARNING!
# WIP. Not ready. semlbci not yet works for the CFA model.

library(testthat)
library(semlbci)

context("Check semlbci: Diagnostic info")

data(simple_med)
dat <- simple_med
mod <- 
"
m ~ x
y ~ m
"
fit_med <- lavaan::sem(mod, simple_med, fixed.x = FALSE)

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
fit_med_OpenMx <- mxRun(mod_mx, silent = TRUE, intervals = TRUE)
ci_OpenMx <- summary(fit_med_OpenMx)$CI

lbci_med <- semlbci(fit_med, method = "nm")

test_that("Equal to OpenMx LBCIs for free parameters", {
    expect_equivalent(
        as.numeric(unlist(lbci_med[c(1, 2), c("lbci_lb", "lbci_ub")])), 
        unlist(ci_OpenMx[c("a", "b"), c("lbound", "ubound")]) ,
        tolerance = 1e-5
      )
  })

lbci_med2 <- semlbci(fit_med, pars = c(1, 3), method = "nm")

test_that("Check whether only selectd parameters were processed", {
    expect_equivalent(
        as.numeric(unlist(lbci_med2[c(1), c("lbci_lb", "lbci_ub")])), 
        unlist(ci_OpenMx[c("a"), c("lbound", "ubound")]) ,
        tolerance = 1e-4
      )
  })

