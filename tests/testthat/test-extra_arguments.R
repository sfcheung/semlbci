library(testthat)
library(semlbci)

# context("Check extra arguments")

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
ci_OpenMx_target <- unlist(ci_OpenMx[c("a"), c("lbound", "ubound")])

lbci_debug_TRUE <- semlbci(fit_med, pars = c(1), debug = TRUE, method = "wn")
lbci_debug_FALSE <- semlbci(fit_med, pars = c(1), debug = FALSE, method = "wn")
lbci_lav_warn_TRUE <- semlbci(fit_med, pars = c(1), lav_warn = TRUE, method = "wn")
lbci_lav_warn_FALSE <- semlbci(fit_med, pars = c(1), lav_warn = FALSE, method = "wn")
lbci_history_TRUE <- semlbci(fit_med, pars = c(1), history = TRUE, method = "wn")
lbci_history_FALSE <- semlbci(fit_med, pars = c(1), history = FALSE, method = "wn")

test_that("Can run with debug = TRUE", {
    expect_equal(
        as.numeric(unlist(lbci_debug_TRUE[c(1), c("lbci_lb", "lbci_ub")])),
        ci_OpenMx_target,
        tolerance = 1e-5,
        ignore_attr = TRUE
      )
  })

test_that("Can run with debug = FALSE", {
    expect_equal(
        as.numeric(unlist(lbci_debug_FALSE[c(1), c("lbci_lb", "lbci_ub")])),
        ci_OpenMx_target,
        tolerance = 1e-5,
        ignore_attr = TRUE
      )
  })

test_that("Can run with lav_warn = TRUE", {
    expect_equal(
        as.numeric(unlist(lbci_lav_warn_TRUE[c(1), c("lbci_lb", "lbci_ub")])),
        ci_OpenMx_target,
        tolerance = 1e-5,
        ignore_attr = TRUE
      )
  })

test_that("Can run with lav_warn = FALSE", {
    expect_equal(
        as.numeric(unlist(lbci_lav_warn_FALSE[c(1), c("lbci_lb", "lbci_ub")])),
        ci_OpenMx_target,
        tolerance = 1e-5,
        ignore_attr = TRUE
      )
  })

test_that("Can run with history = TRUE", {
    expect_equal(
        as.numeric(unlist(lbci_history_TRUE[c(1), c("lbci_lb", "lbci_ub")])),
        ci_OpenMx_target,
        tolerance = 1e-5,
        ignore_attr = TRUE
      )
  })

test_that("Can run with history = FALSE", {
    expect_equal(
        as.numeric(unlist(lbci_history_FALSE[c(1), c("lbci_lb", "lbci_ub")])),
        ci_OpenMx_target,
        tolerance = 1e-5,
        ignore_attr = TRUE
      )
  })

