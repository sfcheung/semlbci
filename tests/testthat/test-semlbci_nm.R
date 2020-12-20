library(testthat)
library(semlbci)

# context("Check semlbci: No equality constraints, Neale-Miller-1997")

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
    expect_equal(
        as.numeric(unlist(lbci_med[c(1, 2), c("lbci_lb", "lbci_ub")])), 
        unlist(ci_OpenMx[c("a", "b"), c("lbound", "ubound")]) ,
        tolerance = 1e-5,
        ignore_attr = TRUE
      )
  })

lbci_med2 <- semlbci(fit_med, pars = c(1, 3), method = "nm")

test_that("Check whether only selectd parameters were processed", {
    expect_equal(
        as.numeric(unlist(lbci_med2[c(1), c("lbci_lb", "lbci_ub")])), 
        unlist(ci_OpenMx[c("a"), c("lbound", "ubound")]) ,
        tolerance = 1e-4,
        ignore_attr = TRUE
      )
  })



# Do CFA later. Too slow for lavaan

# dat <- cfa_two_factors
# mod <- 
# "
# f1 =~ x1 + x2 + x3
# f2 =~ x4 + x5 + x6
# "
# fit_cfa <- lavaan::sem(mod, dat)

# library(OpenMx)
# cov_dat <- cov(dat)
# n <- nrow(dat)
# manifest_vars <- paste0("x", 1:6)
# latent_vars   <- paste0("f", 1:2)
# mod_mx <- mxModel("CFA", type = "RAM", 
#     manifestVars = manifest_vars, latentVars = latent_vars,
#     mxPath(from = manifest_vars, arrows = 2, free = TRUE, values = 1,
#                   labels = paste0("e", 1:6)),
#     mxPath(from = "f1", arrows = 2, free = TRUE, values = 1,
#                   labels = "phi11"),
#     mxPath(from = "f2", arrows = 2, free = TRUE, values = 1,
#                   labels = "phi22"),
#     mxPath(from = "f1", to = "f2", arrows = 2, 
#                   free = FALSE, values = 0,
#                   labels = "phi12"),
#     mxPath(from = "f1", to = manifest_vars[1:3], arrows = 1,
#           free = c(FALSE, TRUE, TRUE), values = 1,
#           labels = c("lambda11", "lambda21", "lambda31")),
#     mxPath(from = "f2", to = manifest_vars[4:6], arrows = 1,
#           free = c(FALSE, TRUE, TRUE), values = 1,
#           labels = c("lambda42", "lambda52", "lambda62")),
#     mxCI(reference = c("lambda21", "lambda31", "lambda52", "lambda62",
#                        "phi11", "phi22", "phi12"), 
#                        interval = .95, type = "both"),
#     mxData(observed = cov_dat, type = "cov", numObs = n)
#   )

# fit_cfa_OpenMx <- mxRun(mod_mx, silent = TRUE, intervals = TRUE)
# ci_cfa_OpenMx <- summary(fit_cfa_OpenMx)$CI
# lbci_cfa <- semlbci(fit_cfa)


