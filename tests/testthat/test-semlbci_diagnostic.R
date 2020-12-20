library(testthat)
library(semlbci)

# Disabled

# context("Check semlbci: Diagnostic info")

data(simple_med)
dat <- simple_med
# mod <- 
# "
# m ~ x
# y ~ m
# "
# fit_med <- lavaan::sem(mod, simple_med, fixed.x = FALSE)

# library(OpenMx)
# cov_dat <- cov(dat)
# n <- nrow(dat)
# manifest_vars <- c("x", "m", "y")
# mod_mx <- mxModel("Mediation", type = "RAM", 
#     manifestVars = manifest_vars,
#     mxPath(from = "x", to = "m", arrows = 1, free = TRUE, values = 1,
#                   labels = "a"),
#     mxPath(from = "m", to = "y", arrows = 1, free = TRUE, values = 1,
#                   labels = "b"),
#     mxPath(from = "x", arrows = 2, free = TRUE, values = 1,
#                   labels = "var_x"),
#     mxPath(from = "m", arrows = 2, free = TRUE, values = 1,
#                   labels = "evar_m"),
#     mxPath(from = "y", arrows = 2, free = TRUE, values = 1,
#                   labels = "evar_y"),
#     mxAlgebra(a * b , name = "ab"),
#     mxCI(reference = c("a", "b", "ab", "evar_m"), 
#                        interval = .95, type = "both"),
#     mxData(observed = cov_dat, type = "cov", numObs = n)
#   )
# fit_med_OpenMx <- mxRun(mod_mx, silent = TRUE, intervals = TRUE)
# ci_OpenMx <- summary(fit_med_OpenMx)$CI

# lbci_med <- semlbci(fit_med, method = "nm")
# lbci_med_std <- semlbci(fit_med, pars = c("m ~  x",
#                                           "m ~~ m",
#                                           "y ~~ y"),
#                         method = "nm", standardized = TRUE)
# lbci_med_wn <- semlbci(fit_med, method = "wn")
# lbci_med_wn_std <- semlbci(fit_med, pars = c("m ~  x",
#                                              "y ~  m",
#                                              "y ~~ y"),
#                             method = "wn", standardized = TRUE)

mod2 <- 
"
m ~ a*x
y ~ b*m
ab := a * b
"
fit_med2 <- lavaan::sem(mod2, simple_med, fixed.x = FALSE)

# lbci_med2 <- semlbci(fit_med2, method = "nm")
# lbci_med2 <- semlbci(fit_med2, pars = c("m ~x", "y ~ m", "ab :="), method = "nm")
# lbci_med2 <- semlbci(fit_med2, pars = c("m ~x", "y ~ m", "ab :="), method = "nm", verbose = TRUE)
# lbci_med2_std <- semlbci(fit_med2, pars = c("m ~ x",
#                                             "y ~ m",
#                                             "ab := "),
#                         method = "nm", standardized = TRUE)
# lbci_med2_wn <- semlbci(fit_med2, method = "wn")
# lbci_med2_wn_std <- semlbci(fit_med2, pars = c("m ~  x",
#                                                "y ~  m",
#                                                "ab := "),
#                             method = "wn", standardized = TRUE)

