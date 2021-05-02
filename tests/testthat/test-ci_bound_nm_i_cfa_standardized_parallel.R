skip("WIP: Tests not passed or tests not ready. To fix")
skip("Test parallel processing: To be tested in interactive sessions only.")

library(testthat)
library(semlbci)

# Do CFA later. Too slow for lavaan

data(cfa_two_factors)
dat <- cfa_two_factors
mod <- 
"
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
"
fit_cfa <- lavaan::sem(mod, dat)

# opts0 <- list(print_level = 3)
opts0 <- list()
opts0 <- list(ftol_abs = 1e-7,
              ftol_rel = 1e-7,
              xtol_abs = 1e-7,
              xtol_rel = 1e-7,
              tol_constraints_eq = 1e-7
              )
system.time(out <- semlbci(fit_cfa, c(2, 3, 5, 6), opts = opts0, parallel = TRUE, ncpu = 4, standardized = TRUE))
system.time({
      out2l <- ci_bound_nm_i(2, sem_out = fit_cfa, which = "lbound", opts = opts0, standardized = TRUE)
      out2u <- ci_bound_nm_i(2, sem_out = fit_cfa, which = "ubound", opts = opts0, standardized = TRUE)
      out3l <- ci_bound_nm_i(3, sem_out = fit_cfa, which = "lbound", opts = opts0, standardized = TRUE)
      out3u <- ci_bound_nm_i(3, sem_out = fit_cfa, which = "ubound", opts = opts0, standardized = TRUE)
      out5l <- ci_bound_nm_i(5, sem_out = fit_cfa, which = "lbound", opts = opts0, standardized = TRUE)
      out5u <- ci_bound_nm_i(5, sem_out = fit_cfa, which = "ubound", opts = opts0, standardized = TRUE)
      out6l <- ci_bound_nm_i(6, sem_out = fit_cfa, which = "lbound", opts = opts0, standardized = TRUE)
      out6u <- ci_bound_nm_i(6, sem_out = fit_cfa, which = "ubound", opts = opts0, standardized = TRUE)
      })

system.time(out <- semlbci(fit_cfa, c(1:12), standardized = TRUE, opts = opts0, parallel = TRUE, ncpu = 6))
system.time(out <- semlbci(fit_cfa, c(1:12), standardized = TRUE, opts = opts0))


# TO-DO: Ensure CFA results in lavaan and OpenMx are the same first

library(lavaan)
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

fit <- cfa(HS.model, data = HolzingerSwineford1939)
parameterEstimates(fit)

library(OpenMx)
cov_dat <- cov(HolzingerSwineford1939[, paste0("x", 1:9)])
n <- nrow(HolzingerSwineford1939)
manifest_vars <- paste0("x", 1:9)
latent_vars   <- c("visual", "textual", "speed")
mod_mx <- mxModel("CFA", type = "RAM", 
    manifestVars = manifest_vars, latentVars = latent_vars,
    mxPath(from = manifest_vars, arrows = 2, free = TRUE, values = 1,
                  labels = paste0("e", 1:9)),
    mxPath(from = "visual", arrows = 2, free = TRUE, values = 1,
                  labels = "phi11"),
    mxPath(from = "textual", arrows = 2, free = TRUE, values = 1,
                  labels = "phi22"),
    mxPath(from = "speed", arrows = 2, free = TRUE, values = 1,
                  labels = "phi33"),
    mxPath(from = "visual", to = "textual", arrows = 2, 
                  free = TRUE, values = 0,
                  labels = "phi12"),
    mxPath(from = "visual", to = "speed", arrows = 2, 
                  free = TRUE, values = 0,
                  labels = "phi13"),
    mxPath(from = "textual", to = "speed", arrows = 2, 
                  free = TRUE, values = 0,
                  labels = "phi23"),
    mxPath(from = "visual", to = manifest_vars[1:3], arrows = 1,
          free = c(FALSE, TRUE, TRUE), values = 1,
          labels = c("lambda11", "lambda21", "lambda31")),
    mxPath(from = "textual", to = manifest_vars[4:6], arrows = 1,
          free = c(FALSE, TRUE, TRUE), values = 1,
          labels = c("lambda42", "lambda52", "lambda62")),
    mxPath(from = "speed", to = manifest_vars[7:9], arrows = 1,
          free = c(FALSE, TRUE, TRUE), values = 1,
          labels = c("lambda73", "lambda83", "lambda93")),
    mxCI(reference = c("lambda21", "lambda31",
                       "lambda52", "lambda62",
                       "lambda83", "lambda93",
                       "phi11", "phi22", "phi33",
                       "phi12", "phi13", "phi23"), 
                       interval = .95, type = "both"),
    mxData(observed = cov_dat, type = "cov", numObs = n)
  )

fit_cfa_OpenMx <- mxRun(mod_mx, silent = TRUE, intervals = TRUE)
ci_cfa_OpenMx <- summary(fit_cfa_OpenMx)$CI

ci_cfa_OpenMx
parameterEstimates(fit, remove.nonfree = TRUE)

ci_cfa_lavaan <- rbind(
  c(out2l, out2u),
  c(out3l, out3u),
  c(out5l, out5u),
  c(out6l, out6u))


opts0 <- list()
opts0 <- list(ftol_abs = 1e-5,
              ftol_rel = 1e-5,
              xtol_abs = 1e-5,
              xtol_rel = 1e-5,
              tol_constraints_eq = 1e-4
              )
ci_cfa_lavaan <- semlbci(fit, pars = c(2, 3, 5, 6), opts = opts0)
ci_cfa_lavaan[c(2, 3, 5, 6), 1:7]
ci_cfa_OpenMx[1:4, c(1, 2, 3)]
