skip("WIP: Tests not passed or tests not ready. To fix")

library(testthat)
library(semlbci)

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


# To replicate OpenMx 2.18.1 with SLSQP
mod_mx <- mxOption(mod_mx, "Feasibility tolerance", "1e-6")

fit_cfa_OpenMx <- mxRun(mod_mx, silent = TRUE, intervals = TRUE)
ci_cfa_OpenMx <- summary(fit_cfa_OpenMx)$CI

# Compare results

test_that("lavaan and OpenMx estimates are equal", {
    expect_equal(
        coef(fit_cfa_OpenMx)[1:9], 
        coef(fit)[1:9],
        tolerance = 1e-5,
        ignore_attr = TRUE
      )
    expect_equal(
        coef(fit_cfa_OpenMx)[c(16, 18, 21, 17, 19, 20)], 
        coef(fit)[16:21],
        tolerance = 1e-5,
        ignore_attr = TRUE
      )
  })

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

ci_cfa_lavaan[c(2, 3, 5, 6), ]
summary(fit_cfa_OpenMx, verbose = TRUE)$CIdetail[1:4, ]
