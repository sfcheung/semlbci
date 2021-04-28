skip("WIP")

library(testthat)
library(semlbci)

library(lavaan)

dat <- HolzingerSwineford1939
mod <- "
  visual  =~ 1 * x1 + l2 * x2 + l3 * x3 
  textual =~ 1 * x4 + l5 * x5 + l6 * x6
  speed   =~ 1 * x7 + l8 * x8 + l9 * x9
  x1 ~~ p1 * x1
  x2 ~~ p2 * x2
  x3 ~~ p3 * x3
  x4 ~~ p4 * x4
  x5 ~~ p5 * x5
  x6 ~~ p6 * x6
  x7 ~~ p7 * x7
  x8 ~~ p8 * x8
  x9 ~~ p9 * x9
  visual  ~~ f11 * visual
  visual  ~~ f12 * textual
  visual  ~~ f13 * speed
  textual ~~ f22 * textual
  textual ~~ f23 * speed
  speed   ~~ f33 * speed
"

fit_lavaan <- cfa(mod, dat)
coef_lavaan <- coef(fit_lavaan)


library(OpenMx)
tmp <- matrix(FALSE, 12, 12)
tmp[2:3, 10] <- tmp[5:6, 11] <- tmp[8:9, 12] <- TRUE
tmp2 <- matrix(0, 12, 12)
tmp2[1:3, 10] <- tmp2[4:6, 11] <- tmp2[7:9, 12] <- 1
tmp3 <- matrix(NA, 12, 12)
tmp3[2:3, 10] <- c("l2", "l3")
tmp3[5:6, 11] <- c("l5", "l6")
tmp3[8:9, 12] <- c("l8", "l9")
A <- mxMatrix(
  type = "Full",
  nrow = 12,
  ncol = 12,
  free = tmp,
  values = tmp2,
  labels = tmp3,
  byrow = TRUE, name = "A"
)
tmp <- matrix(FALSE, 12, 12)
diag(tmp) <- TRUE
tmp[10:12, 10:12] <- TRUE
tmp2 <- matrix(0, 12, 12)
diag(tmp2) <- 0.2
tmp2[10:12, 10:12] <- 0.2
tmp3 <- matrix(NA, 12, 12)
diag(tmp3) <- paste0("p", 1:12)
tmp3[10:12, 10:12] <- c("f11", "f12", "f13", "f12", "f22", "f23", "f13", "f23", "f33")
S <- mxMatrix(
  type = "Symm",
  nrow = 12,
  ncol = 12,
  free = tmp,
  values = tmp2,
  labels = tmp3,
  byrow = TRUE,
  name = "S"
)
tmp <- cbind(diag(9), matrix(0, 9, 3))
F <- mxMatrix(
  type = "Full",
  nrow = 9,
  ncol = 12,
  free = FALSE,
  values = tmp,
  byrow = TRUE,
  name = "F"
)
expRAM <- mxExpectationRAM(
  A = "A",
  S = "S",
  F = "F",
  dimnames = c(
    "x1", "x2", "x3",
    "x4", "x5", "x6",
    "x7", "x8", "x9",
    "visual",
    "textual",
    "speed"
  )
)
dat_cov <- cov(dat[, paste0("x", 1:9)])
n <- nrow(dat)
mod_OpenMx <- mxModel(
  name = "model",
  data = OpenMx::mxData(
      dat_cov,
      type = "cov",
      numObs = n
    ),
  matrices = list(A, S, F),
  expectation = expRAM,
  fitfunction = mxFitFunctionML(),
  mxCI(reference = c("l2", "l6",
                     "f11", "f23"), 
                      interval = .95, type = "both")
    )
mod_OpenMx <- mxOption(mod_OpenMx, "Feasibility tolerance", "1e-6")
fit_OpenMx <- mxRun(mod_OpenMx, silent = TRUE, intervals = TRUE)
coef_OpenMx <- coef(fit_OpenMx)
coef_OpenMx <- coef_OpenMx[names(coef_lavaan)]
coef_lavaan - coef_OpenMx


# opts0 <- list(print_level = 3)
opts0 <- list()
opts0 <- list(ftol_abs = 1e-7,
              ftol_rel = 1e-7,
              xtol_abs = 1e-7,
              xtol_rel = 1e-7,
              tol_constraints_eq = 1e-10
              )
system.time(out02l <- ci_bound_nm_i(2, sem_out = fit_lavaan, which = "lbound", opts = opts0))
system.time(out02u <- ci_bound_nm_i(2, sem_out = fit_lavaan, which = "ubound", opts = opts0))
system.time(out06l <- ci_bound_nm_i(6, sem_out = fit_lavaan, which = "lbound", opts = opts0))
system.time(out06u <- ci_bound_nm_i(6, sem_out = fit_lavaan, which = "ubound", opts = opts0))
system.time(out19l <- ci_bound_nm_i(19, sem_out = fit_lavaan, which = "lbound", opts = opts0))
system.time(out19u <- ci_bound_nm_i(19, sem_out = fit_lavaan, which = "ubound", opts = opts0))
system.time(out23l <- ci_bound_nm_i(23, sem_out = fit_lavaan, which = "lbound", opts = opts0))
system.time(out23u <- ci_bound_nm_i(23, sem_out = fit_lavaan, which = "ubound", opts = opts0))

ci_lavaan <- semlbci(fit_lavaan, pars = c(2, 6, 19, 23), opts = opts0)
ci_lavaan[c(2, 6, 19, 23), ]
#summary(fit_OpenMx, verbose = TRUE)
ci_OpenMx <- summary(fit_OpenMx)$CI
# ci_lavaan <- c(out02l, out06l, out19l, out23l,
#                out02u, out06u, out19u, out23u)

ci_OpenMx[, c(1, 3)]
ci_lavaan[c(2, 6, 19, 23), c("lbci_lb", "lbci_ub")]

ci_lavaan <- semlbci(fit_lavaan, pars = c(2, 3, 5, 6, 8, 9, 19:24), opts = opts0)
ci_lavaan

ci_lavaan <- semlbci(fit_lavaan, pars = c(6), opts = opts0, method = "wn")




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
system.time(out2l <- ci_bound_nm_i(2, sem_out = fit_cfa, which = "lbound", opts = opts0))
system.time(out2u <- ci_bound_nm_i(2, sem_out = fit_cfa, which = "ubound", opts = opts0))
system.time(out3l <- ci_bound_nm_i(3, sem_out = fit_cfa, which = "lbound", opts = opts0))
system.time(out3u <- ci_bound_nm_i(3, sem_out = fit_cfa, which = "ubound", opts = opts0))
system.time(out5l <- ci_bound_nm_i(5, sem_out = fit_cfa, which = "lbound", opts = opts0))
system.time(out5u <- ci_bound_nm_i(5, sem_out = fit_cfa, which = "ubound", opts = opts0))
system.time(out6l <- ci_bound_nm_i(6, sem_out = fit_cfa, which = "lbound", opts = opts0))
system.time(out6u <- ci_bound_nm_i(6, sem_out = fit_cfa, which = "ubound", opts = opts0))

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
ci_cfa_lavaan <- semlbci(fit, pars = c(2, 3, 5, 6), opts = opts0, standardized = TRUE)
system.time(out2l <- ci_bound_nm_i(2, sem_out = fit_cfa, which = "lbound", opts = opts0, standardized = TRUE))
system.time(out3l <- ci_bound_nm_i(3, sem_out = fit_cfa, which = "lbound", opts = opts0, standardized = TRUE))
system.time(out5l <- ci_bound_nm_i(5, sem_out = fit_cfa, which = "lbound", opts = opts0, standardized = TRUE))
system.time(out6l <- ci_bound_nm_i(6, sem_out = fit_cfa, which = "lbound", opts = opts0, standardized = TRUE))
ci_cfa_lavaan[c(2, 3, 5, 6), 1:7]
ci_cfa_OpenMx[1:4, c(1, 2, 3)]
