
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
  visual  ~ f1x * textual
  visual  ~ f1x * speed
  textual ~~ f22 * textual
  textual ~~ f23 * speed
  speed   ~~ f33 * speed
  f1213 := f1x * f1x
"

fit_lavaan <- sem(mod, dat)
coef_lavaan <- coef(fit_lavaan)

library(OpenMx)
tmp <- matrix(FALSE, 12, 12)
tmp[2:3, 10] <- tmp[5:6, 11] <- tmp[8:9, 12] <- TRUE
tmp[10, 11:12] <- TRUE
tmp2 <- matrix(0, 12, 12)
tmp2[1:3, 10] <- tmp2[4:6, 11] <- tmp2[7:9, 12] <- 1
tmp2[10, 11:12] <- 1
tmp3 <- matrix(NA, 12, 12)
tmp3[2:3, 10] <- c("l2", "l3")
tmp3[5:6, 11] <- c("l5", "l6")
tmp3[8:9, 12] <- c("l8", "l9")
tmp3[10, 11:12] <- c("f1x", "f1x")
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
tmp[11:12, 10] <- tmp[10, 11:12] <- FALSE
tmp2 <- matrix(0, 12, 12)
diag(tmp2) <- 0.2
tmp2[10:12, 10:12] <- 0.2
tmp2[11:12, 10] <- tmp2[10, 11:12] <- 0
tmp3 <- matrix(NA, 12, 12)
diag(tmp3) <- paste0("p", 1:12)
tmp3[10:12, 10:12] <- c("f11", "f12", "f13", "f12", "f22", "f23", "f13", "f23", "f33")
tmp3[11:12, 10] <- tmp3[10, 11:12] <- NA
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
  mx.algebras = list(f1213 = mxAlgebra(f1x * f1x, name = "f1213")),
  mxCI(reference = c("l2", "l6",
                     "f11", "f23", "f1213"), 
                      interval = .95, type = "both")
    )
mod_OpenMx <- mxOption(mod_OpenMx, "Feasibility tolerance", "1e-6")
fit_OpenMx <- mxRun(mod_OpenMx, silent = TRUE, intervals = TRUE)
coef_OpenMx <- coef(fit_OpenMx)
coef_OpenMx <- coef_OpenMx[names(coef_lavaan)]
coef_lavaan - coef_OpenMx

ptable <- parameterTable(fit_lavaan)
ptable

fn_constr0 <- set_constraint(fit_lavaan)

# opts0 <- list(print_level = 3)
opts0 <- list()
opts0 <- list(ftol_abs = 1e-7,
              ftol_rel = 1e-7,
              xtol_abs = 1e-7,
              xtol_rel = 1e-7
              )
system.time(out06l <- ci_bound_wn_i(6, 21, sem_out = fit_lavaan, which = "lbound", opts = opts0, f_constr = fn_constr0))
system.time(out06u <- ci_bound_wn_i(6, 21, sem_out = fit_lavaan, which = "ubound", opts = opts0, f_constr = fn_constr0))
# system.time(out11l <- ci_bound_wn_i(11, 21, sem_out = fit_lavaan, which = "lbound", opts = opts0, f_constr = fn_constr0))
# system.time(out11u <- ci_bound_wn_i(11, 21, sem_out = fit_lavaan, which = "ubound", opts = opts0, f_constr = fn_constr0))
# system.time(out23l <- ci_bound_wn_i(23, 21, sem_out = fit_lavaan, which = "lbound", opts = opts0, f_constr = fn_constr0))
# system.time(out23u <- ci_bound_wn_i(23, 21, sem_out = fit_lavaan, which = "ubound", opts = opts0, f_constr = fn_constr0))
system.time(out25l <- ci_bound_wn_i(25, 21, sem_out = fit_lavaan, which = "lbound", opts = opts0, f_constr = fn_constr0))
system.time(out25u <- ci_bound_wn_i(25, 21, sem_out = fit_lavaan, which = "ubound", opts = opts0, f_constr = fn_constr0))

ci_OpenMx <- summary(fit_OpenMx)$CI
ci_lavaan <- matrix(
              c(out06l, out25l,
                out06u, out25u), 2, 2)

test_that("Equal to OpenMx LBCI", {
    expect_equal(
        as.vector(ci_lavaan),
        unlist(ci_OpenMx[c(2,5), c("lbound", "ubound")]),
        tolerance = 1e-4,
        ignore_attr = TRUE
      )
  })
