skip("WIP: Tests not passed or tests not ready. To fix")

library(testthat)
library(semlbci)

library(lavaan)

data(cfa_two_factors)
dat <- cfa_two_factors
mod <- 
"
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
"
fit_cfa <- lavaan::cfa(mod, dat)
ptable <- parameterTable(fit_cfa)
ptable

fn_constr0 <- set_constraint(fit_cfa)

# opts0 <- list(print_level = 3)
opts0 <- list()
opts0 <- list(ftol_abs = 1e-7,
              ftol_rel = 1e-7,
              xtol_abs = 1e-7,
              xtol_rel = 1e-7,
              tol_constraints_eq = 1e-10
              )
system.time(out02l <- ci_bound_wn_i(2, 13, sem_out = fit_cfa, which = "lbound", opts = opts0, f_constr = fn_constr0, standardized = TRUE))
system.time(out06u <- ci_bound_wn_i(6, 13, sem_out = fit_cfa, which = "ubound", opts = opts0, f_constr = fn_constr0, standardized = TRUE))


gen_fct <- function(fit) {
    force(fit)
    tmpfct <- function(which, ...) {
        force(fit)
        .x. <- get(".x.", envir = parent.frame())
        fit@Model <- lavaan::lav_model_set_parameters(fit@Model, .x.)
        fit_pt2 <- lavaan::parameterTable(fit)
        fit_pt2[fit_pt2$free > 0, "est"] <- .x.
        fit@ParTable <- as.list(fit_pt2)
        std <- lavaan::standardizedSolution(fit, se = FALSE)
        std[which, "est.std"]
      }
    return(tmpfct)
  }

fct1 <- gen_fct(fit = fit_cfa)

modc0 <- 
"
f1 =~ x1 + b*x2 + c*x3
f2 =~ x4 + d*x5 + e*x6
"
modc <- paste(modc0, "\ng1 := fct1(2)\ng1 == ", out02l,
                     "\nb < 0")
cat(modc)
fit_cfac <- lavaan::cfa(modc, dat)
anova(fit_cfac, fit_cfa)

test_that("Check p-value for the chi-square difference test", {
    expect_equal(
        anova(fit_cfac, fit_cfa)[2, "Pr(>Chisq)"], 
        .05,
        tolerance = 1e-6,
        ignore_attr = TRUE
      )
  })

modc <- paste(modc0, "\ng1 := fct1(6)\ng1 == ", out06u,
                     "\nb < 0")
cat(modc)
fit_cfac <- lavaan::cfa(modc, dat)
anova(fit_cfac, fit_cfa)

test_that("Check p-value for the chi-square difference test", {
    expect_equal(
        anova(fit_cfac, fit_cfa)[2, "Pr(>Chisq)"], 
        .05,
        tolerance = 1e-6,
        ignore_attr = TRUE
      )
  })



#
 
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

fn_constr0 <- set_constraint(fit_lavaan)

# opts0 <- list(print_level = 3)
opts0 <- list()
opts0 <- list(ftol_abs = 1e-7,
              ftol_rel = 1e-7,
              xtol_abs = 1e-7,
              xtol_rel = 1e-7,
              tol_constraints_eq = 1e-10
              )
system.time(out02l <- ci_bound_wn_i(2, 21, sem_out = fit_lavaan, which = "lbound", opts = opts0, f_constr = fn_constr0))
system.time(out02u <- ci_bound_wn_i(2, 21, sem_out = fit_lavaan, which = "ubound", opts = opts0, f_constr = fn_constr0))
system.time(out06l <- ci_bound_wn_i(6, 21, sem_out = fit_lavaan, which = "lbound", opts = opts0, f_constr = fn_constr0))
system.time(out06u <- ci_bound_wn_i(6, 21, sem_out = fit_lavaan, which = "ubound", opts = opts0, f_constr = fn_constr0))
system.time(out19l <- ci_bound_wn_i(19, 21, sem_out = fit_lavaan, which = "lbound", opts = opts0, f_constr = fn_constr0))
system.time(out19u <- ci_bound_wn_i(19, 21, sem_out = fit_lavaan, which = "ubound", opts = opts0, f_constr = fn_constr0))
system.time(out23l <- ci_bound_wn_i(23, 21, sem_out = fit_lavaan, which = "lbound", opts = opts0, f_constr = fn_constr0))
system.time(out23u <- ci_bound_wn_i(23, 21, sem_out = fit_lavaan, which = "ubound", opts = opts0, f_constr = fn_constr0))

ci_OpenMx <- summary(fit_OpenMx)$CI
ci_lavaan <- matrix(
              c(out02l, out06l, out19l, out23l,
                out02u, out06u, out19u, out23u),
              4, 2)
ci_OpenMx[, c(1, 3)]
# OpenMx could not find one of the limit.

test_that("Equal to OpenMx LBCI", {
    expect_equal(
        as.vector(ci_lavaan)[-7], 
        unlist(ci_OpenMx[, c("lbound", "ubound")])[-7],
        tolerance = 1e-6,
        ignore_attr = TRUE
      )
  })

modc0 <- "
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

modc <- paste(modc0, "\nf11 == ", out19l)
fit_cfac <- lavaan::cfa(modc, dat)
anova(fit_cfac, fit_lavaan)

test_that("Check p-value for the chi-square difference test", {
    expect_equal(
        anova(fit_cfac, fit_lavaan)[2, "Pr(>Chisq)"], 
        .05,
        tolerance = 1e-6,
        ignore_attr = TRUE
      )
  })

