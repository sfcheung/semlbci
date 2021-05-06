library(testthat)
library(semlbci)

# context("Check ci_bound_wn_i: Standardized user defined parameters. No constraints.")

library(lavaan)

data(simple_med)
dat <- simple_med
mod <- 
"
m ~ a*x
y ~ b*m
ab := a*b
"
fit_med <- lavaan::sem(mod, simple_med, fixed.x = FALSE)

fn_constr0 <- set_constraint(fit_med)

# system.time(out1l <- ci_bound_wn_i(6, 5, sem_out = fit_med, f_constr = fn_constr0, which = "lbound", standardized = TRUE))
# system.time(out1u <- ci_bound_wn_i(6, 5, sem_out = fit_med, f_constr = fn_constr0, which = "ubound", standardized = TRUE))
system.time(out2l <- ci_bound_wn_i(1, 5, sem_out = fit_med, f_constr = fn_constr0, which = "lbound", standardized = TRUE, verbose = TRUE))
system.time(out2u <- ci_bound_wn_i(1, 5, sem_out = fit_med, f_constr = fn_constr0, which = "ubound", standardized = TRUE, verbose = TRUE))
# system.time(out1ul <- ci_bound_wn_i(6, 5, sem_out = fit_med, f_constr = fn_constr0, which = "lbound", standardized = FALSE))
# system.time(out1uu <- ci_bound_wn_i(6, 5, sem_out = fit_med, f_constr = fn_constr0, which = "ubound", standardized = FALSE))



gen_fct <- function(fit, i) {
    force(fit)
    force(i)
    fit_pt <- lavaan::parameterTable(fit)
    tmpfct <- function(...) {
        force(fit)
        force(i)
        force(fit_pt)
        .x. <- get(".x.", envir = parent.frame())
        fit@Model <- lavaan::lav_model_set_parameters(
                          fit@Model, .x.
                        )
        fit_pt2 <- fit_pt
        fit_pt2[fit_pt$free > 0, "est"] <- .x.
        fit@ParTable <- as.list(fit_pt2)
        std <- lavaan::standardizedSolution(
                          fit,
                          se = FALSE,
                          zstat = FALSE,
                          pvalue = FALSE,
                          ci = FALSE,
                          cov.std = FALSE,
                          remove.eq = FALSE,
                          remove.ineq = FALSE,
                          remove.def = FALSE,
                          )
        std[i, "est.std"]
      }
    return(tmpfct)
  }
geteststd <- gen_fct(fit = fit_med, i = 1)

modc0 <- 
"
m ~ a*x
y ~ b*m
ab := a*b
astd := geteststd()
"

test_limit <- out2l
modc <- paste(modc0, "\nastd == ", test_limit, "\n0 < 1")
fit_medc <- lavaan::sem(modc, dat, do.fit = FALSE, fixed.x = FALSE)
ptable <- parameterTable(fit_medc)
ptable[ptable$free > 0, "est"] <-  attr(test_limit, "diag")$history$solution
ptable
# fit_medc <- update(fit_medc, start = ptable, do.fit = TRUE, optim.force.converged = TRUE, control = list(iterations = 2, control.outer = list(itmax = 2)))
fit_medc <- update(fit_medc, start = ptable, do.fit = TRUE, 
                   verbose = FALSE, optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02)))
parameterTable(fit_medc)
parameterTable(fit_med)
anova(fit_medc, fit_med)

test_that("Check p-value for the chi-square difference test", {
    expect_equal(
        anova(fit_medc, fit_med)[2, "Pr(>Chisq)"], 
        .05,
        tolerance = 1e-4,
        ignore_attr = TRUE
      )
  })



test_limit <- out2u
modc <- paste(modc0, "\nastd == ", test_limit, "\n0 < 1")
fit_medc <- lavaan::sem(modc, dat, do.fit = FALSE, fixed.x = FALSE)
ptable <- parameterTable(fit_medc)
ptable[ptable$free > 0, "est"] <-  attr(test_limit, "diag")$history$solution
ptable
# fit_medc <- update(fit_medc, start = ptable, do.fit = TRUE, optim.force.converged = TRUE, control = list(iterations = 2, control.outer = list(itmax = 2)))
fit_medc <- update(fit_medc, start = ptable, do.fit = TRUE, 
                   verbose = FALSE, optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02)))
parameterTable(fit_medc)
parameterTable(fit_med)
anova(fit_medc, fit_med)

test_that("Check p-value for the chi-square difference test", {
    expect_equal(
        anova(fit_medc, fit_med)[2, "Pr(>Chisq)"], 
        .05,
        tolerance = 1e-4,
        ignore_attr = TRUE
      )
  })



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
#     mxAlgebra((a^2) * var_x + evar_m, name = "var_m"),
#     mxAlgebra((b^2) * var_m + evar_y, name = "var_y"),
#     mxAlgebra(a * sqrt(var_x) / sqrt(var_m), name = "a_std"),
#     mxAlgebra(b * sqrt(var_m) / sqrt(var_y) , name = "b_std"),
#     mxAlgebra(a * b * sqrt(var_x / ((b^2)*evar_m + evar_y + var_x*(a * b + 0)^2)),
#                             name = "ab_std"),
#     mxCI(reference = c("a", "b", "ab", "a_std", "b_std", "ab_std"), 
#                        interval = .95, type = "both"),
#     mxData(observed = cov_dat, type = "cov", numObs = n)
#   )

# # To replicate OpenMx 2.18.1 with SLSQP
# mod_mx <- mxOption(mod_mx, "Feasibility tolerance", "1e-6")

# fit_med_OpenMx <- mxRun(mod_mx, silent = TRUE, intervals = TRUE)
# ci_OpenMx <- summary(fit_med_OpenMx)$CI

# #ci_semlbci <- c(out1l, out2l, out1ul, out1u, out2u, out1uu)
# ci_semlbci <- c(out2l, out2u)

# test_that("Equal to OpenMx LBCI", {
#     expect_equal(
#         ci_semlbci, 
#         unlist(ci_OpenMx[c(4), c("lbound", "ubound")]),
#         tolerance = 1e-6,
#         ignore_attr = TRUE
#       )
#   })
