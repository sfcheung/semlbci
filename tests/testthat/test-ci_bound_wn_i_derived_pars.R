library(testthat)
library(semlbci)

# context("Check ci_bound_wn_i: Derived parameters")

library(lavaan)
data(simple_med)
dat <- simple_med
mod <- 
"
m ~ a*x
y ~ b*m
ab:= a*b
asq:= a^2
"
fit_med <- lavaan::sem(mod, simple_med, fixed.x = FALSE)
lavaan::parameterTable(fit_med)

fn_constr0 <- set_constraint(fit_med)

system.time(out1l <- ci_bound_wn_i(6, 5, sem_out = fit_med, f_constr = fn_constr0, which = "lbound", method = "wn", verbose = TRUE))
system.time(out1u <- ci_bound_wn_i(6, 5, sem_out = fit_med, f_constr = fn_constr0, which = "ubound", method = "wn", verbose = TRUE))
system.time(out2l <- ci_bound_wn_i(7, 5, sem_out = fit_med, f_constr = fn_constr0, which = "lbound", method = "wn", verbose = TRUE))
system.time(out2u <- ci_bound_wn_i(7, 5, sem_out = fit_med, f_constr = fn_constr0, which = "ubound", method = "wn", verbose = TRUE))


modc0 <- 
"
m ~ a*x
y ~ b*m
ab:= a*b
asq:= a^2
"

test_limit <- out1l
modc <- paste(modc0, "\nab == ", test_limit)
fit_medc <- lavaan::sem(modc, dat, do.fit = FALSE, fixed.x = FALSE)
ptable <- parameterTable(fit_medc)
ptable[ptable$free > 0, "start"] <-  attr(test_limit, "diag")$history$solution
ptable
fit_medc <- update(fit_medc, start = ptable, do.fit = TRUE)
anova(fit_medc, fit_med)

test_that("Check p-value for the chi-square difference test", {
    expect_equal(
        anova(fit_medc, fit_med)[2, "Pr(>Chisq)"], 
        .05,
        tolerance = 1e-6,
        ignore_attr = TRUE
      )
  })


test_limit <- out1u
modc <- paste(modc0, "\nab == ", test_limit)
fit_medc <- lavaan::sem(modc, dat, do.fit = FALSE, fixed.x = FALSE)
ptable <- parameterTable(fit_medc)
ptable[ptable$free > 0, "start"] <-  attr(test_limit, "diag")$history$solution
ptable
fit_medc <- update(fit_medc, start = ptable, do.fit = TRUE)
anova(fit_medc, fit_med)

test_that("Check p-value for the chi-square difference test", {
    expect_equal(
        anova(fit_medc, fit_med)[2, "Pr(>Chisq)"], 
        .05,
        tolerance = 1e-5,
        ignore_attr = TRUE
      )
  })


test_limit <- out2l
modc <- paste(modc0, "\nasq == ", test_limit)
fit_medc <- lavaan::sem(modc, dat, do.fit = FALSE, fixed.x = FALSE)
ptable <- parameterTable(fit_medc)
ptable[ptable$free > 0, "start"] <-  attr(test_limit, "diag")$history$solution
ptable
fit_medc <- update(fit_medc, start = ptable, do.fit = TRUE)
anova(fit_medc, fit_med)

test_that("Check p-value for the chi-square difference test", {
    expect_equal(
        anova(fit_medc, fit_med)[2, "Pr(>Chisq)"], 
        .05,
        tolerance = 1e-5,
        ignore_attr = TRUE
      )
  })


test_limit <- out2u
modc <- paste(modc0, "\nasq == ", test_limit)
fit_medc <- lavaan::sem(modc, dat, do.fit = FALSE, fixed.x = FALSE)
ptable <- parameterTable(fit_medc)
ptable[ptable$free > 0, "start"] <-  attr(test_limit, "diag")$history$solution
ptable
fit_medc <- update(fit_medc, start = ptable, do.fit = TRUE)
anova(fit_medc, fit_med)

test_that("Check p-value for the chi-square difference test", {
    expect_equal(
        anova(fit_medc, fit_med)[2, "Pr(>Chisq)"], 
        .05,
        tolerance = 1e-5,
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
#     mxAlgebra(a ^ 2 , name = "asq"),
#     mxCI(reference = c("a", "b", "ab", "asq"), 
#                        interval = .95, type = "both"),
#     mxData(observed = cov_dat, type = "cov", numObs = n)
#   )

# # To replicate OpenMx 2.18.1 with SLSQP
# mod_mx <- mxOption(mod_mx, "Feasibility tolerance", "1e-6")

# fit_med_OpenMx <- mxRun(mod_mx, silent = TRUE, intervals = TRUE)
# ci_OpenMx <- summary(fit_med_OpenMx)$CI

# ci_semlbci <- c(out1l, out2l, out1u, out2u)

# test_that("Equal to OpenMx LBCI", {
#     expect_equal(
#         ci_semlbci, 
#         unlist(ci_OpenMx[c(3, 4), c("lbound", "ubound")]),
#         tolerance = 1e-6,
#         ignore_attr = TRUE
#       )
#   })
