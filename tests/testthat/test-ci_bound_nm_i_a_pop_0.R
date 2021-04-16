library(testthat)
library(semlbci)

# Check ci_bound_nm_i: a_pop == 0"

# Disabled.

# set.seed(51314535)
# n <- 100
# x <- rnorm(n)
# m <- rnorm(n)
# y <- rnorm(n)
# dat <- data.frame(x, m, y)
# mod <-
# "
# m ~ a*x
# y ~ b*m + cp*x
# ab := a*b
# "
# fit_med <- lavaan::sem(mod, dat, fixed.x = FALSE)

# # opts0 <- list(print_level = 3)
# opts0 <- list()
# opts0 <- list(ftol_abs = 1e-7,
#               ftol_rel = 1e-7,
#               xtol_abs = 1e-7,
#               xtol_rel = 1e-7,
#               tol_constraints_eq = 1e-7
#               )
# system.time(out1l <- ci_bound_nm_i(1, 6, sem_out = fit_med, which = "lbound", opts = opts0, verbose = TRUE))
# system.time(out1u <- ci_bound_nm_i(1, 6, sem_out = fit_med, which = "ubound", opts = opts0, verbose = TRUE))
# system.time(out2l <- ci_bound_nm_i(2, 6, sem_out = fit_med, which = "lbound", opts = opts0, verbose = TRUE))
# system.time(out2u <- ci_bound_nm_i(2, 6, sem_out = fit_med, which = "ubound", opts = opts0, verbose = TRUE))
# system.time(out3l <- ci_bound_nm_i(3, 6, sem_out = fit_med, which = "lbound", opts = opts0, verbose = TRUE))
# system.time(out3u <- ci_bound_nm_i(3, 6, sem_out = fit_med, which = "ubound", opts = opts0, verbose = TRUE))
# system.time(out4l <- ci_bound_nm_i(4, 6, sem_out = fit_med, which = "lbound", opts = opts0, verbose = TRUE))
# system.time(out4u <- ci_bound_nm_i(4, 6, sem_out = fit_med, which = "ubound", opts = opts0, verbose = TRUE))
# system.time(out5l <- ci_bound_nm_i(5, 6, sem_out = fit_med, which = "lbound", opts = opts0, verbose = TRUE))
# system.time(out5u <- ci_bound_nm_i(5, 6, sem_out = fit_med, which = "ubound", opts = opts0, verbose = TRUE))
# system.time(out6l <- ci_bound_nm_i(6, 6, sem_out = fit_med, which = "lbound", opts = opts0, verbose = TRUE))
# system.time(out6u <- ci_bound_nm_i(6, 6, sem_out = fit_med, which = "ubound", opts = opts0, verbose = TRUE))
# system.time(out7l <- ci_bound_nm_i(7, 6, sem_out = fit_med, which = "lbound", opts = opts0, verbose = TRUE))
# system.time(out7u <- ci_bound_nm_i(7, 6, sem_out = fit_med, which = "ubound", opts = opts0, verbose = TRUE))

# diag1l <- attr(out1l, "diag")
# diag1u <- attr(out1u, "diag")
# diag2l <- attr(out2l, "diag")
# diag2u <- attr(out2u, "diag")
# diag3l <- attr(out3l, "diag")
# diag3u <- attr(out3u, "diag")
# diag4l <- attr(out4l, "diag")
# diag4u <- attr(out4u, "diag")
# diag5l <- attr(out5l, "diag")
# diag5u <- attr(out5u, "diag")
# diag6l <- attr(out6l, "diag")
# diag6u <- attr(out6u, "diag")
# diag7l <- attr(out7l, "diag")
# diag7u <- attr(out7u, "diag")

# library(lavaan)

# parameterTable(diag1l$fit_final)
# parameterTable(diag1u$fit_final)
# parameterTable(diag2l$fit_final)
# parameterTable(diag2u$fit_final)
# parameterTable(diag3l$fit_final)
# parameterTable(diag3u$fit_final)
# parameterTable(diag4l$fit_final)
# parameterTable(diag4u$fit_final)
# parameterTable(diag5l$fit_final)
# parameterTable(diag5u$fit_final)
# parameterTable(diag6l$fit_final)
# parameterTable(diag6u$fit_final)
# parameterTable(diag7l$fit_final)
# parameterTable(diag7u$fit_final)

