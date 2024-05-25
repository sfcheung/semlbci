library(testthat)
library(lavaan)

# cfa() example
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9
              textual ~ a*visual
              speed ~ b*textual
              ab := a*b'
fit <- cfa(HS.model, data = HolzingerSwineford1939)

# Get one parameter

test_that("add_func", {
  ind <- function(object) {
      # Need to use lav_model_get_parameters()
      # because coef() may not work.
      est <- lavaan::lav_model_get_parameters(object@Model, type = "user")
      unname(est[10] * est[11])
    }
  fit_i_free <- add_func(func = ind,
                         sem_out = fit,
                         fix = FALSE)
  fit_i <- add_func(func = ind,
                    sem_out = fit)
  # Need to see whether fit_i() can run without ind
  rm(ind)

  fit_i_tmp <- sem_out_userp_run(target = .400,
                                 object = fit_i)
  expect_equal(lavTestLRT(fit_i_tmp, fit)[2, "Df diff"],
               1)
  expect_equal(unname(coef(fit_i_tmp, type = "user")["user"]),
               .400,
               tolerance = 1e-4)
})

skip("Long test")

# Not ready

ind <- function(object) {
    # Need to use lav_model_get_parameters()
    # because coef() may not work.
    est <- lavaan::lav_model_get_parameters(object@Model, type = "user")
    unname(est[10] * est[11])
  }

fit_i_free <- add_func(func = ind,
                        sem_out = fit,
                        fix = FALSE)
fit_i <- add_func(func = ind,
                  sem_out = fit)

tmp <- loglik_user(.250,
                   sem_out_userp = fit_i,
                   sem_out = fit)
fit_tmp <- sem_out_userp_run(target = .400,
                             object = fit_i_free)
est_i <- parameterEstimates(fit_tmp)[25, ]
x_range <- unlist(est_i[, c("ci.lower", "ci.upper")])
d <- (x_range[2] - x_range[1]) / 10
x_seq <- seq(x_range[1] - d, x_range[2] + d, length.out = 15)
x_seq
x_range

# Plot loglikelihood

library(pbapply)
library(parallel)
my_cl <- makeCluster(length(x_seq))
clusterExport(cl = my_cl, c("sem_out_userp_run"))
ll_out <- pbsapply(x_seq,
                   FUN = loglik_user,
                   sem_out_userp = fit_i,
                   sem_out = fit,
                   global_ok = TRUE,
                   cl = my_cl)
stopCluster(my_cl)

plot(x_seq, ll_out, type = "l", lwd = 4, col = "red")
abline(h = 3.84159 / -2, lwd = 4, col = "blue")

# Plot p-value

ll2p <- function(x) {
    stats::pchisq(abs(-2 * x), df = 1, lower.tail = FALSE)
  }

p_out <- sapply(ll_out, ll2p)

plot(x_seq, p_out, type = "l", lwd = 4, col = "red")
abline(h = 0, lwd = 4, col = "blue")

# Zoom in

x_seq <- seq(x_range[1] - d, x_range[1] + d, length.out = 15)

library(pbapply)
library(parallel)
my_cl <- makeCluster(length(x_seq))
clusterExport(cl = my_cl, c("sem_out_userp_run"))
ll_out <- pbsapply(x_seq,
                   FUN = loglik_user,
                   sem_out_userp = fit_i,
                   sem_out = fit,
                   global_ok = TRUE,
                   cl = my_cl)
stopCluster(my_cl)

plot(x_seq, ll_out, type = "l", lwd = 4, col = "red")
abline(h = 3.84159 / -2, lwd = 4, col = "blue")

p_out <- sapply(ll_out, ll2p)

plot(x_seq, p_out, type = "l", lwd = 4, col = "red")
abline(h = 0.05, lwd = 4, col = "blue")

fit_i_l <- sem_out_userp_run(target = x_seq[1],
                             object = fit_i)
lavTestLRT(fit_i_l, fit)
fit_i_u <- sem_out_userp_run(target = x_seq[length(x_seq)],
                             object = fit_i)
lavTestLRT(fit_i_u, fit)

# Bound

ind <- function(object) {
    # Need to use lav_model_get_parameters()
    # because coef() may not work.
    est <- lavaan::lav_model_get_parameters(object@Model, type = "user")
    unname(est[10] * est[11])
  }

system.time(
ci_lb <- ci_bound_ur(sem_out = fit,
                       func = ind,
                       which = "lbound",
                       progress = TRUE)
)
system.time(
ci_ub <- ci_bound_ur(sem_out = fit,
                       func = ind,
                       which = "ubound",
                       progress = TRUE,
                       d = 5)
)

ci_lb$lrt
ci_ub$lrt


system.time(
ci_lb <- ci_bound_ur(sem_out = fit,
                      func = ind,
                      which = "lbound",
                      root_target = "pvalue",
                      progress = TRUE)
)
system.time(
ci_ub <- ci_bound_ur(sem_out = fit,
                      func = ind,
                      which = "ubound",
                      root_target = "pvalue",
                      progress = TRUE)
)

ci_lb$lrt
ci_ub$lrt


# Compare with WN

library(semlbci)

ciperc <- .95

fn_constr0 <- set_constraint(fit, ciperc = ciperc)

opts0 <- list()
opts0 <- list(ftol_abs = 1e-7,
              ftol_rel = 1e-7,
              xtol_abs = 1e-7,
              xtol_rel = 1e-7
              # tol_constraints_eq = 1e-10
              )
time1l <- system.time(out1l <- ci_bound_wn_i(i = 24, npar = 20, sem_out = fit, which = "lbound", opts = opts0, f_constr = fn_constr0, verbose = TRUE, ciperc = ciperc))
time1u <- system.time(out1u <- ci_bound_wn_i(i = 24, npar = 20, sem_out = fit, which = "ubound", opts = opts0, f_constr = fn_constr0, verbose = TRUE, ciperc = ciperc))

time1l
time1u

round(out1l$bound, 3)
round(ci_lb$bound, 3)

round(out1u$bound, 3)
round(ci_ub$bound, 3)
