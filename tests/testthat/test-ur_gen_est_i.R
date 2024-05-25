skip_on_cran()

# Ready

library(testthat)
library(lavaan)

# cfa() example
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9
              textual ~ a*visual
              speed ~ b*textual
              ab := a*b'
fit <- sem(HS.model, data = HolzingerSwineford1939)
fit_gp <- sem(HS.model, data = HolzingerSwineford1939,
              group = "school",
              group.equal = "intercepts")

# Unstandardized

test_that("Unstandardized", {
  est_i <- gen_est_i(i = 9, sem_out = fit)
  expect_equal(est_i(fit),
              unname(coef(fit, type = "user")["speed=~x9"]))
  est_i <- gen_est_i(i = 24, sem_out = fit)
  expect_equal(est_i(fit),
              unname(coef(fit, type = "user")["ab"]))
  est_i <- gen_est_i(i = 46, sem_out = fit_gp)
  expect_equal(est_i(fit_gp),
              unname(coef(fit_gp, type = "user")["b"]))
  est_i <- gen_est_i(i = 71, sem_out = fit_gp)
  expect_equal(est_i(fit_gp),
              unname(coef(fit_gp, type = "user")["ab"]))
})

# Standardized

test_that("Standardized", {
  std_all <- standardizedSolution(fit)
  std_all_gp <- standardizedSolution(fit_gp)

  est_i <- gen_est_i(i = 9, sem_out = fit, standardized = TRUE)
  expect_equal(est_i(fit),
              unname(std_all[9, "est.std"]))
  est_i <- gen_est_i(i = 24, sem_out = fit, standardized = TRUE)
  expect_equal(est_i(fit),
              unname(std_all[24, "est.std"]))
  est_i <- gen_est_i(i = 46, sem_out = fit_gp, standardized = TRUE)
  expect_equal(est_i(fit_gp),
              unname(std_all_gp[46, "est.std"]))
  est_i <- gen_est_i(i = 71, sem_out = fit_gp, standardized = TRUE)
  expect_equal(est_i(fit_gp),
              unname(std_all_gp[71, "est.std"]))
})

# gne_userp

test_that("Unstandardized", {

est_i <- gen_est_i(i = 9, sem_out = fit)
userp <- gen_userp(func = est_i, sem_out = fit)
expect_equal(userp(coef(fit)),
             coef(fit, type = "user")[9],
             ignore_attr = TRUE)
tmp <- coef(fit)
tmp[6] <- .30
expect_equal(userp(tmp),
             .30)

est_i <- gen_est_i(i = 24, sem_out = fit)
userp <- gen_userp(func = est_i, sem_out = fit)
expect_equal(userp(coef(fit)),
             coef(fit, type = "user")[24],
             ignore_attr = TRUE)
tmp <- coef(fit)
tmp[7] <- .30
tmp[8] <- .40
expect_equal(userp(tmp),
             .30 * .40)

est_i <- gen_est_i(i = 46, sem_out = fit_gp)
userp <- gen_userp(func = est_i, sem_out = fit_gp)
expect_equal(userp(coef(fit_gp)),
             coef(fit_gp, type = "user")[46],
             ignore_attr = TRUE)
tmp <- coef(fit_gp)
tmp[37] <- .30
expect_equal(userp(tmp),
             .30)

est_i <- gen_est_i(i = 71, sem_out = fit_gp)
userp <- gen_userp(func = est_i, sem_out = fit_gp)
expect_equal(userp(coef(fit_gp)),
             coef(fit_gp, type = "user")[71],
             ignore_attr = TRUE)
tmp <- coef(fit_gp)
tmp[7] <- .30
tmp[8] <- .40
tmp[37] <- .30
tmp[38] <- .40
expect_equal(userp(tmp),
             .30 * .40)
})


test_that("Standardized", {

  std_all <- standardizedSolution(fit)
  std_all_gp <- standardizedSolution(fit_gp)

  est_i <- gen_est_i(i = 9, sem_out = fit, standardized = TRUE)
  userp <- gen_userp(func = est_i, sem_out = fit)
  expect_equal(userp(coef(fit)),
              std_all[9, "est.std"],
              ignore_attr = TRUE)

  est_i <- gen_est_i(i = 24, sem_out = fit, standardized = TRUE)
  userp <- gen_userp(func = est_i, sem_out = fit)
  expect_equal(userp(coef(fit)),
              std_all[24, "est.std"],
              ignore_attr = TRUE)

  est_i <- gen_est_i(i = 46, sem_out = fit_gp, standardized = TRUE)
  userp <- gen_userp(func = est_i, sem_out = fit_gp)
  expect_equal(userp(coef(fit_gp)),
              std_all_gp[46, "est.std"],
              ignore_attr = TRUE)

  est_i <- gen_est_i(i = 71, sem_out = fit_gp, standardized = TRUE)
  userp <- gen_userp(func = est_i, sem_out = fit_gp)
  expect_equal(userp(coef(fit_gp)),
              std_all_gp[71, "est.std"],
              ignore_attr = TRUE)
})

# Not ready

skip("Long test")
skip("WIP")

HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9
              visual ~~ a*textual
              visual ~~ b*speed
              textual ~~ d*speed
              ab := a*b'
fit <- sem(HS.model, data = HolzingerSwineford1939)
fit_gp <- sem(HS.model, data = HolzingerSwineford1939,
              group = "school",
              group.equal = "intercepts")

# Unstandardized

est_i <- gen_est_i(i = 9, sem_out = fit)
system.time(
ci_lb <- ci_bound_ur(sem_out = fit,
                      func = est_i,
                      which = "lbound",
                      progress = TRUE)
)
system.time(
ci_ub <- ci_bound_ur(sem_out = fit,
                      func = est_i,
                      which = "ubound",
                      progress = TRUE)
)
ci_lb$lrt
ci_ub$lrt
c(ci_lb$bound, ci_ub$bound)
parameterEstimates(fit)[9, ]

est_i <- gen_est_i(i = 25, sem_out = fit)
system.time(
ci_lb <- ci_bound_ur(sem_out = fit,
                      func = est_i,
                      which = "lbound",
                      progress = TRUE)
)
system.time(
ci_ub <- ci_bound_ur(sem_out = fit,
                      func = est_i,
                      which = "ubound",
                      progress = TRUE)
)

ci_lbci <- semlbci(sem_out = fit, pars = "ab :=")

ci_lb$lrt
ci_ub$lrt
c(ci_lb$bound, ci_ub$bound)
confint(ci_lbci)
parameterEstimates(fit)[25, ]

est_i <- gen_est_i(i = 46, sem_out = fit_gp)
system.time(
ci_lb <- ci_bound_ur(sem_out = fit_gp,
                      func = est_i,
                      which = "lbound",
                      progress = TRUE)
)
system.time(
ci_ub <- ci_bound_ur(sem_out = fit_gp,
                      func = est_i,
                      which = "ubound",
                      progress = TRUE)
)
ci_lbci <- semlbci(sem_out = fit, pars = "visual ~~ textual")

ci_lb$lrt
ci_ub$lrt
c(ci_lb$bound, ci_ub$bound)
confint(ci_lbci)
parameterEstimates(fit_gp)[46, ]

est_i <- gen_est_i(i = 73, sem_out = fit_gp)
system.time(
ci_lb <- ci_bound_ur(sem_out = fit_gp,
                      func = est_i,
                      which = "lbound",
                      progress = TRUE)
)
system.time(
ci_ub <- ci_bound_ur(sem_out = fit_gp,
                      func = est_i,
                      which = "ubound",
                      progress = TRUE)
)
ci_lbci <- semlbci(sem_out = fit_gp, pars = "ab :=")

ci_lb$lrt
ci_ub$lrt
c(ci_lb$bound, ci_ub$bound)
confint(ci_lbci)
parameterEstimates(fit_gp)[73, ]

# Standardized

est_i <- gen_est_i(i = 9, sem_out = fit, standardized = TRUE)
system.time(
ci_lb <- ci_bound_ur(sem_out = fit,
                      func = est_i,
                      which = "lbound",
                      progress = TRUE)
)
system.time(
ci_ub <- ci_bound_ur(sem_out = fit,
                      func = est_i,
                      which = "ubound",
                      progress = TRUE)
)

ci_lbci <- semlbci(sem_out = fit, pars = "speed =~ x9", standardized = TRUE)

ci_lb$lrt
ci_ub$lrt
c(ci_lb$bound, ci_ub$bound)
confint(ci_lbci)
standardizedSolution(fit)[9, ]

est_i <- gen_est_i(i = 25, sem_out = fit, standardized = TRUE)
system.time(
ci_lb <- ci_bound_ur(sem_out = fit,
                      func = est_i,
                      which = "lbound",
                      progress = TRUE)
)
system.time(
ci_ub <- ci_bound_ur(sem_out = fit,
                      func = est_i,
                      which = "ubound",
                      progress = TRUE)
)

ci_lbci <- semlbci(sem_out = fit, pars = "ab :=", standardized = TRUE)

ci_lb$lrt
ci_ub$lrt
c(ci_lb$bound, ci_ub$bound)
confint(ci_lbci)
standardizedSolution(fit)[25, ]

est_i <- gen_est_i(i = 46, sem_out = fit_gp, standardized = TRUE)
system.time(
ci_lb <- ci_bound_ur(sem_out = fit_gp,
                      func = est_i,
                      which = "lbound",
                      progress = TRUE)
)
system.time(
ci_ub <- ci_bound_ur(sem_out = fit_gp,
                      func = est_i,
                      which = "ubound",
                      progress = TRUE)
)

ci_lbci <- semlbci(sem_out = fit_gp, pars = "visual ~~ 2*textual", standardized = TRUE)

ci_lb$lrt
ci_ub$lrt
c(ci_lb$bound, ci_ub$bound)
confint(ci_lbci)
standardizedSolution(fit_gp)[46, ]


# Too difficult to search by both methods

# est_i <- gen_est_i(i = 73, sem_out = fit_gp, standardized = TRUE)
# system.time(
# ci_lb <- ci_bound_ur(sem_out = fit_gp,
#                       func = est_i,
#                       which = "lbound",
#                       progress = TRUE)
# )
# system.time(
# ci_ub <- ci_bound_ur(sem_out = fit_gp,
#                       func = est_i,
#                       which = "ubound",
#                       progress = TRUE)
# )

# ci_lbci <- semlbci(sem_out = fit_gp, pars = "ab :=", standardized = TRUE)

# ci_lb$lrt
# ci_ub$lrt
# c(ci_lb$bound, ci_ub$bound)
# confint(ci_lbci)
# standardizedSolution(fit_gp)[46, ]
