library(testthat)
library(semlbci)

#context("Check sem_out")

dat <- cfa_two_factors

mod <-
"
f1 =~ x1 + x2 + a*x3
f2 =~ x4 + a*x5 + equal('f1=~x2')*x6
f1 ~~ 0*f2
asq := a^2
"

dat_gp <- dat
dat$gp <- rep(c("gp1", "gp2"), length.out = nrow(dat_gp))

fit01 <- lavaan::sem(mod, dat)
(out_01 <- check_sem_out(fit01))

fit02 <- lavaan::sem(mod, dat, estimator = "MLR")
(out_02 <- check_sem_out(fit02))

# No need to check SE method. SEs are not used.
# fit03 <- lavaan::sem(mod, dat, estimator = "ML", se = "robust")
# (out_03 <- check_sem_out(fit03))

# The warning can be ignored because this problem is intentional
suppressWarnings(fit04 <- lavaan::sem(mod, dat, estimator = "DWLS"))
(out_04 <- check_sem_out(fit04))

suppressWarnings(fit05 <- lavaan::sem(mod, dat, group = "gp"))
(out_05 <- check_sem_out(fit05))

fit06<- lavaan::sem(mod, dat, estimator = "MLR")
(out_06 <- check_sem_out(fit06, robust = "satorra.2000"))

fit07<- lavaan::sem(mod, dat, estimator = "ML")
(out_07 <- check_sem_out(fit07, robust = "satorra.2000"))

fit08<- lavaan::sem(mod, dat, estimator = "ML", likelihood = "wishart")
(out_08 <- check_sem_out(fit08))


test_that("Check against the flags", {
    expect_true(
        out_01 == 0
      )
  })

test_that("Check against the flags", {
    expect_true(
        out_02 == -1
      )
  })

# test_that("Check against the flags", {
#     expect_true(
#         out_03 == -1
#       )
#   })

test_that("Check against the flags", {
    expect_true(
        out_04 == -2
      )
  })

# test_that("Check against the flags", {
#     expect_true(
#         out_05 == -1
#       )
#   })

test_that("Check against the flags", {
    expect_true(
        out_06 == 0
      )
  })

test_that("Check against the flags", {
    expect_true(
        out_07 == -1
      )
  })

test_that("Check against the flags", {
    expect_error(
        semlbci(fit02)
      )
  })

test_that("Check against the flags", {
    expect_error(
        semlbci(fit03)
      )
  })

test_that("Check against the flags", {
    expect_error(
        semlbci(fit04)
      )
  })

# test_that("Check against the flags", {
#     expect_error(
#         semlbci(fit05)
#       )
#   })

test_that("Check against the flags", {
    expect_error(
        semlbci(fit06)
      )
  })

test_that("Check against the flags", {
    expect_error(
        semlbci(fit07, robust = "satorra.2000")
      )
  })

test_that("Check against the flags", {
    expect_error(
        semlbci(fit08)
      )
  })
