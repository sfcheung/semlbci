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
fit <- cfa(HS.model, data = HolzingerSwineford1939)

# Get one parameter

ind <- function(object) {
    # Need to use lav_model_get_parameters()
    # because coef() may not work.
    est <- lavaan::lav_model_get_parameters(object@Model, type = "user")
    unname(est[10] * est[11])
  }

test_that("Indirect effect", {
  userp <- gen_userp(func = ind, sem_out = fit)
  expect_equal(userp(coef(fit)),
               coef(fit, type = "user")[24],
               ignore_attr = TRUE)
  tmp <- coef(fit)
  tmp[7] <- .30
  tmp[8] <- .40
  expect_equal(userp(tmp),
               .30 * .40)
})

# gen_sem_out_userp

skip("To be run in an interactive session")

test_that("gen_sem_out_userp", {
    userp1234 <- gen_userp(func = ind, sem_out = fit)
    fit_userp <- gen_sem_out_userp(userp = userp1234,
                                   userp_name = "userp1234",
                                   sem_out = fit)
    fit_test <- fit_userp(.12)
    expect_equal(coef(fit_test, type = "user")["user"],
                 .12,
                 ignore_attr = TRUE,
                 tolerance = 1e-4)
})
