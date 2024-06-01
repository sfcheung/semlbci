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
