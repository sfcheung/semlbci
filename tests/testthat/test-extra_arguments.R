skip("Run it only if are major changes in the arguments in ci_i_* functions")

library(testthat)
library(semlbci)

# context("Check extra arguments")

data(simple_med)
dat <- simple_med
mod <- 
"
m ~ x
y ~ m
"
fit_med <- lavaan::sem(mod, simple_med, fixed.x = FALSE)

lbci_debug_TRUE <- semlbci(fit_med, pars = c(1), debug = TRUE, method = "wn")
lbci_debug_FALSE <- semlbci(fit_med, pars = c(1), debug = FALSE, method = "wn")
lbci_lav_warn_TRUE <- semlbci(fit_med, pars = c(1), lav_warn = TRUE, method = "wn")
lbci_lav_warn_FALSE <- semlbci(fit_med, pars = c(1), lav_warn = FALSE, method = "wn")
lbci_history_TRUE <- semlbci(fit_med, pars = c(1), history = TRUE, method = "wn")
lbci_history_FALSE <- semlbci(fit_med, pars = c(1), history = FALSE, method = "wn")

test_that("Can run with debug = TRUE", {
    expect_equal(
        as.numeric(unlist(lbci_debug_TRUE[c(1), c("lbci_lb", "lbci_ub")])),
        c(0.8277014, 2.5245539),
        tolerance = 1e-5,
        ignore_attr = TRUE
      )
  })

test_that("Can run with debug = FALSE", {
    expect_equal(
        as.numeric(unlist(lbci_debug_FALSE[c(1), c("lbci_lb", "lbci_ub")])),
        c(0.8277014, 2.5245539),
        tolerance = 1e-5,
        ignore_attr = TRUE
      )
  })

test_that("Can run with lav_warn = TRUE", {
    expect_equal(
        as.numeric(unlist(lbci_lav_warn_TRUE[c(1), c("lbci_lb", "lbci_ub")])),
        c(0.8277014, 2.5245539),
        tolerance = 1e-5,
        ignore_attr = TRUE
      )
  })

test_that("Can run with lav_warn = FALSE", {
    expect_equal(
        as.numeric(unlist(lbci_lav_warn_FALSE[c(1), c("lbci_lb", "lbci_ub")])),
        c(0.8277014, 2.5245539),
        tolerance = 1e-5,
        ignore_attr = TRUE
      )
  })

test_that("Can run with history = TRUE", {
    expect_equal(
        as.numeric(unlist(lbci_history_TRUE[c(1), c("lbci_lb", "lbci_ub")])),
        c(0.8277014, 2.5245539),
        tolerance = 1e-5,
        ignore_attr = TRUE
      )
  })

test_that("Can run with history = FALSE", {
    expect_equal(
        as.numeric(unlist(lbci_history_FALSE[c(1), c("lbci_lb", "lbci_ub")])),
        c(0.8277014, 2.5245539),
        tolerance = 1e-5,
        ignore_attr = TRUE
      )
  })

