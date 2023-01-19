library(testthat)
library(semlbci)

# context("Check find_variance_in_free")

dat <- cfa_two_factors

mod <-
"
f1 =~ x1 + x2 + a*x3
f2 =~ x4 + a*x5 + equal('f1=~x2')*x6
f1 ~~ 0*f2
asq := a^2
"

fit <- lavaan::sem(mod, dat)
ptable <- lavaan::parameterTable(fit)
id_free <- ptable[ptable$free > 0, "id"]
p_var <- (ptable$lhs == ptable$rhs) & ptable$op == "~~"
id_free_var <- ptable[(ptable$free > 0) & (p_var), "id"]

test_that("Correct variance parameters", {
    expect_equal(
        find_variance_in_free(fit), id_free %in% id_free_var,
        ignore_attr = TRUE
      )
  })

test_that("Stop when the object is invalid", {
    expect_error(
        find_variance_in_free(mod),
        class = "simpleError"
      )
  })

# fixed.x = TRUE

dat <- simple_med

mod <-
"
m ~ a * x
y ~ b * m
ab := a * b
"

fit <- lavaan::sem(mod, dat, fixed.x = TRUE)
ptable <- lavaan::parameterTable(fit)
id_free <- ptable[ptable$free > 0, "id"]
p_var <- (ptable$lhs == ptable$rhs) & ptable$op == "~~"
id_free_var <- ptable[(ptable$free > 0) & (p_var), "id"]
free_var <- ptable[id_free_var, "est"]

test_that("Correct variance parameters when fixed.x = TRUE", {
    expect_equal(
        find_variance_in_free_lb(fit, prop = 1, se_k = 0),
        free_var,
        ignore_attr = TRUE
      )
  })
