library(testthat)
library(semlbci)

context("Check find_variance_in_free")

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
    expect_equivalent(
        find_variance_in_free(fit), id_free %in% id_free_var
      )
  })

test_that("Stop when the object is invalid", {
    expect_error(
        find_variance_in_free(mod),
        class = "simpleError"
      )
  })
