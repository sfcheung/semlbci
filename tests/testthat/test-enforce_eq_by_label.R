library(testthat)
library(semlbci)

# context("Check semlbci: With equality constraints")

dat <- cfa_two_factors
mod <- 
"
f1 =~ x1 + b*x2 + c*x3
f2 =~ x4 + c*x5 + x6
"
fit <- lavaan::sem(mod, cfa_two_factors, fixed.x = FALSE)
p_table <- lavaan::parameterTable(fit)
options(widht = 1802)
p_table

out1 <- semlbci:::enforce_eq_by_label(c(2, 3), p_table)
out2 <- semlbci:::enforce_eq_by_label(c(2, 5), p_table)
out3 <- semlbci:::enforce_eq_by_label(c(1, 2, 5), p_table)
out4 <- semlbci:::enforce_eq_by_label(c(1, 2), p_table)
out5 <- semlbci:::enforce_eq_by_label(c(1, 4, 6), p_table)


test_that("Test 1a", {
    expect_equal(
      out1$i_exp_target,
      c(2, 3, 5)  
    )
  })

test_that("Test 1b", {
    expect_equal(
      out1$i_exp_source,
      c(2, 3, 3)  
    )
  })


test_that("Test 2a", {
    expect_equal(
      out2$i_exp_target,
      c(2, 3, 5)  
    )
  })

test_that("Test 2b", {
    expect_equal(
      out2$i_exp_source,
      c(2, 5, 5)  
    )
  })



test_that("Test 3b", {
    expect_equal(
      out3$i_exp_target,
      c(1, 2, 3, 5)  
    )
  })

test_that("Test 3b", {
    expect_equal(
      out3$i_exp_source,
      c(1, 2, 5, 5)  
    )
  })



test_that("Test 4b", {
    expect_equal(
      out4$i_exp_target,
      c(1, 2)  
    )
  })

test_that("Test 4b", {
    expect_equal(
      out4$i_exp_source,
      c(1, 2)  
    )
  })


test_that("Test 5b", {
    expect_equal(
      out5$i_exp_target,
      c(1, 4, 6)  
    )
  })

test_that("Test 5b", {
    expect_equal(
      out5$i_exp_source,
      c(1, 4, 6)  
    )
  })
