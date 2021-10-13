library(testthat)
library(semlbci)

# Two Groups

library(lavaan)

data(cfa_two_factors_mg)
dat <- cfa_two_factors_mg
mod <- 
"
f1 =~ x1 + c(a1, a2)*x2 + c(b1, b2)*x3
f2 =~ x4 + c(c1, c1)*x5 + c(d1, d2)*x6
f1 ~ f2
ad := a1 * c1
b1 == b2
"
fit <- lavaan::sem(mod, cfa_two_factors_mg, group = "gp")

lav_mod <- lavInspect(fit, "partable")

ram <- lav_mod_to_ram(lav_mod[[2]])

test_A <- structure(
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 24, 25, 26, 0, 0, 0, 0, 0, 0, 0,
    0, 27, 28, 29, 30, 0),
  .Dim = c(8L, 8L),
  .Dimnames = list(
    c("x1",
      "x2", "x3", "x4", "x5", "x6", "f1", "f2"), c("x1", "x2", "x3",
      "x4", "x5", "x6", "f1", "f2")))

test_S <- structure(
    c(31, 0, 0, 0, 0, 0, 0, 0, 0, 32, 0, 0, 0, 0, 0, 0,
      0, 0, 33, 0, 0, 0, 0, 0, 0, 0, 0, 34, 0, 0, 0, 0, 0, 0, 0, 0,
      35, 0, 0, 0, 0, 0, 0, 0, 0, 36, 0, 0, 0, 0, 0, 0, 0, 0, 37, 0,
      0, 0, 0, 0, 0, 0, 0, 38),
    .Dim = c(8L, 8L),
    .Dimnames = list(
      c("x1", "x2", "x3", "x4", "x5", "x6", "f1", "f2"), c("x1",
        "x2", "x3", "x4", "x5", "x6", "f1", "f2")))

test_F <- structure(
    c(1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0,
      0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    .Dim = c(6L, 8L),
    .Dimnames = list(
      c("x1", "x2", "x3", "x4", "x5", "x6"), c("x1", "x2", "x3",
      "x4", "x5", "x6", "f1", "f2")))

test_M <- structure(
    c(39, 40, 41, 42, 43, 44, 45, 46),
    .Dim = c(1L, 8L),
    .Dimnames = list(
       NULL, c("x1", "x2", "x3", "x4", "x5", "x6", "f1", "f2")))

test_that("Check the generated matrices", {
    expect_equal(ram$A, test_A)
    expect_equal(ram$S, test_S)
    expect_equal(ram$F, test_F)
    expect_equal(ram$M, test_M)
  })

# One Group

data(cfa_two_factors)
dat <- cfa_two_factors
mod <-
"
f1 =~ x1 + a*x2 + b*x3
f2 =~ x4 + d*x5 + e*x6
f1 ~ f*f2
ad := a * d
b == e
"
fit <- lavaan::sem(mod, cfa_two_factors)

lav_mod <- lavInspect(fit, "partable", drop.list.single.group = FALSE)

ram <- lav_mod_to_ram(lav_mod[[1]])

test_A <- structure(
    c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 0, 0, 0, 0, 0, 0, 0, 0,
      4, 5, 6, 7, 0),
    .Dim = c(8L, 8L),
    .Dimnames = list(
      c("x1", "x2",
        "x3", "x4", "x5", "x6", "f1", "f2"), c("x1", "x2", "x3", "x4",
        "x5", "x6", "f1", "f2")))

test_S <- structure(
    c(8, 0, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 
      0, 10, 0, 0, 0, 0, 0, 0, 0, 0, 11, 0, 0, 0, 0, 0, 0, 0, 0, 12,
      0, 0, 0, 0, 0, 0, 0, 0, 13, 0, 0, 0, 0, 0, 0, 0, 0, 14, 0, 0,
      0, 0, 0, 0, 0, 0, 15),
    .Dim = c(8L, 8L),
    .Dimnames = list(
      c("x1",
        "x2", "x3", "x4", "x5", "x6", "f1", "f2"), c("x1", "x2", "x3",
        "x4", "x5", "x6", "f1", "f2")))

test_F <- structure(
    c(1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 
      0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    .Dim = c(6L, 8L),
    .Dimnames = list(
      c("x1", "x2", "x3", "x4", "x5", "x6"), c("x1", "x2", "x3",
       "x4", "x5", "x6", "f1", "f2")))

test_M <- structure(
  c(0, 0, 0, 0, 0, 0, 0, 0),
  .Dim = c(1L, 8L),
  .Dimnames = list(
    NULL, c("x1", "x2", "x3", "x4", "x5", "x6", "f1", "f2")))

test_that("Check the generated matrices", {
    expect_equal(ram$A, test_A)
    expect_equal(ram$S, test_S)
    expect_equal(ram$F, test_F)
    expect_equal(ram$M, test_M)
  })
