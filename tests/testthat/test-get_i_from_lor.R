library(testthat)
library(semlbci)

library(lavaan)

dat <- HolzingerSwineford1939
mod <- "
  visual  =~ x1 + x2 + a*x3
  textual =~ x4 + x5 + d*x6
  speed   =~ x7 + b*x8 + c*x9
  ab := a*b
  cd := c*d
  cd == 2
"

fit_lavaan <- cfa(mod, dat, do.fit = FALSE)
ptable <- parameterTable(fit_lavaan)

i <- get_i_from_lor(ptable, lor = data.frame(lhs = "ab",
                                    op  = ":=",
                                    rhs = "a*b",
                                    block = 0,
                                    group = 0))
i_test <- which((ptable$lhs == "ab") &
                (ptable$op  == ":=") &
                (ptable$rhs == "a*b") &
                (ptable$block == 0) &
                (ptable$group == 0))
test_that("Check output in one-group CFA", {
    expect_equal(
        i,
        i_test
      )
  })

i <- get_i_from_lor(ptable, lhs = "ab",
                            op  = ":=",
                            rhs = "a*b",
                            block = 0,
                            group = 0)
i_test <- which((ptable$lhs == "ab") &
                (ptable$op  == ":=") &
                (ptable$rhs == "a*b") &
                (ptable$block == 0) &
                (ptable$group == 0))
test_that("Check output in one-group CFA", {
    expect_equal(
        i,
        i_test
      )
  })

mod2 <- "
  visual  =~ x1 + c(a1, a2)*x2 + c(b, b)*x3
  textual =~ x4 + c(c, c)*x5 + c(d1, d2)*x6
  cd1 := c*d1
  cd2 := c*d2
  cd1 == .900 
"
fit_lavaan_gp <- cfa(mod2, dat, group = "school", baseline = FALSE, 
                     h1 = FALSE, do.fit = FALSE)
ptable_gp <- parameterTable(fit_lavaan_gp)

i <- get_i_from_lor(ptable_gp, lor = data.frame(lhs = "cd1",
                                    op  = ":=",
                                    rhs = "c*d1",
                                    block = 0,
                                    group = 0))
i_test <- which((ptable_gp$lhs == "cd1") &
                (ptable_gp$op  == ":=") &
                (ptable_gp$rhs == "c*d1") &
                (ptable_gp$block == 0) &
                (ptable_gp$group == 0))
test_that("Check output in two-group CFA", {
    expect_equal(
        i,
        i_test
      )
  })

i <- get_i_from_lor(ptable_gp, lhs = "cd2",
                            op  = ":=",
                            rhs = "c*d2",
                            block = 0,
                            group = 0)
i_test <- which((ptable_gp$lhs == "cd2") &
                (ptable_gp$op  == ":=") &
                (ptable_gp$rhs == "c*d2") &
                (ptable_gp$block == 0) &
                (ptable_gp$group == 0))
test_that("Check output in two-group CFA", {
    expect_equal(
        i,
        i_test
      )
  })

i <- get_i_from_lor(ptable_gp, lor = data.frame(lhs = "textual",
                                    op  = "=~",
                                    rhs = "x4",
                                    block = 2,
                                    group = 2))
i_test <- which((ptable_gp$lhs == "textual") &
                (ptable_gp$op  == "=~") &
                (ptable_gp$rhs == "x4") &
                (ptable_gp$block == 2) &
                (ptable_gp$group == 2))
test_that("Check output in two-group CFA", {
    expect_equal(
        i,
        i_test
      )
  })

test_that("Can it detect there are more than one match?", {
    expect_error(
        get_i_from_lor(ptable_gp, 
            lor = data.frame(lhs = "textual",
                             op  = "=~",
                             rhs = "x4"))
      )
  })
