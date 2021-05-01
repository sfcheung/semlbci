library(testthat)
library(semlbci)

library(lavaan)

dat <- HolzingerSwineford1939
mod <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
"

fit_lavaan <- cfa(mod, dat)

out1 <- get_lhs_op_rhs(i = 6, fit_lavaan)
out2 <- get_lhs_op_rhs(i = 6, fit_lavaan, more = TRUE)

test_that("Check output in one-group CFA", {
    expect_equal(
        out1,
        data.frame(lhs = "textual", op = "=~", rhs = "x6")
      )
    expect_equal(
        out2,
        data.frame(lhs = "textual", op = "=~", rhs = "x6",
                   block = 1, group = 1)
      )
  })

fit_lavaan_gp <- cfa(mod, dat, group = "school")


summary(fit)


out1gp <- get_lhs_op_rhs(i = 58, fit_lavaan_gp)
out2gp <- get_lhs_op_rhs(i = 58, fit_lavaan_gp, more = TRUE)

test_that("Check output in two-group CFA", {
    expect_equal(
        out1gp,
        data.frame(lhs = "visual", op = "~~", rhs = "textual")
      )
    expect_equal(
        out2gp,
        data.frame(lhs = "visual", op = "~~", rhs = "textual",
                   block = 2, group = 2)
      )
  })
