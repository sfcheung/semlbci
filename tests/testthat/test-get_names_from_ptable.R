library(testthat)
library(semlbci)

library(lavaan)

dat <- HolzingerSwineford1939
mod <- "
  visual  =~ x1 + d*x2 + a*x3
  textual =~ x4 + c*x5 + a*x6
  cd := c*d
  a == 2
"

fit_lavaan <- cfa(mod, dat, do.fit = FALSE)

ptable <- parameterTable(fit_lavaan)

out1 <- get_names_from_ptable(ptable)

out1_test <- c(ptable$lhs, ptable$rhs, ptable$label, ptable$plabel)
out1_test <- out1_test[out1_test != ""]

test_that("Check output in one-group CFA", {
    expect_true(
        all(out1 %in% out1_test)
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

out1_gp <- get_names_from_ptable(ptable_gp)

out1gp_test <- c(ptable_gp$lhs, ptable_gp$rhs, ptable_gp$label, ptable_gp$plabel)
out1gp_test <- out1gp_test[out1gp_test != ""]

test_that("Check output in two-group CFA", {
    expect_true(
        all(out1_gp %in% out1gp_test)
      )
  })

x <- data.frame(matrix(NA, 2, 5))

test_that("Can it detect an inappropriate input?", {
    expect_error(
        get_names_from_ptable(x)
      )
  })
