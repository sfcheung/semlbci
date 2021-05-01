library(testthat)
library(semlbci)

library(lavaan)

dat <- HolzingerSwineford1939
mod <- "
  visual  =~ x1 + x2 + a*x3
  textual =~ x4 + b*x5 + x6
  speed   =~ x7 + b*x8 + a*x9
  ab := a*b
  b == 2
"

fit_lavaan <- cfa(mod, dat, do.fit = FALSE)

out1 <- gen_unique_name(get_names_from_ptable(parameterTable(fit_lavaan)))

test_that("Check output in one-group CFA", {
    expect_false(
        out1 %in% get_names_from_ptable(parameterTable(fit_lavaan))
      )
  })

mod2 <- "
  visual  =~ x1 + x2 + c(a1, a2)*x3
  textual =~ x4 + c(b, b)*x5 + x6
  speed   =~ x7 + c(d1, d2)*x8 + c(a1, a3)*x9
  a1a2 := a1*a2
  d1 == 3
  d2 == 4
"

fit_lavaan_gp <- cfa(mod2, dat, group = "school", do.fit = FALSE)

out1 <- gen_unique_name(get_names_from_ptable(parameterTable(fit_lavaan_gp)))

test_that("Check output in two-group CFA", {
    expect_false(
        out1 %in% get_names_from_ptable(parameterTable(fit_lavaan_gp))
      )
  })

test_that("Can it detect an inappropriate input? (1)", {
    expect_error(
        gen_unique_name(list("a", "b"))
      )
  })

test_that("Can it detect an inappropriate input? (2)", {
    expect_error(
        gen_unique_name(c(1, 2))
      )
  })
