library(testthat)
library(semlbci)

dat <- cfa_two_factors

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

est_std <- standardizedSolution(fit)

lav_mod <- lavInspect(fit, "est")
lav_mod_pt <- lavInspect(fit, "partable")

ram <- lapply(lav_mod, lav_mod_to_ram)

lav_mod_bak <- mapply(ram_to_lav_mod, ram, lav_mod_pt,
                      SIMPLIFY = FALSE)

test_that("Check conversion", {
    expect_equal(lav_mod_bak[[1]]$lambda, unclass(lav_mod[[1]]$lambda))
    expect_equal(lav_mod_bak[[1]]$beta, unclass(lav_mod[[1]]$beta))
    expect_equal(lav_mod_bak[[2]]$theta, unclass(lav_mod[[2]]$theta))
    expect_equal(lav_mod_bak[[2]]$psi, unclass(lav_mod[[2]]$psi))
  })
