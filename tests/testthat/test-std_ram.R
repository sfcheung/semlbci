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

ram <- lapply(lav_mod, lav_mod_to_ram)

ram_std <- lapply(ram, std_ram)

iA <- diag(nrow(ram_std[[2]]$A))
Binv <- solve(iA - ram_std[[2]]$A)
tmp <- Binv %*% ram_std[[2]]$S %*% t(Binv)

test_that("Check implied correlations", {
    expect_equal(tmp[1:6, 1:6],
                 unclass(cov2cor(lavInspect(fit, "implied")[[2]]$cov)))
  })

lav_mod_std <- lavInspect(fit, "std")
ram_std_check <- lapply(lav_mod_std, lav_mod_to_ram)


test_that("Check against lavaan std.all", {
    expect_equal(ram_std_check[[1]]$A,
                 ram_std[[1]]$A)
    expect_equal(ram_std_check[[1]]$S,
                 ram_std[[1]]$S)
  })
