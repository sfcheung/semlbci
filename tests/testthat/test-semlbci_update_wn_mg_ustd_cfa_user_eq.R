skip_on_cran()
skip("To be run in an interactive session")

library(testthat)
library(semlbci)

# Fit the model

library(lavaan)

data(cfa_two_factors_mg)
dat <- cfa_two_factors_mg
mod <-
"
f1 =~ x1 + c(b1, b2)*x2 + c(c1, c1)*x3
f2 =~ x4 + x5 + c(e1, e1)*x6
f1 ~~ c(fr1, fr2)*f2
ce := c1*e1
b1 == c1
"
fit <- lavaan::cfa(mod, cfa_two_factors_mg, group = "gp")

# Find the LBCIs

pars <- c("b1 :=",
          "f2 =~ x5",
          "ce :=")
pars1 <- pars[2]
pars2 <- pars[c(1, 3)]
pars3 <- pars[c(2, 3)]
pars_i <- syntax_to_i(pars, fit)

system.time(
    lbci_fit1 <- semlbci(fit, pars = pars1,
                       method = "wn",
                       verbose = TRUE,
                       opts = list(ftol_rel = 1e-5))
  )
system.time(
    lbci_fit2 <- semlbci(fit, pars = pars2,
                       method = "wn",
                       verbose = TRUE,
                       opts = list(ftol_rel = 1e-5),
                       semlbci_out = lbci_fit1)
  )
system.time(
    lbci_fit2_chk <- semlbci(fit, pars = unique(c(pars1, pars2)),
                       method = "wn",
                       verbose = TRUE,
                       opts = list(ftol_rel = 1e-5),
                       semlbci_out = lbci_fit1)
  )

system.time(
    lbci_fit3 <- semlbci(fit, pars = pars,
                       method = "wn",
                       verbose = TRUE,
                       opts = list(ftol_rel = 1e-5),
                       semlbci_out = lbci_fit1)
  )
system.time(
    lbci_fit3_chk <- semlbci(fit, pars = unique(c(pars1, pars)),
                       method = "wn",
                       verbose = TRUE,
                       opts = list(ftol_rel = 1e-5),
                       semlbci_out = lbci_fit1)
  )

system.time(
    lbci_fit4 <- semlbci(fit, pars = pars3,
                       method = "wn",
                       verbose = TRUE,
                       opts = list(ftol_rel = 1e-5),
                       semlbci_out = lbci_fit1)
  )
system.time(
    lbci_fit4_chk <- semlbci(fit, pars = unique(c(pars1, pars3)),
                       method = "wn",
                       verbose = TRUE,
                       opts = list(ftol_rel = 1e-5),
                       semlbci_out = lbci_fit1)
  )

print(lbci_fit1, annotation = FALSE)
print(lbci_fit2, annotation = FALSE)
print(lbci_fit2_chk, annotation = FALSE)
print(lbci_fit3, annotation = FALSE)
print(lbci_fit3_chk, annotation = FALSE)
print(lbci_fit4, annotation = FALSE)
print(lbci_fit4_chk, annotation = FALSE)

test_that("Check results", {
    expect_equal(lbci_fit2$lbci_lb, lbci_fit2_chk$lbci_lb)
    expect_equal(lbci_fit3$lbci_lb, lbci_fit3_chk$lbci_lb)
    expect_equal(lbci_fit4$lbci_ub, lbci_fit4_chk$lbci_ub)
    expect_warning(semlbci(fit, pars = pars3, semlbci_out = lbci_fit4))
  })

