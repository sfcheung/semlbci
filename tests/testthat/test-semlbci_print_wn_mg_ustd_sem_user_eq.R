skip("WIP")
skip_on_cran()
library(testthat)
library(semlbci)


# Fit the model

library(lavaan)

data(cfa_two_factors_mg)
dat <- cfa_two_factors_mg
mod <-
"
f1 =~ x1 + c(a1, a2)*x2 + c(c1, c2)*x3
f2 =~ x4 + c(b1, b2)*x5 + c(d1, d2)*x6
f1 ~ c(fr1, fr2)*f2
ab := a1 * b2
c1 == c2
"
fit <- lavaan::sem(mod, cfa_two_factors_mg, group = "gp")

# Find the LBCIs

pars <- c("c2 :=",
          "f1 ~ f2",
          "ab :=")
pars_i <- syntax_to_i(pars, fit)
system.time(
    lbci_fit <- semlbci(fit,
                        pars = pars,
                        method = "wn",
                        verbose = TRUE,
                        opts = list(ftol_rel = 1e-6))
  )

lbci_fit

pars <- c("c2 :=",
          "f1 ~ f2",
          "ab :=")
pars_i <- syntax_to_i(pars, fit)
system.time(
    lbci_std <- semlbci(fit,
                        pars = pars,
                        method = "wn",
                        verbose = TRUE,
                        opts = list(ftol_rel = 1e-6),
                        standardized = TRUE)
  )

lbci_std
