skip("Test parallel processing: Test in interactive sections")

library(testthat)
library(semlbci)

# context("Check semlbci: No equality constraints, Neale-Miller-1997")

data(simple_med)
dat <- simple_med
mod <- 
"
m ~ x
y ~ m
"
fit <- lavaan::sem(mod, simple_med, fixed.x = FALSE)

lbci_med <- semlbci(fit, parallel = TRUE, ncpus = 5)


