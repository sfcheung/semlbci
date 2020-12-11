library(testthat)
library(semlbci)

context("Check sem_out")

dat <- cfa_two_factors

mod <- 
"
f1 =~ x1 + x2 + a*x3
f2 =~ x4 + a*x5 + equal('f1=~x2')*x6
f1 ~~ 0*f2
asq := a^2
"

fit <- lavaan::sem(mod, dat)
