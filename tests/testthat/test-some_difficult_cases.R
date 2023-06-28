skip("WIP")
library(testthat)
library(semlbci)
library(profvis)

library(lavaan)

set.seed(1657656)
dat <- HolzingerSwineford1939[sample(301, 100),  ]

mod <-
"
fx =~ x2 + x1 + x3
fm =~ x5 + x4 + x6
fy =~ x8 + x7 + x9
fm ~ a*fx
fy ~ b*fm
ab := a*b
"
fit <- lavaan::sem(mod, dat)

semlbci(fit, pars = c("ab :="), parallel = FALSE)

fn_constr0 <- set_constraint(fit)
set_start_wn(i = 24,
             sem_out = fit,
             which = "lbound",
             standardized = FALSE,
             ciperc = .95)
poutl <- profvis(outl <- ci_i_one(i = 24,
                which = "lbound",
                sem_out = fit,
                npar = 20,
                wald_ci_start = TRUE,
                f_constr = fn_constr0,
                verbose = TRUE,
                opts = list(print_level = 3)))
poutl

poutu <- profvis(outu <- ci_i_one(i = 24,
                which = "ubound",
                sem_out = fit,
                npar = 20,
                f_constr = fn_constr0,
                verbose = TRUE,
                opts = list(print_level = 3)))
poutu
