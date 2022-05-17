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
fit_rb <- lavaan::sem(mod, simple_med, fixed.x = FALSE, test = "satorra.bentler")

pars <- c("m ~ x", "y ~ m")

ciperc <- .96

lbci_med <- semlbci(fit, pars = pars,
                    ciperc = ciperc,
                    parallel = FALSE)

lbci_med <- semlbci(fit, pars = pars,
                    ciperc = ciperc,
                    standardized = TRUE)

lbci_med <- semlbci(fit, pars = pars,
                    ciperc = ciperc,
                    parallel = TRUE, ncpus = 2)

lbci_med <- semlbci(fit, pars = pars,
                    ciperc = ciperc,
                    parallel = TRUE, ncpus = 2,
                    standardized = TRUE)

lbci_med <- semlbci(fit, pars = pars,
                    ciperc = ciperc,
                    parallel = TRUE, ncpus = 2,
                    use_pbapply = FALSE)

lbci_med <- semlbci(fit, pars = pars,
                    ciperc = ciperc,
                    parallel = TRUE, ncpus = 2,
                    standardized = TRUE,
                    use_pbapply = FALSE)

lbci_med_rb <- semlbci(fit_rb, pars = pars,
                        ciperc = ciperc,
                        robust = "satorra.2000",
                        parallel = FALSE)

lbci_med_rb <- semlbci(fit_rb, pars = pars,
                        ciperc = ciperc,
                        robust = "satorra.2000",
                        standardized = TRUE)

lbci_med_rb <- semlbci(fit_rb, pars = pars,
                        ciperc = ciperc,
                        robust = "satorra.2000",
                        parallel = TRUE, ncpus = 2)

lbci_med_rb <- semlbci(fit_rb, pars = pars,
                        ciperc = ciperc,
                        robust = "satorra.2000",
                        parallel = TRUE, ncpus = 2,
                        standardized = TRUE)

lbci_med_rb <- semlbci(fit_rb, pars = pars,
                        ciperc = ciperc,
                        robust = "satorra.2000",
                        parallel = TRUE, ncpus = 2,
                        use_pbapply = FALSE)

lbci_med_rb <- semlbci(fit_rb, pars = pars,
                        ciperc = ciperc,
                        robust = "satorra.2000",
                        parallel = TRUE, ncpus = 2,
                        standardized = TRUE,
                        use_pbapply = FALSE)
