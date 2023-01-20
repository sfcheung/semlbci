skip("To be run in an interactive session")

library(testthat)
library(semlbci)

# Fit the model

library(lavaan)

data(simple_med)
dat <- simple_med
mod <-
"
m ~ x
y ~ m
"
fit <- lavaan::sem(mod, simple_med, fixed.x = FALSE, test = "satorra.bentler")

# Find the LBCIs

ciperc <- .96

system.time(
    lbci_fit <- semlbci(fit,
                        ciperc = ciperc,
                        pars = c(1, 2),
                        method = "wn",
                        robust = "satorra.2000",
                        verbose = TRUE,
                        standardized = TRUE,
                        opts = list(ftol_rel = 1e-5))
  )

lbci_fit_nb <- nearby_levels(lbci_fit,
                             ciperc_levels = c(-.050, 0, .050))
lbci_fit_ci_order <- ci_order(lbci_fit_nb)

lbci_fit_ci_order_ans <- structure(list(lb_0.99 = c(0.0732955058429774, 0.313789805112419
), lb_0.96 = c(0.113324154786895, 0.34477769701192), lb_0.91 = c(0.140522141534472,
0.365583931602904), ub_0.91 = c(0.381708455384884, 0.542111558787007
), ub_0.96 = c(0.405073164311676, 0.559185891986767), ub_0.99 = c(0.437791063166971,
0.582702217756065)), class = c("ci_order", "data.frame"), row.names = c("m~x",
"y~m"))

# Check with known results

test_that("Check with known results", {
    expect_equal(names(lbci_fit_nb), c("0.91", "0.96", "0.99"))
    expect_equal(lbci_fit_ci_order, lbci_fit_ci_order_ans)
  })

