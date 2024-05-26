skip_on_cran()
skip("To be run in an interactive session")

# Ready

library(testthat)
library(lavaan)

HS.model_gp <- ' visual  =~ x1 + x2 + x3
                 textual =~ x4 + x5 + x6
                 visual ~~ c(a1, a2)*textual
                 adiff := a1^2'
fit_gp <- sem(HS.model_gp, data = HolzingerSwineford1939,
              group = "school",
              group.equal = "loadings")

## Two groups

test_that("ci_bound_ur: Two groups, standardized", {

# Take more than one minute to run
# Test one bound is enough

est_i <- gen_est_i(i = 29, sem_out = fit_gp, standardized = TRUE)
system.time(
ci_lb <- ci_bound_ur(sem_out = fit_gp,
                      func = est_i,
                      which = "lbound",
                      progress = FALSE)
)
# system.time(
# ci_ub <- ci_bound_ur(sem_out = fit_gp,
#                       func = est_i,
#                       which = "ubound",
#                       progress = FALSE)
# )

expect_equal(round(ci_lb$lrt[2, "Pr(>Chisq)"], 2),
             .05,
             tolerance = 1e-3)
# expect_equal(round(ci_ub$lrt[2, "Pr(>Chisq)"], 2),
#              .05,
#              tolerance = 1e-3)

ci_lbci <- semlbci(sem_out = fit_gp, pars = "textual =~ 2*x6", standardized = TRUE, use_pbapply = FALSE)

expect_equal(unname(round(c(ci_lb$bound), 3)),
             unname(round(unlist(confint(ci_lbci))[1], 3)),
             tolerance = 1e-2)

# expect_equal(unname(round(c(ci_lb$bound, ci_ub$bound), 3)),
#              unname(round(unlist(confint(ci_lbci)), 3)),
#              tolerance = 1e-2)

})
