skip_on_cran()

# Ready

library(testthat)
library(lavaan)

# Standardized

HS.model <- ' visual  =~ x1 + x2 + d1*x3
              textual =~ x4 + x5 + d2*x6
              visual ~~ textual
              d12 := d1^2'
fit <- sem(HS.model, data = HolzingerSwineford1939)

## One group

test_that("ci_bound_ur: One group, standardized", {

# Take more than one minute to run
# Test one bound is enough

est_i <- gen_est_i(i = 7, sem_out = fit, standardized = TRUE)
system.time(
ci_lb <- ci_bound_ur(sem_out = fit,
                     func = est_i,
                     which = "lbound",
                     progress = FALSE)
)
# system.time(
# ci_ub <- ci_bound_ur(sem_out = fit,
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

ci_lbci <- semlbci(sem_out = fit, pars = "visual ~~ textual", standardized = TRUE, use_pbapply = FALSE)

expect_equal(unname(round(c(ci_lb$bound), 3)),
             unname(round(unlist(confint(ci_lbci))[1], 3)),
             tolerance = 1e-2)

# est_i <- gen_est_i(i = 3, sem_out = fit, standardized = TRUE)
# system.time(
# ci_lb <- ci_bound_ur(sem_out = fit,
#                      func = est_i,
#                      which = "lbound",
#                      progress = FALSE)
# )
# system.time(
# ci_ub <- ci_bound_ur(sem_out = fit,
#                       func = est_i,
#                       which = "ubound",
#                       progress = FALSE)
# )

# expect_equal(round(ci_lb$lrt[2, "Pr(>Chisq)"], 2),
#              .05,
#              tolerance = 1e-3)
# expect_equal(round(ci_ub$lrt[2, "Pr(>Chisq)"], 2),
#              .05,
#              tolerance = 1e-3)

# ci_lbci <- semlbci(sem_out = fit, pars = "visual =~ x3", standardized = TRUE, use_pbapply = FALSE)

# expect_equal(unname(round(c(ci_lb$bound), 3)),
#              unname(round(unlist(confint(ci_lbci))[1], 3)),
#              tolerance = 1e-2)

})
