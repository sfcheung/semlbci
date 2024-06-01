skip_on_cran()

# Ready

library(testthat)
library(lavaan)

HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9
              visual ~~ a*textual
              visual ~~ b*speed
              textual ~~ d*speed
              ab := a*b'
fit <- sem(HS.model, data = HolzingerSwineford1939)

# Unstandardized

## One group

test_that("ci_bound_ur: One group, unstandardized", {

# Test one bound is enough

est_i <- gen_est_i(i = 9, sem_out = fit)
# system.time(
# ci_lb <- ci_bound_ur(sem_out = fit,
#                      func = est_i,
#                      which = "lbound",
#                      progress = FALSE,
#                      user_callr = FALSE)
# )
system.time(
ci_ub <- ci_bound_ur(sem_out = fit,
                     func = est_i,
                     which = "ubound",
                     progress = FALSE,
                     user_callr = TRUE)
)
# expect_equal(round(ci_lb$lrt[2, "Pr(>Chisq)"], 2),
#              .05,
#              tolerance = 1e-3)
expect_equal(round(ci_ub$lrt[2, "Pr(>Chisq)"], 2),
             .05,
             tolerance = 1e-3)

est_i <- gen_est_i(i = 25, sem_out = fit)
# system.time(
# ci_lb <- ci_bound_ur(sem_out = fit,
#                       func = est_i,
#                       which = "lbound",
#                       progress = FALSE,
#                       use_callr = TRUE)
# )
system.time(
ci_ub <- ci_bound_ur(sem_out = fit,
                      func = est_i,
                      which = "ubound",
                      progress = FALSE,
                      user_callr = FALSE)
)
# expect_equal(round(ci_lb$lrt[2, "Pr(>Chisq)"], 2),
#              .05,
#              tolerance = 1e-3)
expect_equal(round(ci_ub$lrt[2, "Pr(>Chisq)"], 2),
             .05,
             tolerance = 1e-3)

ci_lbci <- semlbci(sem_out = fit, pars = "ab :=", use_pbapply = FALSE)

expect_equal(unname(round(c(ci_ub$bound), 3)),
             unname(round(unlist(confint(ci_lbci))[2], 3)))
})
