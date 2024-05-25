skip_on_cran()

# Ready

# After skip("Long test"): WIP

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
fit_gp <- sem(HS.model, data = HolzingerSwineford1939,
              group = "school",
              group.equal = "intercepts")

# Unstandardized

# One group

est_i <- gen_est_i(i = 9, sem_out = fit)
system.time(
ci_lb <- ci_bound_ur(sem_out = fit,
                     func = est_i,
                     which = "lbound",
                     user_callr = FALSE)
)
system.time(
ci_ub <- ci_bound_ur(sem_out = fit,
                     func = est_i,
                     which = "ubound",
                     user_callr = TRUE)
)
expect_equal(ci_lb$lrt[2, "Pr(>Chisq)"],
             .05,
             tolerance = 1e-3)
expect_equal(ci_ub$lrt[2, "Pr(>Chisq)"],
             .05,
             tolerance = 1e-3)

est_i <- gen_est_i(i = 25, sem_out = fit)
system.time(
ci_lb <- ci_bound_ur(sem_out = fit,
                      func = est_i,
                      which = "lbound",
                      use_callr = TRUE)
)
system.time(
ci_ub <- ci_bound_ur(sem_out = fit,
                      func = est_i,
                      which = "ubound",
                      user_callr = FALSE)
)
expect_equal(round(ci_lb$lrt[2, "Pr(>Chisq)"], 2),
             .05,
             tolerance = 1e-2)
expect_equal(ci_ub$lrt[2, "Pr(>Chisq)"],
             .05,
             tolerance = 1e-3)

ci_lbci <- semlbci(sem_out = fit, pars = "ab :=", use_pbapply = FALSE)

expect_equal(unname(round(c(ci_lb$bound, ci_ub$bound), 3)),
             unname(round(unlist(confint(ci_lbci)), 3)))

# Two groups

est_i <- gen_est_i(i = 46, sem_out = fit_gp)
system.time(
ci_lb <- ci_bound_ur(sem_out = fit_gp,
                      func = est_i,
                      which = "lbound",
                      use_callr = FALSE)
)
system.time(
ci_ub <- ci_bound_ur(sem_out = fit_gp,
                      func = est_i,
                      which = "ubound",
                      use_callr = TRUE)
)
expect_equal(ci_lb$lrt[2, "Pr(>Chisq)"],
             .05,
             tolerance = 1e-2)
expect_equal(ci_ub$lrt[2, "Pr(>Chisq)"],
             .05,
             tolerance = 1e-3)

# # Too long to run
# est_i <- gen_est_i(i = 73, sem_out = fit_gp)
# system.time(
# ci_lb <- ci_bound_ur(sem_out = fit_gp,
#                       func = est_i,
#                       which = "lbound",
#                       progress = TRUE)
# )
# system.time(
# ci_ub <- ci_bound_ur(sem_out = fit_gp,
#                       func = est_i,
#                       which = "ubound",
#                       progress = TRUE)
# )
# ci_lbci <- semlbci(sem_out = fit_gp, pars = "ab :=")

# ci_lb$lrt
# ci_ub$lrt
# c(ci_lb$bound, ci_ub$bound)
# confint(ci_lbci)
# parameterEstimates(fit_gp)[73, ]

# Standardized

skip("WIP")

est_i <- gen_est_i(i = 9, sem_out = fit, standardized = TRUE)
system.time(
ci_lb <- ci_bound_ur(sem_out = fit,
                      func = est_i,
                      which = "lbound",
                      progress = TRUE,
                      use_callr = FALSE)
)
system.time(
ci_ub <- ci_bound_ur(sem_out = fit,
                     func = est_i,
                     which = "ubound",
                     progress = TRUE,
                     use_callr = TRUE)
)

ci_lbci <- semlbci(sem_out = fit, pars = "speed =~ x9", standardized = TRUE)

expect_equal(round(ci_lb$lrt[2, "Pr(>Chisq)"], 2),
             .05,
             tolerance = 1e-2)
expect_equal(ci_ub$lrt[2, "Pr(>Chisq)"],
             .05,
             tolerance = 1e-2)

expect_equal(unname(round(c(ci_lb$bound, ci_ub$bound), 3)),
             unname(round(unlist(confint(ci_lbci)), 3)))

# TO WORK

est_i <- gen_est_i(i = 25, sem_out = fit, standardized = TRUE)
system.time(
ci_lb <- ci_bound_ur(sem_out = fit,
                      func = est_i,
                      which = "lbound",
                      progress = TRUE)
)
system.time(
ci_ub <- ci_bound_ur(sem_out = fit,
                      func = est_i,
                      which = "ubound",
                      progress = TRUE)
)

ci_lbci <- semlbci(sem_out = fit, pars = "ab :=", standardized = TRUE)

ci_lb$lrt
ci_ub$lrt
c(ci_lb$bound, ci_ub$bound)
confint(ci_lbci)
standardizedSolution(fit)[25, ]

est_i <- gen_est_i(i = 46, sem_out = fit_gp, standardized = TRUE)
system.time(
ci_lb <- ci_bound_ur(sem_out = fit_gp,
                      func = est_i,
                      which = "lbound",
                      progress = TRUE)
)
system.time(
ci_ub <- ci_bound_ur(sem_out = fit_gp,
                      func = est_i,
                      which = "ubound",
                      progress = TRUE)
)

ci_lbci <- semlbci(sem_out = fit_gp, pars = "visual ~~ 2*textual", standardized = TRUE)

ci_lb$lrt
ci_ub$lrt
c(ci_lb$bound, ci_ub$bound)
confint(ci_lbci)
standardizedSolution(fit_gp)[46, ]


# Too difficult to search by both methods

# est_i <- gen_est_i(i = 73, sem_out = fit_gp, standardized = TRUE)
# system.time(
# ci_lb <- ci_bound_ur(sem_out = fit_gp,
#                       func = est_i,
#                       which = "lbound",
#                       progress = TRUE)
# )
# system.time(
# ci_ub <- ci_bound_ur(sem_out = fit_gp,
#                       func = est_i,
#                       which = "ubound",
#                       progress = TRUE)
# )

# ci_lbci <- semlbci(sem_out = fit_gp, pars = "ab :=", standardized = TRUE)

# ci_lb$lrt
# ci_ub$lrt
# c(ci_lb$bound, ci_ub$bound)
# confint(ci_lbci)
# standardizedSolution(fit_gp)[46, ]
