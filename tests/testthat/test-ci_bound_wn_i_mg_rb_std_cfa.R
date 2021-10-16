skip("Skip due to speed or other issues")
# To be tested in interactive sessions only due to scoping or speed issues

library(testthat)
library(semlbci)

# Fit the model

library(lavaan)

data(cfa_two_factors_mg)
dat <- cfa_two_factors_mg
mod <- 
"
f1 =~ x1 + c(a1, a2)*x2 + x3
f2 =~ x4 + x5 + x6
f1 ~~ c(r1, r2)*f2
"
fit <- lavaan::cfa(mod, cfa_two_factors_mg, test = "satorra.bentler", group = "gp")

# Find the scaling factors

sf1 <- scaling_factor2(fit, 2, standardized = TRUE)
sf2 <- scaling_factor2(fit, 30, standardized = TRUE)

# Find the LBCIs

ciperc <- .96

fn_constr0 <- set_constraint(fit, ciperc = ciperc)

opts0 <- list()
opts0 <- list(ftol_abs = 1e-7,
              ftol_rel = 1e-7,
              xtol_abs = 1e-7,
              xtol_rel = 1e-7
#              tol_constraints_eq = 1e-10
              )
time1l <- system.time(out1l <- ci_bound_wn_i( 2, 38, sem_out = fit, which = "lbound", opts = opts0, f_constr = fn_constr0, verbose = TRUE, ciperc = ciperc, sf = sf1$c_r, sf2 = sf1$c_rb, standardized = TRUE, wald_ci_start = FALSE, std_method = "internal"))
time1u <- system.time(out1u <- ci_bound_wn_i( 2, 38, sem_out = fit, which = "ubound", opts = opts0, f_constr = fn_constr0, verbose = TRUE, ciperc = ciperc, sf = sf1$c_r, sf2 = sf1$c_rb, standardized = TRUE, wald_ci_start = FALSE, std_method = "internal"))
time2l <- system.time(out2l <- ci_bound_wn_i(30, 38, sem_out = fit, which = "lbound", opts = opts0, f_constr = fn_constr0, verbose = TRUE, ciperc = ciperc, sf = sf2$c_r, sf2 = sf2$c_rb, standardized = TRUE, wald_ci_start = FALSE, std_method = "internal"))
time2u <- system.time(out2u <- ci_bound_wn_i(30, 38, sem_out = fit, which = "ubound", opts = opts0, f_constr = fn_constr0, verbose = TRUE, ciperc = ciperc, sf = sf2$c_r, sf2 = sf2$c_rb, standardized = TRUE, wald_ci_start = FALSE, std_method = "internal"))

timexx <- rbind(time1l, time1u, time2l, time2u)
timexx
colSums(timexx)


# Check the results

test_p <- function(fit0, fit1, ciperc, tol) {
    out <- lavTestLRT(fit0, fit1, method = "satorra.2000", A.method = "exact")
    abs(out[2, "Pr(>Chisq)"] - (1 - ciperc)) < tol
  }

get_scaling_factor <- function(lrt_out) {
    diff_from_p <- qchisq(lrt_out[2, "Pr(>Chisq)"], 1, lower.tail = FALSE)
    chisq_1 <- lrt_out[2, "Chisq"]
    chisq_0 <- lrt_out[1, "Chisq"]
    chisq_diff_c <- chisq_1 - chisq_0
    chisq_diff_p <- qchisq(lrt_out[2, "Pr(>Chisq)"], 1, lower.tail = FALSE)
    chisq_diff_r <- lrt_out[2, "Chisq diff"]
    out <- 
      data.frame(chisq_1 = chisq_1,
        chisq_0 = chisq_0,
        chisq_diff_c = chisq_diff_c,
        chisq_diff_r = chisq_diff_r,
        chisq_diff_p = chisq_diff_p,
        c_p = chisq_diff_c / chisq_diff_p,
        c_r = chisq_diff_c / chisq_diff_r)
    out
  }

gen_test_data <- FALSE
if (gen_test_data) {

geteststd1 <- get_std_genfct(fit = fit, i = 2)

modc0 <-
"
f1 =~ x1 + c(a1, a2)*x2 + x3
f2 =~ x4 + x5 + x6
f1 ~~ c(r1, r2)*f2
astd := geteststd1()
"

test_limit <- out1l
modc <- paste(modc0, "\nastd == ", test_limit$bound, "\n0 < 1")
fitc <- lavaan::sem(modc, cfa_two_factors_mg, fixed.x = FALSE, do.fit = FALSE, test = "satorra.bentler", group = "gp")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE, optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02)))
fitc_out1l <- fitc

test_limit <- out1u
modc <- paste(modc0, "\nastd == ", test_limit$bound, "\n0 < 1")
fitc <- lavaan::sem(modc, cfa_two_factors_mg, fixed.x = FALSE, do.fit = FALSE, test = "satorra.bentler", group = "gp")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE, optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02)))
fitc_out1u <- fitc


geteststd2 <- get_std_genfct(fit = fit, i = 30)

modc0 <-
"
f1 =~ x1 + c(a1, a2)*x2 + x3
f2 =~ x4 + x5 + x6
f1 ~~ c(r1, r2)*f2
bstd := geteststd2()
"

test_limit <- out2l
modc <- paste(modc0, "\nbstd == ", test_limit$bound, "\n0 < 1")
fitc <- lavaan::sem(modc, cfa_two_factors_mg, fixed.x = FALSE, do.fit = FALSE, test = "satorra.bentler", group = "gp")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE, optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02)))
fitc_out2l <- fitc

test_limit <- out2u
modc <- paste(modc0, "\nbstd == ", test_limit$bound, "\n0 < 1")
fitc <- lavaan::sem(modc, cfa_two_factors_mg, fixed.x = FALSE, do.fit = FALSE, test = "satorra.bentler", group = "gp")
ptable <- parameterTable(fitc)
ptable[ptable$free > 0, "est"] <- test_limit$diag$history$solution
fitc <- update(fitc, start = ptable, do.fit = TRUE, baseline = FALSE, h1 = FALSE, se = "none",
                   verbose = FALSE, optim.force.converged = TRUE,
                   control = list(eval.max = 2, control.outer = list(tol = 1e-02)))
fitc_out2u <- fitc

save(fitc_out1l, fitc_out1u,
     fitc_out2l, fitc_out2u,
     geteststd1,
     geteststd2,
     file = "inst/testdata/test-ci_bound_wn_i_mg_rb_std_cfa.RData",
     compress = "xz",
     compression_level = 9)
}

load(system.file("testdata", "test-ci_bound_wn_i_mg_rb_std_cfa.RData",
                  package = "semlbci"))

test_that("Check p-value for the chi-square difference test", {
    expect_true(test_p(fitc_out1l, fit, ciperc = ciperc, tol = 1e-5))
    expect_true(test_p(fitc_out1u, fit, ciperc = ciperc, tol = 1e-5))
    expect_true(test_p(fitc_out2l, fit, ciperc = ciperc, tol = 1e-5))
    expect_true(test_p(fitc_out2u, fit, ciperc = ciperc, tol = 1e-5))
  })

