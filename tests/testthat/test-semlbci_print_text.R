skip_on_cran()

library(testthat)
library(semlbci)

# Fit the model

library(lavaan)

data(cfa_two_factors_mg)
dat <- cfa_two_factors_mg
mod <-
"
f1 =~ x1 + c(a1, a2)*x2 + c(c1, c2)*x3
f2 =~ x4 + c(b1, b2)*x5 + c(d1, d2)*x6
f1 ~ c(fr1, fr2)*f2
ab := a1 * b2
c1 == c2
"
fit <- lavaan::sem(mod, cfa_two_factors_mg, group = "gp")

mod_nogp <-
"
f1 =~ x1 + a1*x2 + c2*x3
f2 =~ x4 + b2*x5 + x6
f1 ~ f2
ab := a1 * b2
"
fit_nogp <- lavaan::sem(mod_nogp, cfa_two_factors_mg)

# Find the LBCIs

# pars <- c("c2 :=",
#           "f1 ~ f2",
#           "ab :=")
pars <- c("f1 =~ x3",
          "ab :=")
system.time(
    lbci_fit <- semlbci(fit,
                        pars = pars,
                        method = "wn",
                        verbose = TRUE,
                        opts = list(ftol_rel = 1e-5))
  )

lbci_fit_warn1 <- lbci_fit
lbci_fit_warn1[3, "ratio_lb"] <- 3
lbci_fit_warn2 <- lbci_fit
lbci_fit_warn2[26, "post_check_ub"] <- FALSE
lbci_fit_warn3 <- lbci_fit
lbci_fit_warn3[47, "status_ub"] <- 99

pars_nogp <- c("f1 =~ x3",
               "ab :=")
system.time(
    lbci_fit_nogp <- semlbci(fit_nogp,
                             pars = pars,
                             method = "wn",
                             verbose = TRUE,
                             opts = list(ftol_rel = 1e-5))
  )

lbci_fit_nogp_warn1 <- lbci_fit_nogp
lbci_fit_nogp_warn1[3, "ratio_lb"] <- 3
lbci_fit_nogp_warn2 <- lbci_fit_nogp
lbci_fit_nogp_warn2[16, "post_check_ub"] <- FALSE
lbci_fit_nogp_warn3 <- lbci_fit_nogp
lbci_fit_nogp_warn3[3, "status_ub"] <- 99

test_that("print.semlbci, text", {
    expect_output(print(lbci_fit,
                        output = "text",
                        sem_out = fit),
                  "lb.lower")
    expect_false(any(grepl("ci.upper",
                     capture.output(print(lbci_fit,
                                      output = "text",
                                      sem_out = fit,
                                      lbci_only = TRUE)))))
    expect_output(print(lbci_fit,
                        output = "text",
                        sem_out = fit,
                        drop_no_lbci = FALSE),
                  "--")
    expect_false(any(grepl("ci.upper",
                     capture.output(print(lbci_fit,
                        output = "text",
                        sem_out = fit,
                        lbci_only = TRUE,
                        drop_no_lbci = FALSE)))))
    expect_output(print(lbci_fit,
                        output = "text",
                        sem_out = fit,
                        lbci_only = TRUE,
                        drop_no_lbci = FALSE),
                  "--")
    expect_output(print(lbci_fit_warn1,
                        output = "text",
                        sem_out = fit),
                  "Ratio:")
    expect_output(print(lbci_fit_warn2,
                        output = "text",
                        sem_out = fit),
                  "Check:")
    expect_output(print(lbci_fit_warn3,
                        output = "text",
                        sem_out = fit),
                  "Status:")
    expect_output(print(lbci_fit_warn1,
                        output = "text",
                        sem_out = fit,
                        verbose = TRUE),
                  "Check:")
    expect_output(print(lbci_fit,
                        output = "text",
                        sem_out = fit,
                        verbose = TRUE),
                  "Check:")
    expect_output(print(lbci_fit_warn1,
                        output = "text",
                        sem_out = fit,
                        verbose = TRUE,
                        drop_no_lbci = FALSE),
                  "Check:")
    expect_output(print(lbci_fit_warn1,
                        output = "text",
                        sem_out = fit,
                        verbose = TRUE,
                        drop_no_lbci = FALSE),
                  "--")
    expect_false(any(grepl("Likelihood-Based ",
                     capture.output(print(lbci_fit,
                        output = "text",
                        sem_out = fit,
                        annotation = FALSE)))))
  })

test_that("print.semlbci, text", {
    expect_output(print(lbci_fit_nogp,
                        output = "text",
                        sem_out = fit_nogp),
                  "lb.lower")
    expect_false(any(grepl("ci.upper",
                     capture.output(print(lbci_fit_nogp,
                                      output = "text",
                                      sem_out = fit_nogp,
                                      lbci_only = TRUE)))))
    expect_output(print(lbci_fit_nogp,
                        output = "text",
                        sem_out = fit_nogp,
                        drop_no_lbci = FALSE),
                  "--")
    expect_false(any(grepl("ci.upper",
                     capture.output(print(lbci_fit_nogp,
                        output = "text",
                        sem_out = fit_nogp,
                        lbci_only = TRUE,
                        drop_no_lbci = FALSE)))))
    expect_output(print(lbci_fit_nogp,
                        output = "text",
                        sem_out = fit_nogp,
                        lbci_only = TRUE,
                        drop_no_lbci = FALSE),
                  "--")
    expect_output(print(lbci_fit_nogp_warn1,
                        output = "text",
                        sem_out = fit_nogp),
                  "Ratio:")
    expect_output(print(lbci_fit_nogp_warn2,
                        output = "text",
                        sem_out = fit_nogp),
                  "Check:")
    expect_output(print(lbci_fit_nogp_warn3,
                        output = "text",
                        sem_out = fit_nogp),
                  "Status:")
    expect_output(print(lbci_fit_nogp_warn1,
                        output = "text",
                        sem_out = fit_nogp,
                        verbose = TRUE),
                  "Check:")
    expect_output(print(lbci_fit_nogp,
                        output = "text",
                        sem_out = fit_nogp,
                        verbose = TRUE),
                  "Check:")
    expect_output(print(lbci_fit_nogp_warn1,
                        output = "text",
                        sem_out = fit_nogp,
                        verbose = TRUE,
                        drop_no_lbci = FALSE),
                  "Check:")
    expect_output(print(lbci_fit_nogp_warn1,
                        output = "text",
                        sem_out = fit_nogp,
                        verbose = TRUE,
                        drop_no_lbci = FALSE),
                  "--")
  })

# pars <- c("c2 :=",
#           "f1 ~ f2",
#           "ab :=")
pars <- c("f2 =~ c(1)*x5")
system.time(
    lbci_std <- semlbci(fit,
                        pars = pars,
                        method = "wn",
                        verbose = TRUE,
                        p_tol = 5e-2,
                        opts = list(ftol_rel = 1e-5,
                                    maxeval = 10),
                        standardized = TRUE)
  )

lbci_std_warn1 <- lbci_std
lbci_std_warn1[5, "ratio_lb"] <- 3
lbci_std_warn2 <- lbci_std
lbci_std_warn2[5, "post_check_ub"] <- FALSE
lbci_std_warn3 <- lbci_std
lbci_std_warn3[5, "status_ub"] <- 99

# pars_nogp <- c("f1 =~ x3",
#                "f1 ~ f2",
#                "ab :=")
pars_nogp <- c("f1 ~ f2")
system.time(
    lbci_std_nogp <- semlbci(fit_nogp,
                             pars = pars,
                             method = "wn",
                             verbose = TRUE,
                             standardized = TRUE,
                             opts = list(ftol_rel = 1e-6))
  )

lbci_std_nogp_warn1 <- lbci_std_nogp
lbci_std_nogp_warn1[5, "ratio_lb"] <- 3
lbci_std_nogp_warn2 <- lbci_std_nogp
lbci_std_nogp_warn2[5, "post_check_ub"] <- FALSE
lbci_std_nogp_warn3 <- lbci_std_nogp
lbci_std_nogp_warn3[5, "status_ub"] <- 99

test_that("print.semlbci, text", {
    expect_output(print(lbci_std,
                        output = "text",
                        sem_out = fit),
                  "Standardized")
    expect_output(print(lbci_std,
                        output = "text",
                        sem_out = fit),
                  "lb.lower")
    expect_false(any(grepl("ci.upper",
                     capture.output(print(lbci_std,
                                      output = "text",
                                      sem_out = fit,
                                      lbci_only = TRUE)))))
    expect_output(print(lbci_std,
                        output = "text",
                        sem_out = fit,
                        drop_no_lbci = FALSE),
                  "--")
    expect_false(any(grepl("ci.upper",
                     capture.output(print(lbci_std,
                        output = "text",
                        sem_out = fit,
                        lbci_only = TRUE,
                        drop_no_lbci = FALSE)))))
    expect_output(print(lbci_std,
                        output = "text",
                        sem_out = fit,
                        lbci_only = TRUE,
                        drop_no_lbci = FALSE),
                  "--")
    expect_output(print(lbci_std_warn1,
                        output = "text",
                        sem_out = fit),
                  "Ratio:")
    expect_output(print(lbci_std_warn2,
                        output = "text",
                        sem_out = fit),
                  "Check:")
    expect_output(print(lbci_std_warn3,
                        output = "text",
                        sem_out = fit),
                  "Status:")
    expect_output(print(lbci_std_warn1,
                        output = "text",
                        sem_out = fit,
                        verbose = TRUE),
                  "Check:")
    expect_output(print(lbci_std,
                        output = "text",
                        sem_out = fit,
                        verbose = TRUE),
                  "Check:")
    expect_output(print(lbci_std_warn1,
                        output = "text",
                        sem_out = fit,
                        verbose = TRUE,
                        drop_no_lbci = FALSE),
                  "Check:")
    expect_output(print(lbci_std_warn1,
                        output = "text",
                        sem_out = fit,
                        verbose = TRUE,
                        drop_no_lbci = FALSE),
                  "--")
    expect_false(any(grepl("Likelihood-Based ",
                     capture.output(print(lbci_std,
                        output = "text",
                        sem_out = fit,
                        annotation = FALSE)))))
  })


test_that("print.semlbci, text", {
    expect_output(print(lbci_std_nogp,
                        output = "text",
                        sem_out = fit_nogp),
                  "Standardized")
    expect_output(print(lbci_std_nogp,
                        output = "text",
                        sem_out = fit_nogp),
                  "lb.lower")
    expect_false(any(grepl("ci.upper",
                     capture.output(print(lbci_std_nogp,
                                      output = "text",
                                      sem_out = fit_nogp,
                                      lbci_only = TRUE)))))
    expect_output(print(lbci_std_nogp,
                        output = "text",
                        sem_out = fit_nogp,
                        drop_no_lbci = FALSE),
                  "--")
    expect_false(any(grepl("ci.upper",
                     capture.output(print(lbci_std_nogp,
                        output = "text",
                        sem_out = fit_nogp,
                        lbci_only = TRUE,
                        drop_no_lbci = FALSE)))))
    expect_output(print(lbci_std_nogp,
                        output = "text",
                        sem_out = fit_nogp,
                        lbci_only = TRUE,
                        drop_no_lbci = FALSE),
                  "--")
    expect_output(print(lbci_std_nogp_warn1,
                        output = "text",
                        sem_out = fit_nogp),
                  "Ratio:")
    expect_output(print(lbci_std_nogp_warn2,
                        output = "text",
                        sem_out = fit_nogp),
                  "Check:")
    expect_output(print(lbci_std_nogp_warn3,
                        output = "text",
                        sem_out = fit_nogp),
                  "Status:")
    expect_output(print(lbci_std_nogp_warn1,
                        output = "text",
                        sem_out = fit_nogp,
                        verbose = TRUE),
                  "Check:")
    expect_output(print(lbci_std_nogp,
                        output = "text",
                        sem_out = fit_nogp,
                        verbose = TRUE),
                  "Check:")
    expect_output(print(lbci_std_nogp_warn1,
                        output = "text",
                        sem_out = fit_nogp,
                        verbose = TRUE,
                        drop_no_lbci = FALSE),
                  "Check:")
    expect_output(print(lbci_std_nogp_warn1,
                        output = "text",
                        sem_out = fit_nogp,
                        verbose = TRUE,
                        drop_no_lbci = FALSE),
                  "--")
  })

# Test check

test_that("sem_out", {
    expect_true(compare_semlbci_sem_out(lbci_fit,
                                        fit))
    expect_true(compare_semlbci_sem_out(lbci_fit_nogp,
                                        fit_nogp))
    expect_false(compare_semlbci_sem_out(lbci_fit,
                                         fit_nogp))
    expect_false(compare_semlbci_sem_out(lbci_fit_nogp,
                                         fit))
    expect_error(print(lbci_fit,
                       output = "lavaan",
                       sem_out = fit_nogp))
    expect_error(print(lbci_fit_nogp,
                       output = "lavaan",
                       sem_out = fit))
  })

# Robust

mod_nogp_rb <-
"
f1 =~ x1 + a1*x2 + c2*x3
f2 =~ x4 + b2*x5 + x6
f1 ~ f2
ab := a1 * b2
"
fit_nogp_rob <- lavaan::sem(mod_nogp, cfa_two_factors_mg, test = "satorra.bentler")

pars_nogp <- c("f1 =~ x3",
               "ab :=")
system.time(
    lbci_fit_nogp_rb <- semlbci(fit_nogp_rob,
                             pars = pars,
                             method = "wn",
                             robust = "satorra.2000",
                             verbose = TRUE,
                             opts = list(ftol_rel = 1e-5))
  )


test_that("print.semlbci, text, robust", {
    expect_output(print(lbci_fit_nogp_rb,
                        output = "text",
                        sem_out = fit_nogp_rob),
                  "lb.lower")
    expect_false(any(grepl("ci.upper",
                     capture.output(print(lbci_fit_nogp_rb,
                                      output = "text",
                                      sem_out = fit_nogp_rob,
                                      lbci_only = TRUE)))))
    expect_output(print(lbci_fit_nogp_rb,
                        output = "text",
                        sem_out = fit_nogp_rob,
                        drop_no_lbci = FALSE),
                  "--")
    expect_false(any(grepl("ci.upper",
                     capture.output(print(lbci_fit_nogp_rb,
                        output = "text",
                        sem_out = fit_nogp_rob,
                        lbci_only = TRUE,
                        drop_no_lbci = FALSE)))))
    expect_output(print(lbci_fit_nogp_rb,
                        output = "text",
                        sem_out = fit_nogp_rob,
                        lbci_only = TRUE,
                        drop_no_lbci = FALSE),
                  "--")
  })
