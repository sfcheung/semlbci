#' @title Constraint for Finding the LBCI by the Wu-Neale-2012 Approach
#'
#' @description Sets the constraint for finding the likelihood-based
#'  confidence interval by the Wu-Neale-2012 approach.
#'
#' @details
#'
#' The Wu-Neale-2012 approach uses a simple objective function that is
#' optimized with a constraint. [set_constraint] generates the
#' constraint function used by [ci_bound_wn_i()].
#'
#' Currently supports [lavaan::lavaan-class] outputs only.
#'
#' This function is not to be used by normal users.
#'
#' @return A constraint function for [nloptr].
#'
#' @param sem_out The SEM output. Currently supports
#'  [lavaan::lavaan-class] outputs only.
#'
#' @param ciperc The probability of coverage for the confidence
#'  interval. Default is .95.
#'
#'@examples
#'
#' library(lavaan)
#' data(simple_med)
#' dat <- simple_med
#' mod <-
#' "
#' m ~ x
#' y ~ m
#' "
#' fit_med <- sem(mod, simple_med, fixed.x = FALSE)
#'
#' fn_constr0 <- set_constraint(fit_med)
#' out <- fn_constr0(coef(fit_med), sem_out = fit_med)
#' out
#' lavTech(fit_med, "optim")$fx
#'
#'@export

set_constraint <- function(sem_out, ciperc = .95) {
    p_free <- find_free(sem_out)
    qcrit <- stats::qchisq(ciperc, 1)
    fmin <- lavaan::lavTech(sem_out, "optim")$fx
    if (lavaan::lavTech(sem_out, "ngroups") > 1) {
          n <- lavaan::lavTech(sem_out, "ntotal")
        } else {
          n <- lavaan::lavTech(sem_out, "nobs")
        }
    # NOTE: For lavaan, chisq = 2 * n * fmin
    target <- fmin + qcrit / (2 * n)

    slot_opt2 <- sem_out@Options
    slot_pat2 <- sem_out@ParTable
    slot_mod2 <- sem_out@Model
    slot_smp2 <- sem_out@SampleStats
    slot_dat2 <- sem_out@Data

    slot_opt3 <- slot_opt2
    slot_opt3$do.fit <- FALSE
    slot_opt3$se <- "none"
    slot_opt3$test <- "none"

    # Check if there are any equality constraints
    if (sem_out@Model@eq.constraints ||
        !is.null(body(sem_out@Model@ceq.function))) {
        fn_constraint <- function(param,
                                  sem_out = NULL,
                                  debug = FALSE,
                                  lav_warn = FALSE,
                                  sf = 1,
                                  sf2 = 0) {
            target <- fmin + sf * (qcrit - sf2) / (2 * n)
            if (debug) {
                cat(ls())
                cat(ls(globalenv()))
                }
            # start0 <- lavaan::parameterTable(sem_out)
            # start0[p_free, "est"] <- param
            slot_mod3 <- lavaan::lav_model_set_parameters(slot_mod2, param)
            eq_out <- sem_out@Model@ceq.function(param)
            eq_jac <- sem_out@Model@con.jac
            if (lav_warn) {
                    # fit2 <- lavaan::update(sem_out,
                    #                        start = start0,
                    #                        do.fit = FALSE)
                    fit2 <- lavaan::lavaan(
                              slotOptions = slot_opt3,
                              slotParTable = slot_pat2,
                              slotModel = slot_mod3,
                              slotSampleStats = slot_smp2,
                              slotData = slot_dat2)
                } else {
                    # suppressWarnings(fit2 <- lavaan::update(sem_out,
                    #                                         start = start0,
                    #                                         do.fit = FALSE))
                    suppressWarnings(fit2 <- lavaan::lavaan(
                                              slotOptions = slot_opt3,
                                              slotParTable = slot_pat2,
                                              slotModel = slot_mod3,
                                              slotSampleStats = slot_smp2,
                                              slotData = slot_dat2))
                }
            if (lav_warn) {
                    fit2_gradient <- rbind(lavaan::lavTech(fit2, "gradient"))
                    fit2_jacobian <- rbind(eq_jac,
                                           lavaan::lavTech(fit2, "gradient"))
                } else {
                    suppressWarnings(fit2_gradient <-
                                    rbind(lavaan::lavTech(fit2, "gradient")))
                    suppressWarnings(fit2_jacobian <-
                                    rbind(eq_jac,
                                          lavaan::lavTech(fit2, "gradient")))
                }
            list(
                  objective = lavaan::lavTech(fit2, "optim")$fx,
                  gradient = fit2_gradient,
                  constraints = rbind(t(t(eq_out)),
                                   lavaan::lavTech(fit2, "optim")$fx - target),
                  jacobian = fit2_jacobian,
                  parameterTable = lavaan::parameterTable(fit2))
          }
      } else {
        fn_constraint <- function(param,
                                  sem_out = NULL,
                                  debug = FALSE,
                                  lav_warn = FALSE,
                                  sf = 1,
                                  sf2 = 0) {
            target <- fmin + sf * (qcrit - sf2) / (2 * n)
            if (debug) {
                cat(ls())
                cat(ls(globalenv()))
                }
            # start0 <- lavaan::parameterTable(sem_out)
            # start0[p_free, "est"] <- param
            slot_mod3 <- lavaan::lav_model_set_parameters(slot_mod2, param)
            if (lav_warn) {
                    # fit2 <- lavaan::update(sem_out,
                    #                        start = start0,
                    #                        do.fit = FALSE)
                    fit2 <- lavaan::lavaan(
                              slotOptions = slot_opt3,
                              slotParTable = slot_pat2,
                              slotModel = slot_mod3,
                              slotSampleStats = slot_smp2,
                              slotData = slot_dat2)
                } else {
                    # suppressWarnings(fit2 <- lavaan::update(sem_out,
                    #                                         start = start0,
                    #                                         do.fit = FALSE))
                    suppressWarnings(fit2 <- lavaan::lavaan(
                                              slotOptions = slot_opt3,
                                              slotParTable = slot_pat2,
                                              slotModel = slot_mod3,
                                              slotSampleStats = slot_smp2,
                                              slotData = slot_dat2))
                }
            if (lav_warn) {
                    fit2_gradient <- rbind(lavaan::lavTech(fit2, "gradient"))
                    fit2_jacobian <- rbind(lavaan::lavTech(fit2, "gradient"))
                } else {
                    suppressWarnings(fit2_gradient <-
                                    rbind(lavaan::lavTech(fit2, "gradient")))
                    suppressWarnings(fit2_jacobian <-
                                    rbind(lavaan::lavTech(fit2, "gradient")))
                }
            # fit2@implied <- lavaan::lav_model_implied(slot_mod3)
            list(
                  objective = lavaan::lavTech(fit2, "optim")$fx,
                  gradient = fit2_gradient,
                  constraints = lavaan::lavTech(fit2, "optim")$fx - target,
                  jacobian = fit2_jacobian,
                  parameterTable = lavaan::parameterTable(fit2))
          }
      }
    fn_constraint
  }