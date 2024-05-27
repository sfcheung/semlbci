#' @title Equality Constraint for Finding the LBCI by Wu-Neale-2012
#'
#' @description Create the equality constraint for finding the likelihood-based
#'  confidence interval (LBCI) by the Wu-Neale-2012 method.
#'
#' @details
#'
#' ## Important Notice
#'
#' This function is not supposed to be used directly by users in
#' typical scenarios. Its interface is user-*unfriendly* because it
#' should be used through [semlbci()]. It is exported such that
#' interested users can examine how a confidence bound is found, or
#' use it for experiments or simulations.
#'
#'
#' ## Usage
#'
#' The Wu-Neale-2012 method uses a simple objective function that is
#' optimized with an equality constraint. [set_constraint()] generates
#' the equality constraint function to be used by [ci_bound_wn_i()].
#'
#' It currently supports [lavaan::lavaan-class] outputs only.
#'
#' @return An equality constraint function to be used by [ci_bound_wn_i()].
#'
#' @param sem_out The SEM output. Currently supports
#'  [lavaan::lavaan-class] outputs only.
#'
#' @param ciperc The intendeted coverage probability of the confidence
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
        # The model has at least one equality constraint
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
            slot_mod3 <- lavaan::lav_model_set_parameters(slot_mod2, param)
            eq_out <- sem_out@Model@ceq.function(param)
            eq_jac <- sem_out@Model@con.jac
            if (lav_warn) {
                    fit2 <- tryCatch(lavaan::lavaan(
                              slotOptions = slot_opt3,
                              slotParTable = slot_pat2,
                              slotModel = slot_mod3,
                              slotSampleStats = slot_smp2,
                              slotData = slot_dat2),
                              error = function(e) e)
                } else {
                    fit2 <- tryCatch(suppressWarnings(lavaan::lavaan(
                                              slotOptions = slot_opt3,
                                              slotParTable = slot_pat2,
                                              slotModel = slot_mod3,
                                              slotSampleStats = slot_smp2,
                                              slotData = slot_dat2)),
                                     error = function(e) e)
                }
            is_error <- inherits(fit2, "error")
            if (is_error) {
                fit2_gradient <- rbind(rep(-Inf, length(param)))
                fit2_jacobian <- rbind(eq_jac,
                                        rep(-Inf, length(param)))
                out <- list(objective = Inf,
                            gradient = fit2_gradient,
                            constraints = Inf,
                            jacobian = fit2_jacobian,
                            parameterTable = NA)
                return(out)
              } else {
                if (lav_warn) {
                            fit2_gradient <- rbind(lavaan::lavTech(fit2, "gradient"))
                            fit2_jacobian <- rbind(eq_jac,
                                                  lavaan::lavTech(fit2, "gradient"))
                    } else {
                        fit2_gradient <- tryCatch(suppressWarnings(rbind(lavaan::lavTech(fit2, "gradient"))),
                                                  error = function(e) rep(-Inf, length(param)))
                        suppressWarnings(fit2_jacobian <-
                                        rbind(eq_jac,
                                              fit2_gradient))
                    }
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
        # The model has no equality constraint
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
            slot_mod3 <- lavaan::lav_model_set_parameters(slot_mod2, param)
            if (lav_warn) {
                    fit2 <- tryCatch(lavaan::lavaan(
                              slotOptions = slot_opt3,
                              slotParTable = slot_pat2,
                              slotModel = slot_mod3,
                              slotSampleStats = slot_smp2,
                              slotData = slot_dat2),
                              error = function(e) e)
                } else {
                    fit2 <- tryCatch(suppressWarnings(lavaan::lavaan(
                                              slotOptions = slot_opt3,
                                              slotParTable = slot_pat2,
                                              slotModel = slot_mod3,
                                              slotSampleStats = slot_smp2,
                                              slotData = slot_dat2)),
                                     error = function(e) e)
                }
            is_error <- inherits(fit2, "error")
            if (is_error) {
                fit2_gradient <- rbind(rep(-Inf, length(param)))
                fit2_jacobian <- rbind(rep(-Inf, length(param)))
                out <- list(objective = Inf,
                            gradient = fit2_gradient,
                            constraints = Inf,
                            jacobian = fit2_jacobian,
                            parameterTable = NA)
                return(out)
              } else {
                if (lav_warn) {
                        fit2_gradient <- rbind(lavaan::lavTech(fit2, "gradient"))
                        fit2_jacobian <- rbind(lavaan::lavTech(fit2, "gradient"))
                    } else {
                        fit2_gradient <- tryCatch(suppressWarnings(rbind(lavaan::lavTech(fit2, "gradient"))),
                                                  error = function(e) rep(-Inf, length(param)))
                        suppressWarnings(fit2_jacobian <-
                                        rbind(fit2_gradient))
                    }
              }
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