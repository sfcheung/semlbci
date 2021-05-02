#' @title Set the constraint for finding the LBCI by the Wu-Neale-2012 approach
#'
#' @description Set the constraint for finding the LBCI by the
#'              Wu-Neale-2012 approach
#'
#' @details
#'
#' The Wu-Neale-2012 approach use a simple objective function that is opitmized
#' with a constraint. This function generate the constraint function used by
#' [ci_bound_wn_i()].
#'
#' This approach is easy to implement but turns out to be slow
#' in optimization. The Neale-Meale-2007 approach is now the preferred approach.
#' 
#' This function is still in this package, in case we decide to improve the
#' implementation of the Wu-Neale-2012 approach.
#'
#' Currently supports  [lavaan::lavaan-class] outputs only.
#'
#' @return
#' A constraint function for [nloptr].
#'
#' @param sem_out The SEM output. Currently  [lavaan::lavaan-class] outputs
#'                 only.
#' @param ciperc The proportion of coverage for the confidence interval. 
#'               Default is .95.
#'
#'@examples
#' \dontrun{
#' library(lavaan)
#' data(cfa_two_factors)
#' mod <-
#' "
#' f1 =~ x1 + x2 + a*x3
#' f2 =~ x4 + a*x5 + equal('f1=~x2')*x6
#' f1 ~~ 0*f2
#' asq := a^2
#' "
#' fit <- sem(mod, cfa_two_factors)
#' #fn1 <- set_constraint(sem_out)
#'
#' #fn1(coef(sem_out))
#' #fn1(runif(length(coef(sem_out))), .9, 1.1) * coef(sem_out))
#' #fn1(coef(sem_out) + 2)
#' #fn1(coef(sem_out) - .12)
#' }
#'@export

set_constraint <- function(sem_out, ciperc = .95) {
#    force(sem_out)
    # sem_out2 <- eval(sem_out)
    p_free <- find_free(sem_out)
    qcrit <- stats::qchisq(ciperc, 1)
    fmin <- lavaan::lavTech(sem_out, "optim")$fx
    n <- lavaan::lavTech(sem_out, "nobs")
    # NOTE: For lavaan, chisq = 2 * n * fmin
    target <- fmin + qcrit / (2 * n)
    # Check if there are any equality constraints
    if (sem_out@Model@eq.constraints) {
        fn_constraint <- function(param, sem_out = NULL, debug = FALSE, lav_warn = FALSE) {
            if (debug) {
                cat(ls())
                cat(ls(globalenv()))
                }
            start0 <- lavaan::parameterTable(sem_out)
            start0[p_free, "est"] <- param
            eq_out <- sem_out@Model@ceq.function(param)
            eq_jac <- sem_out@Model@con.jac
            if (lav_warn) {
                    fit2 <- lavaan::update(sem_out, start = start0, do.fit = FALSE)
                } else {
                    suppressWarnings(fit2 <- lavaan::update(sem_out, start = start0, do.fit = FALSE))                    
                }
            if (lav_warn) {
                    fit2_gradient <- rbind(lavaan::lavTech(fit2, "gradient"))
                    fit2_jacobian <- rbind(eq_jac, lavaan::lavTech(fit2, "gradient"))
                } else {
                    suppressWarnings(fit2_gradient <- rbind(lavaan::lavTech(fit2, "gradient")))
                    suppressWarnings(fit2_jacobian <- rbind(eq_jac, lavaan::lavTech(fit2, "gradient")))
                }
            list(
                  objective = lavaan::lavTech(fit2, "optim")$fx,
                  gradient = fit2_gradient,
                  constraints = rbind(t(t(eq_out)), lavaan::lavTech(fit2, "optim")$fx - target),
                  jacobian = fit2_jacobian,
                  parameterTable = lavaan::parameterTable(fit2))
          }
      } else {
        fn_constraint <- function(param, sem_out = NULL, debug = FALSE, lav_warn = FALSE) {
            if (debug) {
                cat(ls())
                cat(ls(globalenv()))
                }
            start0 <- lavaan::parameterTable(sem_out)
            start0[p_free, "est"] <- param
            if (lav_warn) {
                    fit2 <- lavaan::update(sem_out, start = start0, do.fit = FALSE)
                } else {
                    suppressWarnings(fit2 <- lavaan::update(sem_out, start = start0, do.fit = FALSE))                    
                }
            if (lav_warn) {
                    fit2_gradient <- rbind(lavaan::lavTech(fit2, "gradient"))
                    fit2_jacobian <- rbind(lavaan::lavTech(fit2, "gradient"))
                } else {
                    suppressWarnings(fit2_gradient <- rbind(lavaan::lavTech(fit2, "gradient")))
                    suppressWarnings(fit2_jacobian <- rbind(lavaan::lavTech(fit2, "gradient")))
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