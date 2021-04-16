#' @title Set the constraint for finding the LBCI by the Neale-Miller-1997 approach
#'
#' @description Set the constraint for finding the LBCI by the Neale-Miller-1997 approach
#'
#' @details
#'
#' This function is no longer needed and so is not exported.
#'
#' Currently supports \code{lavaan} output only.
#'
#'
#' @return
#' A constraint function for nloptr.
#'
#' @param i The position of the target parameters as in the parameter table of lavaan.
#' @param sem_out The SEM output. Currently \code{lavaan} output only.
#' @param ciperc The proportion of coverage for the confidence interval. Default
#'               is .95.
#' @param envir The enviroment to stroe the state. Default is NULL
#'
#' @examples
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
#' @keywords internal
utils::globalVariables("f_i_free_shared")

set_constraint_nm <- function(i, sem_out, ciperc = .95, envir = NULL, get_fit_from_envir = FALSE) {
#    force(sem_out)
    # sem_out2 <- eval(sem_out)
    p_free <- find_free(sem_out)
    qcrit <- stats::qchisq(ciperc, 1)
    fmin <- lavaan::lavTech(sem_out, "optim")$fx
    n <- lavaan::lavTech(sem_out, "nobs")
    # NOTE: For lavaan, chisq = 2 * n * fmin
    target <- fmin + qcrit / (2 * n)
    # Check if there are any equality constraints
    # if (sem_out@Model@eq.constraints) {
    if (FALSE) {
      } else {
        # if (length(i) == 1) {
        fn_constraint <- function(p_f, sem_out = NULL, debug = FALSE, lav_warn = FALSE) {
            force(i)
            force(get_fit_from_envir)
            if (debug) {
                cat(ls())
                cat(ls(globalenv()))
                }
            start0 <- lavaan::parameterTable(sem_out)
            start0 <- start0[!(start0$op == ":="), ]
            start0[i, "free"] <- 0
            start0[i, "ustart"] <- 0
            start0[i, "est"] <- p_f
            if (!is.null(envir) & (get_fit_from_envir == TRUE)) {
                    fit2 <- envir$f_i_shared
                } else {
                    if (lav_warn) {
                            fit2 <- lavaan::update(sem_out, start0)
                        } else {
                            suppressWarnings(fit2 <- lavaan::update(sem_out, start0))                    
                       }
                    envir$f_i_shared <- fit2
                }
            start0_free <- lavaan::parameterTable(fit2)
            start0_free[i, "free"] <- 1
            fit2_free <- lavaan::update(sem_out, start = start0_free, do.fit = FALSE)
            if (lav_warn) {
                    fit2_gradient <- rbind(lavaan::lavTech(fit2_free, "gradient")[i])
                    fit2_jacobian <- rbind(lavaan::lavTech(fit2_free, "gradient")[i])
                } else {
                    suppressWarnings(fit2_gradient <- rbind(lavaan::lavTech(fit2_free, "gradient")[i]))
                    suppressWarnings(fit2_jacobian <- rbind(lavaan::lavTech(fit2_free, "gradient")[i]))
                }
            if (!is.null(envir)) {
                envir$f_i_free_shared <- fit2_free
            } else {
                f_i_free_shared <<- fit2_free
            }
            list(
                objective = lavaan::lavTech(fit2, "optim")$fx,
                gradient = fit2_gradient,
                constraints = lavaan::lavTech(fit2, "optim")$fx - target,
                jacobian = fit2_jacobian,
                parameterTable = lavaan::parameterTable(fit2))
            }
        return(fn_constraint)
     }
    fn_constraint
  }