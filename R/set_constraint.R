#'@title Set the constraint for finding the LBCI
#'
#'@description Set the constraint for finding the LBCI
#'
#'@details
#'
#' Currently supports \code{lavaan} output only.
#'
#'@return
#' A constraint function for nloptr.
#'
#' @param sem_out The SEM output. Currently \code{lavaan} output only.
#' @param ciperc The proportion of coverage for the confidence interval. Default
#'               is .95.
#'
#'@examples
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
#'@export

set_constraint <- function(sem_out, ciperc = .95) {
    force(sem_out)
    p_free <- find_free(sem_out)
    qcrit <- stats::qchisq(ciperc, 1)
    fmin <- lavaan::lavTech(sem_out, "optim")$fx
    n <- lavaan::lavTech(sem_out, "nobs")
    # NOTE: For lavaan, chisq = 2 * n * fmin
    target <- fmin + qcrit/(2 * n)
      fn_constraint <- function(param) {
        start0 <- lavaan::parameterTable(sem_out)
        start0[p_free, "est"] <- param
        fit2 <- lavaan::update(sem_out, start = start0, do.fit = FALSE)
        list(
              objective = lavaan::lavTech(fit2, "optim")$fx,
              gradient = rbind(lavaan::lavTech(fit2, "gradient")),
              constraints = lavaan::lavTech(fit2, "optim")$fx - target,
              jacobian = rbind(lavaan::lavTech(fit2, "gradient")))
      }
    fn_constraint
  }