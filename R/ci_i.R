#'@title Find the lower bound and upper bound for one parameter
#'
#'@description Find the lower and upper bound for one parameter.
#'
#'@details
#'
#' Currently supports \code{lavaan} output only.
#'
#'@return
#' The requested bounds
#' Can return the optimization history as an attribute.
#'
#' @param i The position of the target parameters.
#' @param method The approach to be used. Can be "wn" (Wu-Neale-2012) or "nm" 
#'               (Neale-Miller-1997). Default is "wn".
#' @param ... Arguments to be passed to \code{ci_bound_i}.
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
#'@export

ci_i <- function(i, method = "wn", ...) {
    if (method == "wn") {
        lb_time <- system.time(lb <- ci_bound_i(i, which = "lbound", ...))
        ub_time <- system.time(ub <- ci_bound_i(i, which = "ubound", ...))
      }
    if (method == "nm") {
        lb_time <- system.time(lb <- ci_bound_nm_i(i, which = "lbound", ...))
        ub_time <- system.time(ub <- ci_bound_nm_i(i, which = "ubound", ...))
      }
    out <- c(lb, ub)
    attr(out, "lb_diag") <- attr(lb, "diag")
    attr(out, "ub_diag") <- attr(ub, "diag")
    attr(out, "method") <- method
    attr(out, "lb_time") <- lb_time[3]
    attr(out, "ub_time") <- ub_time[3]
    out
  }
