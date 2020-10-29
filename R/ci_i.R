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

ci_i <- function(i, ...) {
    lb <- ci_bound_i(i, which = "lbound", ...)
    ub <- ci_bound_i(i, which = "ubound", ...)
    out <- c(lb, ub)
    if (!is.null(attr(lb, "history"))) {
        attr(out, "lb_history") <- attr(lb, "history")
      }
    if (!is.null(attr(ub, "history"))) {
        attr(out, "ub_history") <- attr(ub, "history")
      }
    out
  }
