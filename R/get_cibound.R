#' @title A 'cibound' Output From a 'semlbci' Object
#'
#' @description Get the `cibound` output of a bound from
#'  a `semlbci` object, the output of [semlbci()].
#'
#' @details The function [get_cibound()]
#' returns the original output of
#' [ci_bound_wn_i()] for a bound.
#' Usually for diagnosis.
#'
#' The function [get_cibound_status_not_0()]
#' checks the status code of each bound,
#' and returns the `cibound` outputs of
#' bounds with status code not equal to
#' zero (i.e., something wrong in the
#' search). Printing it can print the
#' diagnostic information for all bounds
#' that failed in the search.
#'
#' @return
#' [get_cibound()] returns a `cibound`-class object. See [ci_bound_wn_i()]
#' for details.
#' [get_cibound_status_not_0()] returns a list of
#' `cibound`-class objects with `status` not equal
#' to zero. If all bounds have `status` equal to
#' zero, it returns an empty list.
#'
#' @param x The output of [semlbci()].
#'
#' @param row_id The row number in `x`. Should be the number
#'  on the left, not the actual row number, because some rows
#'  may be omitted in the printout of `x`.
#'
#' @param which The bound for which the [ci_bound_wn_i()] is
#'  to be extracted. Either `"lbound"`` or `"ubound"``.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [semlbci()]
#'
#' @examples
#'
#' library(lavaan)
#' mod <-
#' "
#' m ~ a*x
#' y ~ b*m
#' ab := a * b
#' "
#' fit_med <- sem(mod, simple_med, fixed.x = FALSE)
#' p_table <- parameterTable(fit_med)
#' p_table
#' lbci_med <- semlbci(fit_med,
#'                     pars = c("ab :="))
#' lbci_med
#'
#' # Get the output of ci_bound_wn_i() of the lower
#' # bound of the LBCI for the indirect effect:
#' get_cibound(lbci_med, row_id = 6, which = "lbound")
#'
#' # Get the output of ci_bound_wn_i() of the upper
#' # bound of the LBCI for the indirect effect:
#' get_cibound(lbci_med, row_id = 6, which = "ubound")
#'
#' @rdname get_cibound
#' @export

get_cibound <- function(x,
                        row_id,
                        which = c("lbound", "ubound")) {
    which <- match.arg(which)
    if (!inherits(x, "semlbci")) {
        stop("x not a 'semlbci'-class object.")
      }
    cb_out <- switch(which,
               lbound = attr(x, "lb_out")[[row_id]],
               ubound = attr(x, "ub_out")[[row_id]])
    if (!inherits(cb_out, "cibound")) {
        stop("The cibound output not found.")
      }
    cb_out
  }

#' @rdname get_cibound
#' @export

get_cibound_status_not_0 <- function(x) {
    status_not_0_lb <- status_not_0(x, which = "lbound")
    status_not_0_ub <- status_not_0(x, which = "ubound")
    if (any(status_not_0_lb)) {
        out_lb <- lapply(which(status_not_0_lb),
                         get_cibound,
                         x = x,
                         which = "lbound")
      } else {
        out_lb <- list()
      }
    if (any(status_not_0_ub)) {
        out_ub <- lapply(which(status_not_0_ub),
                         get_cibound,
                         x = x,
                         which = "ubound")
      } else {
        out_ub <- list()
      }
    c(out_lb, out_ub)
  }

#' @noRd

status_not_0 <- function(x, which = c("lbound", "ubound")) {
    which <- match.arg(which)
    x_diag <- attr(x, switch(which,
                             lbound = "lb_diag",
                             ubound = "ub_diag"))
    i <- sapply(x_diag, function(x) {
              ifelse(length(x) > 1,
                     x$status != 0,
                     FALSE)
            })
    i
  }