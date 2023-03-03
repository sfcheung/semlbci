#' @title A 'cibound' Output From a 'semlbci' Object
#'
#' @description Get the `cibound` output of a bound from
#'  a `semlbci` object, the output of [semlbci()].
#'
#' @details To get the original output of [ci_bound_wn_i()]
#'  for a bound. Usually for diagnosis.
#'
#' @return A `cibound`-class object. See [ci_bound_wn_i()]
#' for details.
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
#' get_cibound(lbci_med, row_id = 6)
#'
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
