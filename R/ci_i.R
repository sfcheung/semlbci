#' @title Find the LBCI for one parameter
#'
#' @description Find the likelihood-based confidence interval (LBCI)
#'              for one parameter.
#'
#' @details
#'
#' This function calls a function to find a bound ([ci_bound_i_nm()] by default)
#' twice to find the two bounds for a confidence interval. The default method
#' is the Neale-Miller-1997 method. Please refer to [ci_bound_i_nm() for further
#' information.
#'
#' This function is not supposed to be used directly by users. It is
#' exported such that interested users can examine how a confidence bound is
#' found.
#'
#' @return
#' A numeric vector of two elements. The first element is the
#' lower bound, and the second element is the upper bound.
#' 
#' The diagnostic information from the function called in finding the
#' lower and upper founds are stored in the
#' attributes `lb_diag` and `ub_diag`, respectively.
#'
#' @param i The position of the target parameters as
#'          appeared in the parameter table of the 
#'          [lavaan::lavaan-class] object.
#' @param method The approach to be used. Can be "wn" 
#'              (Wu-Neale-2012) or "nm"
#'               (Neale-Miller-1997). Default is "wn". 
#'                (This default will be changed to 
#'               "nm" in the 
#'               future. The main function, [semlbci()],
#'               which calls this function, already 
#'                use "nm" as the default.)
#' @param ... Arguments to be passed to [ci_bound_i()], 
#'            [ci_bound_i_nm()], or similar funtions to be 
#'            developed.
#'
#'@examples
#'
#' data(simple_med)
#'
#' library(lavaan)
#' mod <- 
#' "
#' m ~ x
#' y ~ m
#' "
#' fit_med <- lavaan::sem(mod, simple_med, fixed.x = FALSE)
#'
#' parameterTable(fit_med)
#'
#' # Find the LBCI for the first parameter
#' out <- ci_i(1, npar = 5, sem_out = fit_med, method = "nm")
#' out[1:2]
#' 
#' @export

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
