#' @title LBCI for One Parameter
#'
#' @description Finds the likelihood-based confidence interval (LBCI)
#'  for one parameter.
#'
#' @details [ci_i()] calls a function ([ci_bound_wn_i()] by default)
#'  twice to find the two bounds (limits) for a confidence interval.
#'  The default method is the Wu-Neale-2012 method. Please refer to
#'  [ci_bound_wn_i()] for further information.
#'
#' This function is not supposed to be used directly by users. It is
#' exported such that interested users can examine how a confidence bound it
#' found.
#'
#' @return A numeric vector of two elements. The first element is the
#' lower bound, and the second element is the upper bound.
#'
#' The diagnostic information from the function called in finding the
#' lower and upper founds are stored in the attributes `lb_diag` and
#' `ub_diag`, for the lower bound and the upper bound, respectively.
#'
#' @param i The position of the target parameters as appeared in the
#'  parameter table of the [lavaan::lavaan-class] object.
#'
#' @param sem_out The SEM output. Currently supports
#'  [lavaan::lavaan-class] outputs only.
#'
#' @param method The approach to be used. Default is "wn"
#'  (Wu-Neale-2012 Method). The other methods are disabled for now.
#'
#' @param standardized Boolean. Whether the LBCI for the standardized
#'  solution is to be searched. Default is `FALSE`.
#'
#' @param robust Whether the LBCI based on robust likelihood ratio
#'  test is to be found. Only "satorra.2000" in [lavaan] is supported
#'  for now. If `"none"``, the default, then likelihood ratio test based
#'  on maximum likelihood estimation will be used.
#'
#' @param ... Arguments to be passed to the function corresponds to
#'  the requested method ([ci_bound_wn_i()] for "wn").
#'
#' @seealso
#' [semlbci()], [ci_bound_wn_i()]
#'
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
#' # THe method "wn" need the constraint function:
#' fn_constr0 <- set_constraint(fit_med)
#' # Call ci_i to find the bounds
#' out <- ci_i(1, npar = 5, sem_out = fit_med, method = "wn",
#'             f_constr = fn_constr0)
#' out$bounds
#'
#'@export

ci_i <- function(i,
                 sem_out,
                 method = "wn",
                 standardized = FALSE,
                 robust = "none",
                 ...) {
    # It should be the job of the calling function to check whether it is 
    # appropriate to use the robust method.
    if (tolower(robust) == "satorra.2000") {
        sf_full <- scaling_factor(sem_out, i)
        sf <- sf_full$c_r
      } else {
        sf_full <- NA
        sf <- 1
      }
    if (method == "wn") {
        lb_time <- system.time(lb <- ci_bound_wn_i(i,
                                                   sem_out = sem_out,
                                                   which = "lbound",
                                                   standardized = standardized,
                                                   sf = sf,
                                                    ...))
        ub_time <- system.time(ub <- ci_bound_wn_i(i,
                                                   sem_out = sem_out,
                                                   which = "ubound",
                                                   standardized = standardized,
                                                   sf = sf,
                                                    ...))
      }
    if (method == "nm") {
        stop("The method 'nm' is no longer supported.")
      }
    out <- list(bounds = c(lbound = lb$bound, ubound = ub$bound),
                diags = list(lb_diag = lb$diag, ub_diag = ub$diag),
                method = method,
                times = list(lb_time = lb_time[3], ub_time = ub_time[3]),
                sf_full = sf_full)
    out
  }
