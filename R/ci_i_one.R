#' @title Likelihood-Based Confidence Bound for One Parameter
#'
#' @description Find the likelihood-based confidence bound
#'  for one parameter.
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
#' ## Usage
#'
#' [ci_i_one()] is the link between [semlbci()] and the lowest level
#' function (currently [ci_bound_wn_i()]). When called by [semlbci()]
#' to find the bound of a parameter, [ci_i_one()] calls a function
#' ([ci_bound_wn_i()] by default) one or more times to find the bound
#' (limit) for a likelihood-based confidence interval.
#'
#' @return A list of the following elements.
#'
#' - `bound`: The bound located. `NA` if the search failed.
#'
#' - `diags`: Diagnostic information.
#'
#' - `method`: Method used. Currently only `"wn"` is the only possible
#'             value.
#'
#' - `times`: Total time used in the search.
#'
#' - `sf_full`: The scaling and shift factors used.
#'
#' - `ci_bound_i_out`: The original output from [ci_bound_wn_i()].
#'
#' - `attempt_lb_var`: How many attempts used to reduce the lower
#'                     bounds of free variances.
#'
#' - `attempt_more_times`: How many additional attempts used to search
#'                         for the bounds. Controlled by
#'                         `try_k_more_times`.
#'
#' @param i The position (row number) of the target parameters as
#'  appeared in the parameter table of the [lavaan::lavaan-class]
#'  object.
#'
#' @param which Whether the lower bound or the upper bound is to be
#'  found. Must be `"lbound"` or `"ubound"`.
#'
#' @param sem_out The SEM output. Currently supports
#'  [lavaan::lavaan-class] outputs only.
#'
#' @param method The approach to be used. Default is `"wn"`
#'  (Wu-Neale-2012 Method). Another method is "ur",
#' root finding by [stats::uniroot()].
#'
#' @param standardized Logical. Whether the bound of the LBCI of the
#'  standardized solution is to be searched. Default is `FALSE`.
#'
#' @param robust Whether the LBCI based on robust likelihood ratio
#'  test is to be found. Only `"satorra.2000"` in
#'  [lavaan::lavTestLRT()] is supported for now. If `"none"``, the
#'  default, then likelihood ratio test based on maximum likelihood
#'  estimation will be used. For "ur", `"satorra.2000"` is
#'  automatically used if a scaled test statistic is requested
#'  in `sem_out`.
#'
#' @param sf_full A list with the scaling and shift factors. Ignored
#'  if `robust` is `"none"`. If `robust` is `"satorra.2000"` and
#'  `sf_full` is supplied, then its value will be used. If `robust` is
#'  `"satorra.2000"` but `sf_full` is `NA`, then scaling factors will
#'  be computed internally.
#'
#' @param sf_args The list of arguments to be used for computing scaling factors
#'  if `robust` is `"satorra.2000"`. Used only by [semlbci()]. Ignored
#'  if `robust` is not `"satorra.2000"`.
#'
#' @param sem_out_name The name of the object supplied to `sem_out`. `NULL`
#'  by default. Originally used by some internal functions. No longer used
#'  in the current version but kept for backward compatibility.
#'
#' @param try_k_more_times How many more times to try if the status
#'  code is not zero. Default is 0.
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
#' # The method "wn" needs the constraint function.
#' # Use set_constraint() to generate this function:
#' fn_constr0 <- set_constraint(fit_med)
#'
#' # Call ci_i to find the bound, the lower bound in this example.
#' # The constraint function, assigned to f_constr, is passed
#' # to ci_bound_wn_i().
#' # npar is an argument for ci_bound_wn_i().
#' out <- ci_i_one(i = 1,
#'                 which = "lbound",
#'                 sem_out = fit_med,
#'                 npar = 5,
#'                 f_constr = fn_constr0)
#' out$bounds
#'
#'@export

ci_i_one <- function(i,
                     which = NULL,
                     sem_out,
                     method = c("wn", "ur"),
                     standardized = FALSE,
                     robust = "none",
                     sf_full = NA,
                     sf_args = list(),
                     sem_out_name = NULL,
                     try_k_more_times = 0,
                     ...) {
    method <- match.arg(method)
    # It should be the job of the calling function to check whether it is
    # appropriate to use the robust method.
    if (tolower(robust) == "satorra.2000") {
        # Robust LBCI
        if (all(is.na(sf_full))) {
            # Compute the scaling and shift factors
            # sem_out_name no longer used but kept here for
            # backward compatibility.
            if (is.null(sem_out_name)) {
                sem_out_name <- deparse(substitute(sem_out))
              }
            sf_args_final <- utils::modifyList(sf_args,
                                    list(sem_out = sem_out,
                                         i = i,
                                         standardized = standardized,
                                         std_method = "internal",
                                         sem_out_name = sem_out_name))
            sf_full <- do.call(scaling_factor3, sf_args_final)
          }
        # Use caller-supplied scaling and shift factors
        sf <- sf_full$c_r
        sf2 <- sf_full$c_rb
      } else {
        # Normal LBCI
        sf_full <- NA
        sf <- 1
        sf2 <- 0
      }

    if (method == "wn") {
        b_out <- ci_i_one_wn(i = i,
                             which = which,
                             sem_out = sem_out,
                             standardized = standardized,
                             sf = sf,
                             sf2 = sf2,
                             try_k_more_times = try_k_more_times,
                             ...)
      }
    if (method == "ur") {
        # satorra.2000 is turned on automatically for ur
        b_out <- ci_i_one_ur(i = i,
                             which = which,
                             sem_out = sem_out,
                             standardized = standardized,
                             try_k_more_times = try_k_more_times,
                             ...)
      }
    if (method == "nm") {
        stop("The method 'nm' is no longer supported.")
      }

    b <- b_out$b
    b_time <- b_out$b_time
    attempt_lb_var <- b_out$attempt_lb_var
    attempt_more_times <- b_out$attempt_more_times

    # MAY-FIX:
    # Should not name the elements based on the bound (lower/upper).
    # But this is not an issue for now.
    if (which == "lbound") {
        out <- list(bounds = c(lbound = b$bound),
                    diags = list(lb_diag = b$diag),
                    method = method,
                    times = list(lb_time = b_time[3]),
                    sf_full = sf_full,
                    ci_bound_i_out = list(lb_out = b),
                    attempt_lb_var = attempt_lb_var,
                    attempt_more_times = attempt_more_times)
      }
    if (which == "ubound") {
        out <- list(bounds = c(ubound = b$bound),
                    diags = list(ub_diag = b$diag),
                    method = method,
                    times = list(ub_time = b_time[3]),
                    sf_full = sf_full,
                    ci_bound_i_out =  list(ub_out = b),
                    attempt_lb_var = attempt_lb_var,
                    attempt_more_times = attempt_more_times)
      }
    out
  }

#' @noRd

ci_i_one_wn <- function(i,
                        which = c("lbound", "ubound"),
                        sem_out,
                        standardized = FALSE,
                        sf,
                        sf2,
                        try_k_more_times = 0,
                        ...) {
    # Wu-Neale-2012 method.
    # The only method supported for now.
    ## Attempt 1
    wald_ci_start <- !standardized
    std_method_i <- "internal"
    b_time <- system.time(b <- try(suppressWarnings(ci_bound_wn_i(i,
                                                sem_out = sem_out,
                                                which = which,
                                                standardized = standardized,
                                                sf = sf,
                                                sf2 = sf2,
                                                std_method = std_method_i,
                                                wald_ci_start = wald_ci_start,
                                                ...)), silent = TRUE))
    ## If "internal" failed, switches to "lavaan".
    if (inherits(b, "try-error")) {
        std_method_i <- "lavaan"
        b_time <- b_time + system.time(b <- suppressWarnings(ci_bound_wn_i(i,
                                                  sem_out = sem_out,
                                                  which = which,
                                                  standardized = standardized,
                                                  sf = sf,
                                                  sf2 = sf2,
                                                  std_method = std_method_i,
                                                    ...)))
      }
    attempt_lb_var <- 0
    # Attempt 2
    if (b$diag$status != 0) {
        ## Successively reduce the positive lower bounds for free variances
        lb_se_k0 <- 10
        lb_prop0 <- .11
        lb_prop1 <- .01
        lb_propi <- lb_prop0
        while ((lb_propi > lb_prop1) & (b$diag$status != 0)) {
          attempt_lb_var <- attempt_lb_var + 1
          lb_propi <- lb_propi - .01
          b_time <- b_time + system.time(b <- suppressWarnings(ci_bound_wn_i(i,
                                                    sem_out = sem_out,
                                                    which = which,
                                                    standardized = standardized,
                                                    sf = sf,
                                                    sf2 = sf2,
                                                    std_method = std_method_i,
                                                    lb_prop = lb_propi,
                                                    lb_se_k = lb_se_k0,
                                                    ...)))
            }
      }
    # Attempt 3
    attempt_more_times <- 0
    if (b$diag$status != 0) {
        # Try k more times
        # Successively change the tolerance for convergence
        ki <- try_k_more_times
        fxi <- 1
        fti <- 1
        while ((ki > 0) && (b$diag$status != 0)) {
            attempt_more_times <- attempt_more_times + 1
            ki <- ki - 1
            fxi <- fxi * .1
            if (ki > 0) {
                fti <- fti * .1
              } else {
                # Try hard in the last attempt
                fti <- 0
              }
            b_time <- b_time + system.time(b <- suppressWarnings(ci_bound_wn_i(i,
                                                      sem_out = sem_out,
                                                      which = which,
                                                      standardized = standardized,
                                                      sf = sf,
                                                      sf2 = sf2,
                                                      std_method = std_method_i,
                                                      xtol_rel_factor = fxi,
                                                      ftol_rel_factor = fti,
                                                      lb_prop = lb_propi,
                                                      lb_se_k = lb_se_k0,
                                                        ...)))
          }
      }
    out <- list(b = b,
                b_time = b_time,
                attempt_lb_var = attempt_lb_var,
                attempt_more_times = attempt_more_times)
    return(out)
  }

#' @noRd

ci_i_one_ur <- function(i,
                        which = c("lbound", "ubound"),
                        sem_out,
                        standardized = FALSE,
                        sf,
                        sf2,
                        try_k_more_times = 0,
                        ...) {
    # Root finding by uniroot
    ## Rarely need to try more than once.
    std_method_i <- "internal"
    b_time <- system.time(b <- try(suppressWarnings(ci_bound_ur_i(i,
                                                sem_out = sem_out,
                                                which = which,
                                                standardized = standardized,
                                                sf = sf,
                                                sf2 = sf2,
                                                std_method = std_method_i,
                                                ...)), silent = TRUE))
    ## If "internal" failed, switches to "lavaan".
    if (inherits(b, "try-error")) {
        std_method_i <- "lavaan"
        b_time <- b_time + system.time(b <- suppressWarnings(ci_bound_ur_i(i,
                                                  sem_out = sem_out,
                                                  which = which,
                                                  standardized = standardized,
                                                  sf = sf,
                                                  sf2 = sf2,
                                                  std_method = std_method_i,
                                                    ...)))
      }
    attempt_lb_var <- 0
    attempt_more_times <- 0
    out <- list(b = b,
                b_time = b_time,
                attempt_lb_var = attempt_lb_var,
                attempt_more_times = attempt_more_times)
    return(out)
  }

