
#' @title Initial Interval for the
#' Search by 'ci_bound_ur()'
#'
#' @description Determine the interval to
#' be used by [ci_bound_ur()].
#'
#' @details
#' Used when the parameter is the output
#' of a function a lavaan object, for
#' which it is not easy to get the Wald
#' or delta method confidence interval.
#'
#' It fits the model to the data in
#' `sem_out` and gets the confidence
#' interval of the function value of
#' `func`. The distance from the target
#' confidence bound (determiend by
#' `which`) to the estimate is computed,
#' divided by `d`. The confidence bound
#' plus and minus this value is the
#' interval to be used.
#'
#' @param sem_out The fit object.
#' Currently supports
#' [lavaan::lavaan-class] objects only.
#'
#' @param func A function that receives
#' a lavaan object and returns a scalar.
#' Usually the output of
#' [gen_sem_out_userp()].
#'
#' @param which Whether the lower bound
#' or the upper bound is to be found.
#' Must be `"lbound"` or `"ubound"`.
#'
#' @param d A value used to determine
#' the width of the interval in the
#' initial search. Larger this value,
#' *narrow* the interval. Default is 5.
#'
#' @param level The level of confidence
#' of the confidence interval. Default
#' is .95, or 95%.
#'
#' @noRd
# CHECKED: The only change is the level argument
uniroot_interval <- function(sem_out,
                             func,
                             which,
                             d = 5,
                             level = .95) {
    # Set the interval to search

    # NOTE:
    # No need to know standardized or not because
    # standardization, if any, is done inside func.

    fit_i_free <- add_func(func = func,
                           sem_out = sem_out,
                           fix = FALSE)
    fit_start <- sem_out_userp_run(target = 0,
                                   object = fit_i_free)
    est_start <- lavaan::parameterEstimates(fit_start,
                                            ci = TRUE,
                                            level = level)
    i <- which(est_start$rhs == paste0(fit_i_free$userp_name, "()"))
    user_est <- est_start[i, "est"]
    bound_start <- est_start[i, switch(which,
                                       lbound = "ci.lower",
                                       ubound = "ci.upper")]
    bound_start_d <- abs(bound_start - user_est)
    i_l <- bound_start - bound_start_d / d
    i_u <- bound_start + bound_start_d / d
    out <- c(i_l, i_u)
    attr(out, "bound_start") <- bound_start
    attr(out, "user_est") <- user_est
    return(out)
  }


#' @title Fix a User Parameter To a
#' Value and Refit a Model
#'
#' @description Fix a user parameter in
#' a lavaan model to a target value,
#' refit the model, and return the
#' results.
#'
#' @details
#' The model must be the output of
#' [add_func()].
#'
#' @param target The value to which the
#' user parameter will be fixed to.
#'
#' @param object The output of
#' [add_func()].
#'
#' @param verbose Whether diagnostic
#' information will be printed. Default
#' is `FALSE`.
#'
#' @param control To be passed to the
#' argument of the same name in
#' [lavaan::lavaan()]. Default is
#' `list()`.
#'
#' @param seed Numeric. If supplied, it
#' will be used in [set.seed()] to
#' initialize the random number
#' generator. Necessary to reproduce
#' some results because random numbers
#' are used in some steps in `lavaan`.
#' If `NULL`, the default, [set.seed()]
#' will not be called.
#'
#' @param global_ok Logical. Whether
#' is the user function can be stored
#' in the global environment. This
#' option is disabled for now because
#' it is not a good practice to change
#' the global environment.
#'
#' @return
#' A `lavaan` object, with the value
#' of the user-defined parameters
#' fixed to the target value.
#'
#' @noRd
# CHECKED: Identical to the experimental version
sem_out_userp_run <- function(target,
                              object,
                              verbose = FALSE,
                              control = list(),
                              seed = NULL,
                              global_ok = FALSE,
                              rs = NULL) {

    userp <- object$userp
    userp_name <- object$userp_name

    # This option is disabled to avoid
    # storing things to the global
    # environment
    global_ok <- FALSE
    if (global_ok) {
        # Disabled for now
      } else {
        if (is.null(rs)) {
            on.exit(try(rs$kill(), silent = TRUE))
            rs <- callr::r_session$new()
          }
        out <- rs$run(function(target,
                               verbose,
                               control,
                               seed,
                               sem_out_userp,
                               userp,
                               userp_name) {
                          assign(userp_name,
                                 value = userp,
                                 envir = parent.frame())
                          do.call(sem_out_userp,
                                  list(target = target,
                                       verbose = verbose,
                                       control = control,
                                       seed = seed))
                        },
                      args = list(target = target,
                                  verbose = verbose,
                                  control = control,
                                  seed = seed,
                                  sem_out_userp = object$sem_out_userp,
                                  userp = userp,
                                  userp_name = userp_name))
      }
    out
  }


#' @title Add a User Function to a 'lavaan'
#' model
#'
#' @description It adds a user function
#' to a `lavaan` model.
#'
#' @details
#'
#' The function is one accepts a
#' `lavaan` object and returns a scalar.
#' An R session will be started to
#' create the model. This is necessary
#' because the function needs to be
#' present in the global environment
#' even if the model is only created
#' but not fitted.
#'
#' @param func A function that accepts
#' a `lavaan` object and returns a
#' scalar.
#'
#' @param sem_out The fit object.
#' Currently supports
#' [lavaan::lavaan-class] objects only.
#'
#' @param userp_name The name of the
#' function (`func`) to be used in the
#' `lavaan` model. Should be changed
#' only if it conflicts with another
#' object in the parent environment,
#' which should not happen if the model
#' is always fitted in a clean R
#' session.
#'
#' @param fix To be passed to
#' [gen_sem_out_userp()]. If `TRUE`, the
#' default, the function generated is
#' used to fix
#' the value of `func` to a target value
#' using an equality constraint. If
#' `FALSE`, then the function simply
#' fits the model to the data.
#'
#' @return
#' A list with the following elements:
#'
#' - `sem_out_userp`: The `lavaan` model
#' with the user function, `func`, added
#' as a user-defined parameter.
#'
#' - `userp`: A copy of `func`.
#'
#' - `userp_name`: The value of
#' `userp_name`, the label of the
#' user-defined parameter in the model.
#'
#' @noRd
# CHECKED: Identical to the experimental version
add_func <- function(func,
                     sem_out,
                     userp_name = "semlbciuserp1234",
                     fix = TRUE,
                     rs = NULL) {
    userp <- gen_userp(func = func,
                       sem_out = sem_out)
    # Create a child process
    if (is.null(rs)) {
        on.exit(try(rs$kill(), silent = TRUE))
        rs <- callr::r_session$new()
      }
    fit_i <- rs$run(function(...,
                             userp,
                             userp_name) {
                      assign(userp_name,
                             value = userp,
                             parent.frame())
                      semlbci::gen_sem_out_userp(...)
                    }, args = list(userp = userp,
                                   sem_out = sem_out,
                                   userp_name = userp_name,
                                   fix = fix))
    list(sem_out_userp = fit_i,
         userp = userp,
         userp_name = userp_name)
  }

