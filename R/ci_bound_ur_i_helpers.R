
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

#' @noRd
# Generate a function to extract a parameter
# To be used by [ci_bound_ur_i()].
# CHECKED: Reverted to the experimental version
gen_est_i <- function(i,
                      sem_out,
                      standardized = FALSE) {
    ptable <- lavaan::parameterTable(sem_out)
    # i_user <- (ptable$free > 0) | (ptable$user > 0)
    # i_est <- which(which(i_user) == i)
    # From the help page of lavaan-class object,
    # type = "user" should return *all* parameters
    # in the parameter table, including equality constraints.
    # Therefore, the row number should be equal to the order
    # in the vector of coefficients.
    i_user <- i
    i_est <- i
    is_def <- (ptable$op[i] == ":=")
    i_lhs <- (ptable$lhs[i])
    i_rhs <- (ptable$rhs[i])
    i_op <- ptable$op[i]
    i_gp <- ptable$group[i]
    if (standardized) {
        if (is_def) {
            out <- function(object) {
                # Just in case
                force(i_est)
                std <- lavaan::standardizedSolution(object,
                                est = lavaan::lav_model_get_parameters(object@Model, type = "user"),
                                GLIST = object@Model@GLIST,
                                se = FALSE,
                                zstat = FALSE,
                                pvalue = FALSE,
                                ci = FALSE,
                                remove.eq = FALSE,
                                remove.ineq = FALSE,
                                remove.def = FALSE,
                                type = "std.all")
                unname(std[i_est, "est.std"])
              }
          } else {
            out <- function(object) {
                # Just in case
                force(i_est)
                force(i_gp)
                force(i_op)
                force(i_lhs)
                force(i_rhs)
                object@implied <- lavaan::lav_model_implied(object@Model)
                est <- lavaan::lav_model_get_parameters(object@Model, type = "user")[i_est]
                implied <- lavaan::lavInspect(object, "cov.all", drop.list.single.group = FALSE)
                implied_sd <- sqrt(diag(implied[[i_gp]]))
                est1 <- switch(i_op,
                               `~~` = est / (implied_sd[i_lhs] * implied_sd[i_rhs]),
                               `~` = est * implied_sd[i_rhs] / implied_sd[i_lhs],
                               `=~` = est * implied_sd[i_lhs] / implied_sd[i_rhs])
                unname(est1)
              }
          }
      } else {
        out <- function(object) {
            force(i_est)
            unname(lavaan::lav_model_get_parameters(object@Model, type = "user")[i_est])
          }
      }
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
                              global_ok = FALSE) {
    on.exit(try(r1$close(), silent = TRUE))

    userp <- object$userp
    userp_name <- object$userp_name

    # This option is disabled to avoid
    # storing things to the global
    # environment
    global_ok <- FALSE
    if (global_ok) {
        # Disabled for now
      } else {
        r1 <- callr::r_session$new()
        out <- r1$run(function(target,
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
                     fix = TRUE) {
    on.exit(try(r1$close(), silent = TRUE))
    userp <- gen_userp(func = func,
                       sem_out = sem_out)
    # Create a child process
    r1 <- callr::r_session$new()
    fit_i <- r1$run(function(...,
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

#' @title Generate a Function to Fix a
#' User-Defined Parameter
#'
#' @description Add a function to a
#' `lavaan` object as a user-defined
#' parameter and return a function which
#' can fix this user-defined parameter
#' to a value.
#'
#' @details
#' This function is to be used internally
#' for generating a function for searching
#' a likelihood-based confidence bound.
#'
#' It is exported because it needs to
#' be run in an external R process,
#' usually created by `callr` in other
#' internal functions.
#'
#' @param userp A function that receives
#' a `lavaan` object and returns a
#' scalar.
#'
#' @param userp_name The name of the
#' function `userp` to be used in the
#' `lavaan` model. It does not have to
#' be the name of the function in
#' `userp`. Should be changed only if it
#' conflicts with another object in the
#' parent environment, which should not
#' happen if the model is always fitted
#' in a clean R session.
#'
#' @param sem_out A `lavaan`-class object,
#' to which the function will be added
#' as a user-defined parameter.
#'
#' @param fix If `TRUE`, the default,
#' the function generated is used to fix
#' the value of `userp` to a target
#' value using an equality constraint.
#' If `FALSE`, then the function simply
#' fits the model to the data.
#'
#' @param control_args To be passed to
#' the argument of the same name in
#' [lavaan::lavaan()]. Default is
#' `list()`. Can be used to set the
#' default values of this argument
#' in the generated function.
#'
#' @param iter.max The maximum number of
#' iteration when the generated function
#' fit the model. Default is 10000.
#'
#' @param max_attempts If the initial
#' fit with the equality constraint
#' fails, how many more attempts
#' will be made by the generated
#' function. Default is 5.
#'
#' @return
#' If `fix` is `TRUE`, it returns a
#' function with these arguments:
#'
#' - `target`: The value to which the
#' user-defined parameter will be fixed
#' to.
#'
#' - `verbose`: If `TRUE`, additional
#' information will be printed when
#' fitting the model.
#'
#' - `control`: The values to be passed
#' as a list to the argument of the
#' same name in [lavaan::lavaan()].
#'
#' - `seed`: Numeric. If supplied, it
#' will be used in [set.seed()] to
#' initialize the random number
#' generator. Necessary to reproduce
#' some results because random numbers
#' are used in some steps in `lavaan`.
#' If `NULL`, the default, [set.seed()]
#' will not be called.
#'
#' If `fix` is `FALSE, then it returns a
#' function with optional arguments that
#' will be ignored, Calling it will
#' simply fit the modified model to the
#' data. Useful for getting the value of
#' the user-defined parameter.
#'
#'
#' @examples
#'
#' # TODO:
#' # - Add an example
#' data(simple_med)
#' dat <- simple_med
#'
#'
#' @export

gen_sem_out_userp <- function(userp,
                              sem_out,
                              userp_name = "semlbciuserp1234",
                              fix = TRUE,
                              control_args = list(),
                              iter.max = 10000,
                              max_attempts = 5) {

    # Generate a function that:
    # - refits the model with the user-parameter fixed to a target value.
    # CHECKED: Identical to the experimental version
    # For add_fun() using callr.

    iter.max <- max(lavaan::lavInspect(sem_out, "optim")$iterations,
                    iter.max)
    ptable <- lavaan::parameterTable(sem_out)
    # Create a unique label: userp_label
    labels <- unique(ptable$label)
    labels <- labels[nchar(labels) > 0]
    userp_label <- make.unique(c(labels, "user"))
    userp_label <- userp_label[length(userp_label)]
    # Generate the user parameter row using userp()
    user_pt1 <- data.frame(lhs = userp_label[length(userp_label)],
                           op = ":=",
                           rhs = paste0(userp_name, "()"),
                           user = 1,
                           block = 0,
                           group = 0,
                           free = 0,
                           ustart = NA,
                           label = userp_label,
                           exo = 0)
    user_pt1 <- lavaan::lav_partable_complete(partable = user_pt1,
                                              start = TRUE)
    # Add the user parameter to the model
    ptable1 <- lavaan::lav_partable_add(partable = ptable,
                                        add = user_pt1)
    ptable1$se[which(ptable1$label == userp_label)] <- NA

    # Extract slots to be used
    slot_opt <- sem_out@Options
    slot_dat <- sem_out@Data

    if (fix) {
        # Create the row of equality constraint
        user_pt2 <- data.frame(lhs = userp_label,
                               op = "==",
                               rhs = NA,
                               user = 1,
                               block = 0,
                               group = 0,
                               free = 0,
                               ustart = NA,
                               exo = 0)
        user_pt2 <- lavaan::lav_partable_complete(partable = user_pt2,
                                                  start = TRUE)
        # Add the equality constraint to the parameter tahle
        ptable2 <- lavaan::lav_partable_add(partable = ptable1,
                                            add = user_pt2)
        # Store the position of the constraint
        i_eq <- which((ptable2$lhs == userp_label) &
                      (ptable2$op == "=="))

        # Fix the ids of the rows
        ptable2$id <- seq_along(ptable2$id)

        # Set the options
        slot_opt$do.fit <- TRUE
        # The "mplus" version does not take into account the user-parameter
        slot_opt$start <- "simple"
        # Heywood cases are allowed
        slot_opt$check.start <- FALSE
        slot_opt$check.post <- FALSE
        # Disable some options for efficiency
        slot_opt$se <- "none"
        # slot_opt$h1 <- FALSE
        # slot_opt$baseline <- FALSE

        out <- function(target,
                        verbose = FALSE,
                        control = list(),
                        seed = NULL) {
            if (!is.null(seed)) set.seed(seed)
            # Just in case ...
            force(control_args)
            force(slot_opt)
            force(slot_dat)
            force(ptable2)
            force(userp_label)
            force(i_eq)

            ptable3 <- ptable2
            ptable3$rhs[i_eq]  <- target
            control_args <- utils::modifyList(control,
                                              list(iter.max = iter.max))
            slot_opt$control <- control_args

            # Fit once to update the model slot
            attempted <- 0
            fit_new_converged <- FALSE
            if (verbose) {
                cat("Target:", target, "\n")
              }
            if (verbose) {
                cat("First attempt ...\n")
              }
            # Fail when
            # eq.constraints.K is 0x0
            # Succeed when
            # fit_new@Model@eq.constraints, or
            # length(fit_new@Model@ceq.nonlinear.idx) > 0
            slot_opt_tmp <- slot_opt
            slot_opt_tmp$do.fit <- FALSE
            eq_not_ok <- TRUE
            while (eq_not_ok) {
                fit_new <- lavaan::lavaan(model = ptable3,
                                          slotOptions = slot_opt_tmp,
                                          slotData = slot_dat)
                eq_not_ok <- !fit_new@Model@eq.constraints &&
                             !(length(fit_new@Model@ceq.nonlinear.idx) > 0)
              }

            # Extract the updated slots
            slot_pat_new <- fit_new@ParTable
            slot_model_new <- fit_new@Model
            slot_smp_new <- fit_new@SampleStats

            # Fit the model with the user parameter constrained
            # to the target value
            fit_new <- tryCatch(lavaan::lavaan(slotParTable = slot_pat_new,
                                               slotModel = slot_model_new,
                                               slotSampleStats = slot_smp_new,
                                               slotOptions = slot_opt,
                                               slotData = slot_dat),
                                error = function(e) e)
            if (inherits(fit_new, "error")) {
                stop("Something's with the function.",
                     "Error occurred when fitting the modified model.")
              }

            fit_new_converged <- lavaan::lavInspect(fit_new, "converged")

            # Try max_attempts more times
            if (!fit_new_converged) {
                if (verbose) {
                    cat("More attempts ...\n")
                  }
                while (!((attempted > max_attempts) ||
                          fit_new_converged)) {
                    if (verbose) {cat("Attempt", attempted, "\n")}
                    ptable_new <- fit_new@ParTable
                    i_free <- ptable_new$free > 0
                    ptable_new$est[i_free] <- jitter(ptable_new$est[i_free],
                                                     factor = 50)
                    slot_opt$start <- as.data.frame(ptable_new)
                    fit_old <- fit_new
                    fit_new <- tryCatch(lavaan::lavaan(slotParTable = ptable_new,
                                                       slotModel = slot_model_new,
                                                       slotSampleStats = slot_smp_new,
                                                       slotOptions = slot_opt,
                                                       slotData = slot_dat),
                                        error = function(e) e)
                    if (inherits(fit_new, "error")) {
                        fit_new <- fit_old
                        fit_new_converged <- FALSE
                      } else {
                        fit_new_converged <- lavaan::lavInspect(fit_new, "converged")
                      }
                    attempted <- attempted + 1
                  }
              }
            if (!fit_new_converged) {
                if (verbose) {
                    cat("The modified model failed to converge.")
                  }
              }
            if (verbose) {
                cat("Done!\n")
              }
            fit_new
          }
      } else {
        # The arguments are not used in this version
        out <- function(target,
                        verbose = FALSE,
                        control = list(),
                        seed = NULL) {
            # Generate a model with the user parameter added.
            # For debugging
            # Just in case ...
            force(slot_opt)
            force(slot_dat)
            force(ptable1)
            slot_opt$do.fit <- TRUE
            fit_new <- lavaan::lavaan(model = ptable1,
                                      slotOptions = slot_opt,
                                      slotData = slot_dat)
            fit_new
          }
      }
    out
  }



#' @param func A function that receives a
#' `lavaan`-object and returns a scalar.
#'
#' @param sem_out A `lavaan`-class object.
#'
#' @return
#' `gen_userp()` returns a function that
#' accepts a numeric vector of length
#' equals to the number of free parameters
#' in `sem_out`, and returns a scalar
#' which is the output of `func`.
#'
#' @noRd
# CHECKED: Identical to the experimental version
gen_userp <- function(func,
                      sem_out) {
    stopifnot(is.function(func))
    stopifnot(inherits(sem_out, "lavaan"))
    out <- function(.x., ...) {
        if (missing(.x.)) {
            .x. <- get(".x.", parent.frame())
          }
        # Just in case ...
        force(sem_out)

        sem_out@Model <- lavaan::lav_model_set_parameters(sem_out@Model, .x.)
        sem_out@implied <- lavaan::lav_model_implied(sem_out@Model)
        f0 <- func(sem_out)
        if (is.na(f0) || is.nan(f0)) {
            f0 <- Inf
          }
        f0
      }
    out
  }