#' @title Print Method of a 'cibound'-class Object
#'
#' @description Print the diagnostic information of a `cibound`-class
#'  object.
#'
#' @details This is the print method for the output of
#'  [ci_bound_wn_i()], a `cibound`-class object. It prints the
#'  diagnostic information on the bound being found and the search
#'  process.
#'
#' @return
#' `x` is returned invisibly. Called for its side effect.
#'
#' @param x The output of a `ci_bound_xx_i` function. Currently the
#'  only such function is [ci_bound_wn_i()].
#'
#' @param digits The number of digits after the decimal point. To be
#'  passed to [round()]. Default is 5.
#'
#' @param ... Other arguments. They will be ignored.
#'
#' @examples
#' data(simple_med)
#' dat <- simple_med
#'
#' mod <-
#' "
#' m ~ x
#' y ~ m
#' "
#'
#' fit_med <- lavaan::sem(mod, simple_med, fixed.x = FALSE)
#'
#' fn_constr0 <- set_constraint(fit_med)
#'
#' out1l <- ci_bound_wn_i(i = 1,
#'                        npar = 5,
#'                        sem_out = fit_med,
#'                        f_constr = fn_constr0,
#'                        which = "lbound")
#'
#' # Print the output
#' out1l
#'
#' @export

print.cibound <- function(x, digits = 5, ...) {
    call_org <- x$call
    out_diag <- x$diag
    ci_method <- x$method_name
    if (is.null(out_diag$standardized)) {
        std <- "No"
      } else {
        std <- ifelse(out_diag$standardized, "Yes", "No")
      }
    lor <- out_diag$i_lor
    lor2 <- paste0(lor$lhs, " ", lor$op, " ", lor$rhs, " (group = ",
                   lor$group, ", block = ", lor$block, ")")
    cat(paste0(  "Target Parameter:       ", lor2))
    cat(paste0("\nPosition:               ", out_diag$i))
    cat(paste0("\nWhich Bound:            ", switch(out_diag$which,
                                            lbound = "Lower Bound",
                                            ubound = "Upper Bound")))
    cat(paste0("\nMethod:                 ", ci_method))
    cat(paste0("\nConfidence Level:       ", out_diag$ciperc))
    if (!is.null(out_diag$search_error)) {
    cat(paste0("\nSearch Error Message:\n", out_diag$search_error, "\n"))
    cat(paste0(rep("*", round(getOption("width") * .8)), collapse = ""))
    cat("\n")
    cat(strwrap(c("The search was terminated due to the error above.",
                  "The following lines should not be interpreted.")),
        sep = "\n")
    cat(paste0(rep("*", round(getOption("width") * .8)), collapse = ""))
    cat("\n")
    }
    cat(paste0("\nAchieved Level:         ", out_diag$ciperc_final))
    cat(paste0("\nStandardized:           ", std))
    cat(paste0("\nLikelihood-Based Bound: ",
                  ifelse(is.na(x$bound),
                         "Not valid",
                         round(x$bound, digits))))
    cat(paste0("\nWald Bound:             ", round(out_diag$ci_org_limit, digits)))
    cat(paste0("\nPoint Estimate:         ", round(out_diag$est_org, digits)))
    cat(paste0("\nRatio to Wald Bound:    ", ifelse(is.na(x$bound), "Not valid",
                  round(out_diag$ci_limit_ratio, digits))))
    cat(paste0("\n\n-- Check --"))
    # Get p_tol in call
    p_tol_call <- call_org$p_tol
    if (is.null(p_tol_call)) {
        ci_method <- as.character(call_org[[1]])
        # ci_method can be of the form xxx::yyy
        ci_method <- ci_method[length(ci_method)]
        p_tol_call <- formals(ci_method)$p_tol
      }
    ciperc_diff <- abs(out_diag$ciperc - out_diag$ciperc_final)
    cat(paste0("\nLevel achieved?         ",
            ifelse(ciperc_diff <= p_tol_call, "Yes", "No"),
                   " (Difference: ",
                   formatC(ciperc_diff, digits = digits), ";",
                   " Tolerance: ", p_tol_call, ")"))
    cat(paste0("\nSolution admissible?    ",
            ifelse(out_diag$fit_post_check, "Yes", "No")))
    bound_unchecked <- out_diag$bound_unchecked
    if (is.na(bound_unchecked)) {
        direct_valid <- "Not valid"
      } else {
        direct_valid <- switch(out_diag$which,
                          lbound = ifelse(bound_unchecked < out_diag$est_org,
                                          "Yes", "No"),
                          ubound = ifelse(bound_unchecked > out_diag$est_org,
                                          "Yes", "No")
                        )
      }
    cat(paste0("\nDirection valid?        ", direct_valid))
    cat("\n")

    cat("\n-- Optimization Information --")
    cat(paste0("\nSolver Status:          ", out_diag$optim_status))
    cat(paste0("\nConvergence Message:    ", out_diag$optim_message))
    if (any(grepl("NLOPT_MAXTIME_REACHED: ", out_diag$optim_message))) {
        cat("\n - Set 'timeout' to a larger value to increase maximum time.")
      }
    cat(paste0("\nIterations:             ", out_diag$optim_iterations))
    t_cond <- out_diag$optim_termination_conditions
    t_cond2 <- strsplit(t_cond, "\t")[[1]]
    t_cond2 <- gsub("maxeval: ", "maxeval:  ", t_cond2)
    tmp <- paste0(sapply(t_cond2, function(x) paste0("\t", x)), collapse = "\n")
    cat("\nTermination Conditions:\n")
    cat(tmp)
    coef_final <- out_diag$final_values
    coef_c <- rbind(out_diag$start_values,
                    coef_final,
                    coef_final - out_diag$start_values)
    coef_c <- round(coef_c, digits)
    rownames(coef_c) <- c("Start", "Final", "Change")
    cat("\n")
    cat("\n-- Parameter Estimates --\n")
    print(coef_c)

    cat(paste0("\nBound before check:     ",
                round(out_diag$bound_unchecked, digits)))
    cat(paste0("\nStatus Code:            ", out_diag$status))
    cat("\nCall: ")
    call_print <- call_org
    if (!is.name(call_print$f_constr)) {
        call_print$f_constr <- "<not printed>"
      }
    print(call_print)
    cat("\n")
    invisible(x)
  }
