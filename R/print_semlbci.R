#' @title Print Method of a 'semlbci' Object
#'
#' @description Prints the results of a `semlbci` object, the output
#'  of [semlbci()].
#'
#' @details Prints the results of [semlbci()] as a table.
#'
#' @return
#' `x` is returned invisibly. Called for its side effect.
#'
#' @param x The output of [semlbci()].
#'
#' @param digits The number of digits after the decimal point. To be
#'  passed to [formatC()]. Default is 3.
#'
#' @param time If `TRUE`, print the time spent on each bound. Default
#'  is `FALSE`.
#'
#' @param annotation If `TRUE`, print table notes. Default is `TRUE`.
#'
#' @param verbose If `TRUE`, additional diagnostic information will
#'  always be printed. This argument overrides `verbose_if_needed`.
#'  Default is `FALSE`.
#'
#' @param verbose_if_needed If `TRUE`, additional diagnostic
#'  information will be printed only if necessary. If `FALSE`,
#'  additional diagnostic information will always be printed. Default
#'  is `TRUE`.
#'
#' @param drop_no_lbci If `TRUE`, parameters without LBCIs will be
#' removed. Default is `TRUE`.
#'
#' @param ... Other arguments. They will be ignored.
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
#' print(lbci_med, verbose_if_needed = FALSE)
#'
#' print(lbci_med, verbose = TRUE)
#'
#' print(lbci_med, time = TRUE)
#'
#' print(lbci_med, annotation = FALSE)
#'
#' print(lbci_med, digits = 4)
#'
#' @export

print.semlbci <- function(x,
                          digits = 3,
                          annotation = TRUE,
                          time = FALSE,
                          verbose = FALSE,
                          verbose_if_needed = TRUE,
                          drop_no_lbci = TRUE,
                          ...) {
    if (verbose) verbose_if_needed <- FALSE
    out <- x
    if (max(out$group) == 1) {
        out$group <- NULL
      }
    if (all(out$label == "")) {
        out$label <- NULL
      }
    class(out) <- "data.frame"
    out$lbci_lb <- formatC(out$lbci_lb, digits, format = "f")
    if ("est.std" %in% colnames(x)) {
        standardized <- TRUE
        out$est.std <- formatC(out$est.std, digits, format = "f")
      } else {
        standardized <- FALSE
        out$est <- formatC(out$est, digits, format = "f")
      }
    out$lbci_ub <- formatC(out$lbci_ub, digits, format = "f")
    out$ci_org_lb <- formatC(out$ci_org_lb, digits, format = "f")
    out$ci_org_ub <- formatC(out$ci_org_ub, digits, format = "f")
    out$ratio_lb <- formatC(out$ratio_lb, digits, format = "f")
    out$ratio_ub <- formatC(out$ratio_ub, digits, format = "f")
    out$cl_lb <- formatC(out$cl_lb, digits, format = "f")
    out$cl_ub <- formatC(out$cl_ub, digits, format = "f")
    call_org <- attr(out, "call")
    if (drop_no_lbci) {
        out <- out[!is.na(out$status_lb), ]
      }
    if (!time) {
        out$time_lb <- NULL
        out$time_ub <- NULL
      }
    if (verbose_if_needed) {

        if (is.null(call_org$ci_limit_ratio_tol)) {
            ci_limit_ratio_tol <- formals(ci_bound_wn_i)$ci_limit_ratio_tol
          } else {
            ci_limit_ratio_tol <- call_org$ci_limit_ratio_tol
          }
        ratio_x <- c(out$ratio_lb, out$ratio_ub)
        ratio_note <- any(ratio_x > ci_limit_ratio_tol, na.rm = TRUE) |
                      any(ratio_x < 1 / ci_limit_ratio_tol, na.rm = TRUE)
        if (!ratio_note) {
            out$ratio_lb <- NULL
            out$ratio_ub <- NULL
          }

        post_check_note <- !all(out$post_check_lb,
                                out$post_check_ub,
                                na.rm = TRUE)
        if (!post_check_note) {
            out$post_check_lb <- NULL
            out$post_check_ub <- NULL
          }

        status_note <- !all(out$post_check_lb == 0,
                            out$post_check_ub == 0,
                            na.rm = TRUE)
        if (!status_note) {
            out$status_lb <- NULL
            out$status_ub <- NULL
          }

      } else {
        ratio_note <- TRUE
        post_check_note <- TRUE
        status_note <- TRUE
      }

    out$method <- NULL

    out_names <- colnames(out)
    out_names <- gsub("ci_org_lb", "lb", out_names, fixed = TRUE)
    out_names <- gsub("ci_org_ub", "ub", out_names, fixed = TRUE)
    out_names <- gsub("status_lb", "ok_l", out_names, fixed = TRUE)
    out_names <- gsub("status_ub", "ok_u", out_names, fixed = TRUE)
    out_names <- gsub("ratio_lb", "ratio_l", out_names, fixed = TRUE)
    out_names <- gsub("ratio_ub", "ratio_u", out_names, fixed = TRUE)
    out_names <- gsub("post_check_lb", "check_l", out_names, fixed = TRUE)
    out_names <- gsub("post_check_ub", "check_u", out_names, fixed = TRUE)
    out_names <- gsub("time_lb", "sec_l", out_names, fixed = TRUE)
    out_names <- gsub("time_ub", "sec_u", out_names, fixed = TRUE)

    colnames(out) <- out_names

    cat("\nResults:\n")
    print(out)

    if (annotation) {
        msg <- NULL
        msg <- c(msg,
              "* lbci_lb, lbci_ub: The lower and upper likelihood-based bounds.")
        if (standardized) {
            msg <- c(msg,
                  paste0("* est.std: ",
                      "The point estimates from the original lavaan",
                      " standardized solution."))
          } else {
            msg <- c(msg,
                  paste0("* est: ",
                      "The point estimates from the original lavaan",
                      " output."))
          }
        if (status_note) {
          msg <- c(msg,
            paste0("* ok_l, ok_u: Whether the search encountered any problem. ",
                  "If no problem encountered, it is equal to 0. Any value ",
                  "other than 0 indicates something was wrong in the search."))
          }
        if (standardized) {
            msg <- c(msg,
                  paste0("* lb, ub: ",
                  "The original lower and upper bounds,",
                      " extracted from the original lavaan standardized",
                      " solution output.",
                      " Usually the delta method CIs for the",
                      " standardized parameters and user-defined parameters."))
          } else {
            msg <- c(msg,
                  paste0("* lb, ub: ",
                  "The original lower and upper bounds,",
                      " extracted from the original lavaan output.",
                      " Usually Wald CIs for free parameters and",
                      " delta method CIs for user-defined parameters"))
          }
        msg <- c(msg,
              paste0("* cl_lb, cl_ub: ",
              "One minus the p-values of chi-square difference tests",
                   " at the bounds.",
                   " Should be close to the requested level of confidence,",
                   " e.g., .95 for 95% confidence intervals."))
        if (ratio_note) {
          msg <- c(msg,
              paste0("* ratio_l, ratio_u: ",
                  "Ratio of a to b, ",
                  "a = Distance from the point estimate to the likelihood-based",
                  " bound, b = Distance from the point estimate to the original",
                  " bound. A bound should be interpreted with caution if",
                  " the ratio is too large or too small, indicating a large",
                  " difference between the original interval and the",
                  " likelihood-based interval."))
          }
        if (post_check_note) {
          msg <- c(msg,
            paste0("* check_l, check_u: ", "Whether the final solution of ",
                  "a bound passed the post optimization check of lavaan ",
                  "by lavaan::lavInspect(fit, 'post.check'), where ",
                  "fit is the final solution."))
          }
        if ("sec_l" %in% colnames(out)) {
          msg <- c(msg,
            "* sec_l, sec_u: The time (in seconds) used to search a bound.")
          }
        if ("robust" %in% colnames(out)) {
          msg <- c(msg,
              paste0("* robust: ",
              "The robust method used in the likelihood ratio ",
                     "test of lavaan in searching the robust ",
                     "likelihood-based bounds."))
          }
        if ("method" %in% colnames(out)) {
          msg <- c(msg,
              "* method: The method used to search the bounds.")
          }
        cat("\nAnnotation:\n")
        cat(strwrap(msg, exdent = 4), sep = "\n")
        cat("\nCall:\n")
        print(call_org)
      }
    invisible(x)
  }
