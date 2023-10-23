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

#' @noRd
# A helper for printing in the 'lavaan' way.
# Copied from semhelpinghands::print.std_solution_boot()
# Not ready.
# To be modified.

print_text <- function(x,
                       ...,
                       sem_out = sem_out,
                       nd = 3,
                       output = c("table", "text"),
                       lbci_only = FALSE,
                       drop_no_lbci = FALSE) {
    output <- match.arg(output)
    x_call <- attr(x, "call")
    if (output == "table") {
        NextMethod()
        return(invisible(x))
      }
    ciperc <- x_call$ciperc
    if (is.null(ciperc)) {
        ciperc <- formals(semlbci)$ciperc
      }
    x_df <- as.data.frame(x)
    if ("est.std" %in% colnames(x)) {
        std <- TRUE
      } else {
        std <- FALSE
      }
    est0 <- lavaan::parameterEstimates(sem_out,
                                       standardized = FALSE,
                                       level = ciperc,
                                       output = "text",
                                       header = TRUE)
    est1 <- est0
    est1$id <- seq_len(nrow(est1))
    if (std) {
        # If CIs for standardized solution,
        # always print standardized estimates.
        std1 <- lavaan::standardizedSolution(sem_out,
                                             type = "std.all",
                                             ci = TRUE,
                                             level = ciperc)
        i0 <- colnames(std1) %in% c("lhs", "op", "rhs",
                                    "group", "label",
                                    "est.std",
                                    "se", "z", "pvalue",
                                    "ci.lower", "ci.upper")
        est1$est <- NULL
        est1$se <- NULL
        est1$z <- NULL
        est1$pvalue <- NULL
        est1$ci.lower <- NULL
        est1$ci.upper <- NULL
        est1 <- merge(est1,
                      std1[, i0],
                      all.x = TRUE)
        tmp <- colnames(est1)
        tmp <- gsub("est.std", "est", tmp, fixed = TRUE)
        colnames(est1) <- tmp
      }
    i0 <- colnames(x_df) %in% c("lhs", "op", "rhs",
                                "group", "label",
                                "lbci_lb", "lbci_ub",
                                "status_lb", "status_ub",
                                "ratio_lb", "ratio_ub",
                                "post_check_lb", "post_check_ub",
                                "cl_lb", "cl_ub")
    est1 <- merge(est1,
                  x_df[, i0],
                  all.x = TRUE)
    # i0 <- colnames(ptable) %in% c("est", "se",
    #                               "user", "free",
    #                               "ustart", "plabel",
    #                               "start",
    #                               "id")
    # est1 <- merge(est1, ptable[, !i0])
    est1 <- est1[order(est1$id), ]
    est1$id <- NULL
    est1 <- merge_attributes(est1,
                             est0)
    class(est1) <- c("lavaan.parameterEstimates", class(est1))
    if (drop_no_lbci) {
        i0 <- is.na(est1$post_check_lb) & is.na(est1$post_check_ub)
        est1 <- est1[!i0, ]
      }
    est1$status_lb <- NULL
    est1$status_ub <- NULL
    est1$ratio_lb <- NULL
    est1$ratio_ub <- NULL
    est1$post_check_lb <- NULL
    est1$post_check_ub <- NULL
    est1$cl_lb <- NULL
    est1$cl_ub <- NULL
    if (lbci_only) {
        est1$ci.lower <- NULL
        est1$ci.lower <- NULL
      }
    tmp <- colnames(est1)
    tmp <- gsub("lbci_lb", "lbci.lower", tmp, fixed = TRUE)
    tmp <- gsub("lbci_ub", "lbci.upper", tmp, fixed = TRUE)
    colnames(est1) <- tmp
    if (!std) {
        print(est1, ..., nd = nd)
        return(invisible(x))
      } else {
        est2 <- est1
        out <- utils::capture.output(print(est2, nd = nd))
        i <- grepl("Parameter Estimates:", out, fixed = TRUE)
        out[i] <- "Standardized Estimates Only"
        out <- gsub_heading(old = "\\s\\s\\s\\sEstimate",
                            new = "Standardized",
                            object = out)
        # i <- grepl("  Standard errors  ", out, fixed = TRUE)
        # j <- unlist(gregexpr("Bootstrap", out[i]))[1]
        # tmp <- "  Confidence interval"
        # st1 <- paste0(tmp,
        #               paste0(rep(" ", j - nchar(tmp) - 1),
        #                      collapse = ""),
        #               "Bootstrap")
        # j <- nchar(out[i])
        # tmp <- "  Confidence Level"
        # tmp2 <- paste0(formatC(level * 100, digits = 1, format = "f"),
        #                "%")
        # st2 <- paste0(tmp,
        #               paste0(rep(" ", j - nchar(tmp) - nchar(tmp2)),
        #                      collapse = ""),
        #               tmp2)
        # tmp <- "  Standardization Type"
        # tmp2 <- attr(x, "type")
        # st3 <- paste0(tmp,
        #               paste0(rep(" ", j - nchar(tmp) - nchar(tmp2)),
        #                      collapse = ""),
        #               tmp2)
        # out <- c(out[seq_len(which(i))],
        #          st1,
        #          st2,
        #          st3,
        #          out[-seq_len(which(i))])
        # out <- gsub("    Estimate  Std.Err",
        #             "Standardized  Std.Err",
        #             out)
        cat(out, sep = "\n")
        return(invisible(x))
      }
  }

#' @noRd

merge_attributes <- function(target, source) {
    tmp1 <- names(attributes(source))
    tmp2 <- names(attributes(target))
    tmp3 <- setdiff(tmp1, tmp2)
    if (length(tmp3) > 0) {
        for (xx in tmp3) {
            attr(target, xx) <- attr(source, xx)
          }
      }
    target
  }

#' @noRd

gsub_heading <- function(old,
                         new,
                         object) {
    i1 <- grepl(old, object)
    i2 <- grepl("lbci", object)
    i <- i1 & i2
    #     Estimate
    # Standardized
    for (xx in which(i)) {
        tmp <- object[i]
        tmp <- gsub(old, new, tmp)
        object[i] <- tmp
      }
    object
  }