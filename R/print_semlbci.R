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
#' @param output The type of printout.
#' If `"table"`, the default, the
#' results will be printed in a table.
#' If `"text"` or `"lavaan"`, then the
#' results will be printed in the
#' `lavaan` style, as in the [summary()]
#' method for the output of `lavaan`.
#'
#' @param sem_out If `output` is
#' `"text"` or `"lavaan"`, the original
#' output of `lavaan` used in calling
#' [semlbci()] needs to be supplied
#' to this argument.
#'
#' @param lbci_only Used only if `output`
#' is `"text"` or `"lavaan"`. If `TRUE`,
#' only the likelihood-based confidence
#' intervals (LBCIs) will be printed.
#' If `FALSE`, and LBCIs will be
#' printed alongside the confidence
#' intervals by `lavaan`. Its default
#' value depend on the argument
#' `drop_no_lbci`. If `drop_no_lbci`
#' is `TRUE`, then `lbci_only` is
#' `TRUE` by default. If `drop_no_lbci`
#' is `FALSE`, then `lbci_only` is
#' `FALSE` by default.
#'
#' @param ratio_digits The number of
#' digits after the decimal points
#' for the ratios of distance from
#' the confidence limits
#' to the point estimates. Default is
#' 1.
#'
#' @param se Logical. To be passed to
#' [lavaan::parameterEstimates()].
#' Whether standard error (S.E.) will be
#' printed. Only applicable if `output`
#' is `"text"` or `"lavaan"`.
#'
#' @param zstat Logical. To be passed to
#' [lavaan::parameterEstimates()].
#' Whether z-values will be printed.
#' Only applicable if `output` is
#' `"text"` or `"lavaan"`.
#'
#' @param pvalue Logical. To be passed
#' to [lavaan::parameterEstimates()].
#' Whether p-values will be printed.
#' Only applicable if `output` is
#' `"text"` or `"lavaan"`.
#'
#' @param boot.ci.type Logical. To be
#' passed to
#' [lavaan::parameterEstimates()]. The
#' type of bootstrap confidence
#' intervals to be printed if
#' bootstrapping confidence intervals
#' available. Possible values are
#' `"norm"`, `"basic"`, `"perc"`, or
#' `"bca.simple"`. The default value is
#' `"perc"`. Refer to the help of
#' [lavaan::parameterEstimates()] for
#' further information. Only applicable
#' if `output` is `"text"` or
#' `"lavaan"`.
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
#' # Text output
#'
#' print(lbci_med, output = "lavaan", sem_out = fit_med)
#'
#' print(lbci_med, output = "lavaan", sem_out = fit_med, lbci_only = FALSE)
#'
#' print(lbci_med, output = "lavaan", sem_out = fit_med, lbci_only = FALSE,
#'       se = FALSE, zstat = FALSE, pvalue = FALSE)
#'
#' @export

print.semlbci <- function(x,
                          digits = 3,
                          annotation = TRUE,
                          time = FALSE,
                          verbose = FALSE,
                          verbose_if_needed = TRUE,
                          drop_no_lbci = TRUE,
                          output = c("table", "text", "lavaan"),
                          sem_out = NULL,
                          lbci_only = drop_no_lbci,
                          ratio_digits = 1,
                          se = TRUE,
                          zstat = TRUE,
                          pvalue = TRUE,
                          boot.ci.type = "perc",
                          ...) {
    output <- match.arg(output)
    if (isTRUE(output == "lavaan")) output <- "text"
    if (verbose) verbose_if_needed <- FALSE
    if (isTRUE(output == "text")) {
        if (is.null(sem_out)) {
            stop("Argument 'sem_out' must be supplied ",
                 "if output type is 'text' or 'lavaan'.")
          }
        if (!compare_semlbci_sem_out(x,
                                     sem_out = sem_out)) {
            stop("The value of 'sem_out' does not appear ",
                 "to be the fit object used in the call ",
                 "to 'semlbci()'.")
          }
        print_semlbci_text(x = x,
                           nd = digits,
                           sem_out = sem_out,
                           lbci_only = lbci_only,
                           drop_no_lbci = drop_no_lbci,
                           annotation = annotation,
                           verbose = verbose,
                           verbose_if_needed = verbose_if_needed,
                           ratio_digits = ratio_digits,
                           se = se,
                           zstat = zstat,
                           pvalue = pvalue,
                           boot.ci.type = boot.ci.type,
                           ...)
        return(invisible(x))
      }
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

        verbose_chk <- verbose_needed(out)
        ratio_note <- verbose_chk$ratio_note
        post_check_note <- verbose_chk$post_check_note
        status_note <- verbose_chk$status_note

        # if (is.null(call_org$ci_limit_ratio_tol)) {
        #     ci_limit_ratio_tol <- formals(ci_bound_wn_i)$ci_limit_ratio_tol
        #   } else {
        #     ci_limit_ratio_tol <- call_org$ci_limit_ratio_tol
        #   }
        # ratio_x <- c(out$ratio_lb, out$ratio_ub)
        # ratio_note <- any(ratio_x > ci_limit_ratio_tol, na.rm = TRUE) |
        #               any(ratio_x < 1 / ci_limit_ratio_tol, na.rm = TRUE)
        if (!ratio_note) {
            out$ratio_lb <- NULL
            out$ratio_ub <- NULL
          }

        # post_check_note <- !all(out$post_check_lb,
        #                         out$post_check_ub,
        #                         na.rm = TRUE)
        if (!post_check_note) {
            out$post_check_lb <- NULL
            out$post_check_ub <- NULL
          }

        # status_note <- !all(out$status_lb == 0,
        #                     out$status_ub == 0,
        #                     na.rm = TRUE)
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

print_semlbci_text <- function(x,
                               sem_out = sem_out,
                               nd = 3,
                               lbci_only = FALSE,
                               drop_no_lbci = FALSE,
                               annotation = TRUE,
                               verbose = FALSE,
                               verbose_if_needed = TRUE,
                               ratio_digits = 1,
                               se = TRUE,
                               zstat = TRUE,
                               pvalue = TRUE,
                               boot.ci.type = "perc",
                               ...) {
    # Adapted from lavaan::print.lavaan.parameterEstimates()
    num_format  <- paste("%", max(8L, nd + 5L), ".", nd, "f", sep = "")
    int_format  <- paste("%", max(8L, nd + 5L), "d", sep = "")
    char_format <- paste("%", max(8L, nd + 5L), "s", sep = "")

    x_call <- attr(x, "call")
    i_no_lbci <- is.na(x$status_lb) & is.na(x$status_ub)
    verbose_chk <- verbose_needed(x)

    ciperc <- x_call$ciperc
    if (is.null(ciperc)) {
        ciperc <- formals(semlbci)$ciperc
      }
    x_df <- as.data.frame(x)
    x_df$block <- x_df$group
    x_df$ratio_check_lb <- verbose_chk$ratio_check_lb
    x_df$ratio_check_ub <- verbose_chk$ratio_check_ub
    if ("est.std" %in% colnames(x)) {
        std <- TRUE
      } else {
        std <- FALSE
      }
    est0 <- lavaan::parameterEstimates(sem_out,
                                       standardized = FALSE,
                                       level = ciperc,
                                       output = "text",
                                       header = TRUE,
                                       se = se,
                                       zstat = zstat,
                                       pvalue = pvalue,
                                       boot.ci.type = boot.ci.type)
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
                                "cl_lb", "cl_ub",
                                "ratio_check_lb", "ratio_check_ub")
    est1 <- merge(est1,
                  x_df[, i0],
                  all.x = TRUE)
    est1 <- est1[order(est1$id), ]
    est1$id <- NULL
    est1 <- merge_attributes(est1,
                             est0)
    class(est1) <- c("lavaan.parameterEstimates", class(est1))
    i_drop <- is.na(est1$status_lb) & is.na(est1$status_ub)
    est1$lbci_lb <- sprintf(num_format,
                            as.numeric(est1$lbci_lb))
    est1$lbci_lb[i_drop] <- "--"
    est1$lbci_ub <- sprintf(num_format,
                            as.numeric(est1$lbci_ub))
    est1$lbci_ub[i_drop] <- "--"
    if (drop_no_lbci) {
        est1 <- est1[!i_drop, ]
      }
    if (verbose_if_needed) {
        ratio_note <- verbose_chk$ratio_note
        post_check_note <- verbose_chk$post_check_note
        status_note <- verbose_chk$status_note
        if (!ratio_note) {
            est1$ratio_lb <- NULL
            est1$ratio_ub <- NULL
            est1$ratio_check_lb <- NULL
            est1$ratio_check_ub <- NULL

          }
        if (!post_check_note) {
            est1$post_check_lb <- NULL
            est1$post_check_ub <- NULL
          }
        if (!status_note) {
            est1$status_lb <- NULL
            est1$status_ub <- NULL
          }
      } else {
        ratio_note <- TRUE
        post_check_note <- TRUE
        status_note <- TRUE
      }
    est1$cl_lb <- NULL
    est1$cl_ub <- NULL
    if (lbci_only) {
        est1$ci.lower <- NULL
        est1$ci.upper <- NULL
      }
    ngroups <- lavaan::lavTech(sem_out, "ngroups")
    if (ngroups == 1) {
        est1$group <- NULL
      }
    est2 <- est1
    if (status_note) {
        est2$status_lb <- format_status(est2$status_lb)
        est2$status_ub <- format_status(est2$status_ub)
        est2$Status <- paste0(est2$status_lb,
                              ";",
                              est2$status_ub)
        est2$Status <- sprintf(char_format, est2$Status)
        est2$Status[which(est2$lbci_lb == "--")] <- "--"
        est2$status_lb <- NULL
        est2$status_ub <- NULL
      }
    if (ratio_note) {
        est2$ratio_lb <- format_ratio(est2$ratio_lb,
                                      digits = ratio_digits,
                                      flag = est2$ratio_check_lb,
                                      where = "none")
        est2$ratio_ub <- format_ratio(est2$ratio_ub,
                                      digits = ratio_digits,
                                      flag = est2$ratio_check_ub,
                                      where = "none")
        est2$Ratio <- paste0(est2$ratio_lb,
                             ";",
                             est2$ratio_ub)
        est2$Ratio <- sprintf(char_format, est2$Ratio)
        est2$Ratio[which(est2$lbci_lb == "--")] <- "--"
        est2$ratio_lb <- NULL
        est2$ratio_ub <- NULL
        est2$ratio_check_lb <- NULL
        est2$ratio_check_ub <- NULL
      }
    if (post_check_note) {
        est2$post_check_lb <- format_post_check(est2$post_check_lb)
        est2$post_check_ub <- format_post_check(est2$post_check_ub)
        est2$Check <- paste0(est2$post_check_lb,
                             ";",
                             est2$post_check_ub)
        est2$Check <- sprintf(char_format, est2$Check)
        est2$Check[which(est2$lbci_lb == "--")] <- "--"
        est2$post_check_lb <- NULL
        est2$post_check_ub <- NULL
      }
    tmp <- colnames(est2)
    tmp <- gsub("lbci_lb", "lb.lower", tmp, fixed = TRUE)
    tmp <- gsub("lbci_ub", "lb.upper", tmp, fixed = TRUE)
    colnames(est2) <- tmp
    out <- utils::capture.output(print(est2, nd = nd))
    if (annotation) {
        # i0 <- grepl("  Standard errors  ", out, fixed = TRUE)
        # tmp <- out
        # tmp[seq_len(which(i0))] <- "%%"
        # i1 <- match("", tmp)
        tmp0 <- "- lb.lower, lb.upper: "
        tmp <- paste0(tmp0,
                      "The lower and upper likelihood-based ",
                      "confidence bounds.")
        tmp <- strwrap(tmp,
                       indent = 0,
                       exdent = nchar(tmp0))
        msg <- tmp
        if (status_note) {
            tmp0 <- "- Status: "
            tmp <- paste0(tmp0, "Whether the search encountered any problem for the two bounds. ",
                    "If no problem encountered, the code is 0 (displayed as 'OK'). Any value ",
                    "other than 0 indicates something was wrong in the search.")
            tmp <- strwrap(tmp,
                           indent = 0,
                           exdent = nchar(tmp0))
            msg <- c(msg, tmp)
          }
        if (ratio_note) {
            tmp0 <- "- Ratio: "
            tmp <- paste0(tmp0,
                    "Ratio of a to b, ",
                    "a = Distance from the point estimate to a likelihood-based",
                    " confidence bound, b = Distance from the point estimate to the original",
                    " confidence bound. A bound should be interpreted with caution if",
                    " the ratio is too large or too small, indicating a large",
                    " difference between the original interval and the",
                    " likelihood-based interval.")
            tmp <- strwrap(tmp,
                           indent = 0,
                           exdent = nchar(tmp0))
            msg <- c(msg, tmp)
          }
        if (post_check_note) {
            tmp0 <- "- Check: "
            tmp <- paste0(tmp0,
                    "Whether the final solution of ",
                    "a confidence bound passed the post optimization check of lavaan ",
                    "by lavaan::lavInspect(fit, 'post.check'), where ",
                    "fit is the final solution.")
            tmp <- strwrap(tmp,
                           indent = 0,
                           exdent = nchar(tmp0))
            msg <- c(msg, tmp)
          }
        msg <- c("Likelihood-Based CI Notes:", "", msg)
        # out <- append(out,
        #               msg,
        #               after = i1)
        out <- c(msg, out)
      }
    if (!std) {
        cat(out, sep = "\n")
        return(invisible(x))
      } else {
        i <- grepl("Parameter Estimates:", out, fixed = TRUE)
        out[i] <- "Standardized Estimates Only"
        out <- gsub_heading(old = "\\s\\s\\s\\sEstimate",
                            new = "Standardized",
                            object = out)
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
    # i2 <- grepl("lbci", object)
    # i <- i1 & i2
    i <- i1
    for (xx in which(i)) {
        tmp <- object[i]
        tmp <- gsub(old, new, tmp)
        object[i] <- tmp
      }
    object
  }

#' @noRd

verbose_needed <- function(x) {
    call_org <- attr(x, "call")
    if (is.null(call_org$ci_limit_ratio_tol)) {
        ci_limit_ratio_tol <- formals(ci_bound_wn_i)$ci_limit_ratio_tol
      } else {
        ci_limit_ratio_tol <- call_org$ci_limit_ratio_tol
      }
    ratio_x <- c(x$ratio_lb, x$ratio_ub)
    ratio_check_lb <- (x$ratio_lb > ci_limit_ratio_tol) |
                      (x$ratio_lb < 1 / ci_limit_ratio_tol)
    ratio_check_ub <- (x$ratio_ub > ci_limit_ratio_tol) |
                      (x$ratio_ub < 1 / ci_limit_ratio_tol)
    ratio_note <- any(ratio_x > ci_limit_ratio_tol, na.rm = TRUE) |
                  any(ratio_x < 1 / ci_limit_ratio_tol, na.rm = TRUE)

    post_check_note <- !all(x$post_check_lb,
                            x$post_check_ub,
                            na.rm = TRUE)

    status_note <- !all(x$status_lb == 0,
                        x$status_ub == 0,
                        na.rm = TRUE)

    out <- list(ratio_note = ratio_note,
                ratio_check_lb = ratio_check_lb,
                ratio_check_ub = ratio_check_ub,
                post_check_note = post_check_note,
                status_note = status_note)
    out
  }

#' @noRd

format_status <- function(object) {
    tmp <- character(length(object))
    tmp[which(object == 0)] <- "OK"
    i <- object != 0
    tmp[which(i)] <- paste0("<", object[which(i)], ">")
    tmp[is.na(object)] <- ""
    tmp
  }

#' @noRd

format_ratio <- function(object,
                         digits = 1,
                         flag,
                         where = c("left", "right", "none")) {
    where <- match.arg(where)
    tmp <- formatC(as.numeric(object),
                   digits = digits,
                   format = "f")
    tmp2 <- tmp
    if (where == "left") {
        tmp2[which(!flag)] <- paste0(" ", tmp[which(!flag)])
        tmp2[which(flag)] <- paste0("<", tmp[which(flag)])
      } else if (where == "right") {
        tmp2[which(!flag)] <- paste0(tmp[which(!flag)], " ")
        tmp2[which(flag)] <- paste0(tmp[which(flag)], ">")
      }
    tmp2[is.na(flag)] <- ""
    tmp2
  }

#' @noRd

format_post_check <- function(object) {
    tmp <- character(length(object))
    tmp[which(object)] <- "OK"
    tmp[which(!object)] <- "Failed"
    tmp[is.na(object)] <- ""
    tmp
  }

#' @noRd

compare_semlbci_sem_out <- function(semlbci_out,
                                    sem_out) {
    tmp1 <- lavaan::lav_partable_labels(semlbci_out)
    ptable <- lavaan::parameterTable(sem_out)
    tmp2 <- lavaan::lav_partable_labels(ptable)
    out <- (length(setdiff(tmp1, tmp2)) == 0) &&
           (length(setdiff(tmp2, tmp1)) == 0)
    return(out)
  }