#' @title Confidence Intervals for a smelbci-Class Object
#'
#' @description Return the confidence interval of the parameters
#'              in the output of [semlbci()].
#'
#' @details Return the likelihood-based confidence intervals
#'          in the output of [semlbci()].
#'
#' @return
#'  A two-column matrix of the confidence intervals.
#'
#' @param object The output of [semlbci()].
#' @param parm The parameters for which the confidence
#'             intervals are returned. Not used.
#' @param level Ignored. The level of confidence is determined
#'              when calling [semlbci()] and cannot be changed.
#' @param ...  Optional arguments. Ignored.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
#'
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
#' confint(lbci_med)
#'
#' @export

confint.semlbci <- function(object, parm, level = .95, ...) {
    x <- as.data.frame(object)
    out0 <- x[!is.na(x$method),
              c("lhs", "op", "rhs", "group", "lbci_lb", "lbci_ub"),
              drop = FALSE]
    rhs1 <- out0$rhs
    rhs1[out0$op == ":="] <- ""
    op1 <- out0$op
    op1[out0$op == ":="] <- ""
    group1 <- paste0(".g", as.character(out0$group))
    group1[out0$group == 0] <- ""
    ngroups <- max(out0$group)
    group1[out0$group == 1] <- ""
    pnames <- paste0(out0$lhs, op1, rhs1, group1)
    out1 <- out0[, c("lbci_lb", "lbci_ub"), drop = FALSE]
    rownames(out1) <- pnames
    lb_out_tmp <- attr(object, "lb_out")
    lb_out_tmp <- lb_out_tmp[!sapply(lb_out_tmp, anyNA)]
    level_tmp <- sapply(lb_out_tmp, function(x) x$call$ciperc)
    if ((stats::var(level_tmp) != 0) && length(level_tmp) != 1) {
        stop("The levels of confidence are not identical for all LBCIs.")
      }
    level0 <- level_tmp[1]
    cnames <- paste(format(100 * c((1 - level0) / 2,
                                   1 - (1 - level0) / 2),
                           trim = TRUE,
                           scientific = FALSE,
                           digits = 2), "%")
    colnames(out1) <- cnames
    out1
  }