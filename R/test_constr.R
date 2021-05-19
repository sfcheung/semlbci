#' @title Test a limit using a constrained model
#'
#' @description Test a limit using a constrained model
#'
#' @return
#' The anova results
#'
#' @param fit The source fit object.
#' @param dat The test data.
#' @param ciperc The level of confidence of the limit.
#' @param parc The character for the constraint(s).
#' @param parc2 The character for the additional constraint(s).
#' @param modc0 The base model to be constrained.
#' @param ci_out The output from the ci_bound_*_i function.
#' @param semfct The sem function to be used.
#' @param ... Other arguments to be passed to the sem function
#'
#' @examples
#' # TODO
#'
#' @keywords internal

test_constr <- function(fit,
                        dat,
                        ciperc = .95,
                        parc,
                        parc2 = "",
                        modc0,
                        ci_out,
                        semfct,
                        tol = 1e-4,
                        update_args = list(),
                        ...) {
    modc <- paste(modc0, "\n", parc, ci_out$bound, "\n", parc2 = "")
    fitc <- semfct(model = modc, data = dat, do.fit = FALSE, ...)
    ptable <- lavaan::parameterTable(fitc)
    ptable[ptable$free > 0, "est"] <-  ci_out$diag$history$solution
    update_args_final <- utils::modifyList(
                          list(object = fitc,
                               data = dat,
                               start = ptable,
                               do.fit = TRUE),
                          update_args)
    fitc <- do.call(lavaan::update, update_args_final)
    anova_out <- lavaan::anova(fitc, fit)
    anova_p <- anova_out["fitc", "Pr(>Chisq)"]
    test_p <- (abs(anova_p - (1 - ciperc)) < tol)
    out <- test_p
    attr(out, "anova") <- anova_out
    attr(out, "pvalue") <- anova_p
    attr(out, "fitc") <- fitc
    return(out)
  }
