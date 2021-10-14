#' @title Standardized Estimate Function
#'
#' @description This function is to be used to define user-defined
#'  parameters in `lavaan`. Exported to be used by [ci_i()] to find
#'  the scaling factor for robust LBCI. Not to be used by normal
#'  users.
#'
#' @return The standardized estimate of the *i* parameter.
#'
#' @param fit_str A one-element character vector. The name of the fit object.
#'
#' @param i The position of the parameter in the parameter table.
#'
#' @param std_method The method used to find the standardized solution.
#'  If equal to `"lavaan"``, [lavaan::standardizedSolution()] will be used.
#'  If equal to `"internal"`, an internal function of this package will be used.
#'  The `"lavaan"` method should work in all situations, but the `"internal"`
#'  method can be faster. Default is `"lavaan"` for now, but may be changed to
#'  `"internal"` if it is confirmed to work in all situations tested.
#'
#' @examples
#' data(simple_med)
#'
#' library(lavaan)
#' mod <-
#' "
#' m ~ x
#' y ~ m
#' "
#' fit_med <- lavaan::sem(mod, simple_med, fixed.x = FALSE)
#' standardizedSolution(fit_med)[1, ]
#' fit_med2 <- update(fit_med, add = "mx_std := semlbci::get_std('fit_med', 1)")
#' parameterEstimates(fit_med2)[6, ]
#'
#' @export

get_std <- function(fit_str, i, std_method = "lavaan") {
    fit <- eval(get(fit_str), parent.frame())
    fit_pt <- lavaan::parameterTable(fit)
    .x. <- get(".x.", envir = parent.frame())

    # lavaan::standardizedSolution
    if (std_method == "lavaan") {
        fit@Model <- lavaan::lav_model_set_parameters(
                          fit@Model, .x.
                        )
        fit_pt2 <- fit_pt
        nfree <- sum(fit_pt$free > 0)
        fit_pt2[fit_pt$free > 0, "est"] <- .x.[seq_len(nfree)]
        fit@ParTable <- as.list(fit_pt2)
        std <- lavaan::standardizedSolution(
                          fit,
                          se = FALSE,
                          zstat = FALSE,
                          pvalue = FALSE,
                          ci = FALSE,
                          cov.std = FALSE,
                          remove.eq = FALSE,
                          remove.ineq = FALSE,
                          remove.def = FALSE,
                          )
        return(std[i, "est.std"])
      }

    # Internal standardization
    if (std_method == "internal") {
        return(std_lav(.x., fit)[i])
      }
    stop("std_method unknown.")
  }
