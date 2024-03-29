#' @title Dataset (CFA, Two Factors, Six Variables)
#'
#' @description Generated from a two-factor model with six variables, n = 500
#'
#' @details
#' This model is used for examples like this one:
#'
#' ```
#' library(lavaan)
#' mod <- "f1 =~ x1 + x2 + x3
#'         f2 =~ x4 + x5 + x6"
#' fit <- cfa(mod, cfa_two_factors)
#' summary(fit)
#' ```
#'
#' @format A data frame with 500 rows and six variables, `x1` to `x6`.
#'
#' @examples
#'
#' print(head(cfa_two_factors), digits = 3)
#' nrow(cfa_two_factors)
#'
"cfa_two_factors"

#' @title Dataset (Simple Mediation Model)
#'
#' @description Generated from a simple mediation model, n = 200
#'
#' @format A data frame with 200 rows and three variables:
#' \describe{
#'    \item{x}{x, the independent variable}
#'    \item{m}{m, the mediator}
#'    \item{y}{y, the dependent variable}
#' }
#'
#' @details
#' This model is used for examples like this one:
#'
#'
#' ```
#' library(lavaan)
#' mod <- "m ~ x
#'         y ~ m"
#' fit <- cfa(mod, simple_med)
#' summary(fit)
#' ```
#'
#' @examples
#'
#' print(head(simple_med), digits = 3)
#' nrow(simple_med)
#'
#'
"simple_med"

#' @title Dataset (Six Variables, One Correlation Close to One)
#'
#' @description Generated from a regression model six variables,
#'  x4~~x5 correlation close to one.
#'
#' @format A data frame with 100 rows and six variables:
#' \describe{
#'    \item{x1}{x1}
#'    \item{x2}{x2}
#'    \item{x3}{x3}
#'    \item{x4}{x4, with correlation with x5 nearly equal to 1}
#'    \item{x5}{x5, with correlation with x4 nearly equal to 1}
#'    \item{y}{y, the dependent variable}
#' }
#'
#' @details
#' This model is used for examples like this one:
#'
#'
#' ```
#' out <- lm(y ~ x1 + x2 + x3 + x4 + x5, reg_cor_near_one)
#' summary(out)
#' cor(reg_cor_near_one[, c("x4", "x5")])
#' ```
#'
#' @examples
#'
#' print(head(reg_cor_near_one), digits = 3)
#' nrow(reg_cor_near_one)
#'
#'
"reg_cor_near_one"

#' @title Dataset (CFA, Two Factors, One Standardized Error Variance
#'  Close to Zero)
#'
#' @description Generated from a two-factor model, with one
#'  standardized error variance close to zero.
#'
#' @format A data frame with 120 rows and six variables, `x1` to `x6`
#'
#' @details
#' This model is used for examples like this one:
#'
#'
#' ```
#' # If fitted by the following model, the standardized
#' # error variance of `x3` is close to zero.
#' # Consequently, the R-square of `x3` is close to one:
#'
#' library(lavaan)
#' mod <- "f1 =~ x1 + x2 + x3
#'         f2 =~ x4 + x5 + x6"
#' fit <- cfa(mod, cfa_evar_near_zero)
#' summary(fit, standardized = TRUE, rsquare = TRUE)
#' ```
#' @examples
#'
#' print(head(cfa_evar_near_zero), digits = 3)
#' nrow(cfa_evar_near_zero)
#'
#'
#'
"cfa_evar_near_zero"