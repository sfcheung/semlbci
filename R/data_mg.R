#' @title Dataset (CFA, Two Factors, Six Variables, Two Groups)
#'
#' @description Generated from a two-factor model with six variables, n = 500,
#'  two groups, n = 250 each.
#'
#' @details
#' This model is used for examples like this one:
#'
#' ```
#' library(lavaan)
#' mod <- "f1 =~ x1 + x2 + x3
#'         f2 =~ x4 + x5 + x6"
#' fit <- cfa(mod, cfa_two_factors_mg, group = "gp")
#' summary(fit)
#' ```
#'
#' @format A data frame with 500 rows, one grouping variable, `gp`,
#'  six variables, `x1` to `x6`.
#'
#' @examples
#'
#' print(head(cfa_two_factors_mg), digits = 3)
#' nrow(cfa_two_factors_mg)
#' table(cfa_two_factors_mg$gp)
#'
"cfa_two_factors_mg"

#' @title Dataset (Simple Mediation Model, Two Groups)
#'
#' @description Generated from a simple mediation model, n = 200, two groups,
#'  n = 100 each.
#'
#' @format A data frame with 500 rows and four variables:
#' \describe{
#'    \item{gp}{gp, the grouping variable}
#'    \item{x}{x, the independent variable}
#'    \item{m}{m, the mediator}
#'    \item{y}{y, the dependent variable}
#' }
#'
#' @details
#' This model is used for examples like this one:
#'
#' ```
#' library(lavaan)
#' mod <- "m ~ x
#'         y ~ m"
#' fit <- sem(mod, simple_med_mg, gp = "group")
#' summary(fit)
#' ```
#'
#' @examples
#'
#' print(head(simple_med_mg), digits = 3)
#' nrow(simple_med_mg)
#' table(simple_med_mg$gp)
#'
"simple_med_mg"
