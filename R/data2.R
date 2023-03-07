#' @title Dataset (SEM, Three Factors, Nine Variables, Mediation)
#'
#' @description Generated from a three-factor model with nine variables, n = 150
#'
#'
#' @format A data frame with 150 rows and nine variables:
#' \describe{
#'    \item{x1}{x1}
#'    \item{x2}{x2}
#'    \item{x3}{x3}
#'    \item{x4}{x4}
#'    \item{x5}{x5}
#'    \item{x6}{x6}
#'    \item{x7}{x7}
#'    \item{x8}{x8}
#'    \item{x9}{x9}
#' }
#'
#' @details
#' This model is used for examples like this one:
#'
#'
#' ```
#' mod <-
#' "
#' fx =~ x1 + x2 + x3
#' fm =~ x4 + x5 + x6
#' fy =~ x7 + x8 + x9
#' fm ~ a*fx
#' fy ~ b*fm + cp*fx
#' ab := a*b
#' "
#' fit <- lavaan::sem(mod, mediation_latent)
#' ```
#'
#' @examples
#'
#' print(head(mediation_latent), digits = 3)
#' nrow(mediation_latent)
#'
#'
"mediation_latent"
