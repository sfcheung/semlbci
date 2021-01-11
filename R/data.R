#' Two factors, six variables, 500 cases
#'
#' Generated from a two-factor model with six variables, n = 500
#'
#' @format A data framw with 500 rows and six variables.
#'
"cfa_two_factors"

#' Simple mediation model, one IV, one DV, one mediation, 500 cases
#'
#' Generated from a simple mediation model, n = 500
#'
#' @format A data framw with 500 rows and three variables:
#' \describe{
#'    \item{x}{x, the independent variable}
#'    \item{m}{m, the mediatior}
#'    \item{y}{y, the dependent variable}
#' }
#' 
"simple_med"

#' Five-predictor regression model with one correlation close to one
#'
#' Generated from a regression model six variables, with correlation between x4 and x5 close to one.
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
"reg_cor_near_one"

#' Generated from a two-factor model, with one error variacne close to zero.
#' 
"cfa_evar_near_zero"