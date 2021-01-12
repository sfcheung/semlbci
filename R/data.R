#' Dataset: Two factors, six variables, 500 cases
#'
#' Generated from a two-factor model with six variables, n = 500
#'
#' @format A data framw with 500 rows and six variables.
#'
#' @examples 
#' \dontrun{
#' 
#'  library(lavaan)
#'  mod <- "f1 =~ x1 + x2 + x3
#'          f2 =~ x4 + x5 + x6"
#'  fit <- cfa(mod, cfa_two_factors)
#'  summary(fit)
#' }

"cfa_two_factors"

#' Dataset: Simple mediation model, one IV, one DV, one mediation, 500 cases
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
#' @examples 
#' \dontrun{
#' 
#'  library(lavaan)
#'  mod <- "m ~ x
#'          y ~ m"
#'  fit <- cfa(mod, simple_med)
#'  summary(fit)
#' }
#' 
"simple_med"

#' Dataset: Five-predictor regression model with one correlation close to one
#'
#' Generated from a regression model six variables, with correlation
#' between x4 and x5 close to one.
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
#' @examples 
#' \dontrun{
#' 
#'  out <- lm(y ~ x1 + x2 + x3 + x4 + x5, reg_cor_near_one)
#'  summary(out)
#'  cor(reg_cor_near_one[, c("x4", "x5")])
#' 
#' }
#' 
"reg_cor_near_one"

#' Dataset: Two factors with one standardied error variance close to zero.
#'
#' Generated from a two-factor model, with one standardized error variacne close to zero.
#' 
#' @format A data frame with 100 rows and six variables:
#' \describe{
#'    \item{x1}{x1}
#'    \item{x2}{x2}
#'    \item{x3}{x3}
#'    \item{x4}{x4}
#'    \item{x5}{x5}
#'    \item{x6}{x6}
#' }
#' 
#' @examples 
#' \dontrun{
#'  # If fitted by the following model, the standardized 
#'  # error variance of `x3` is close to zero.
#'  # Consequnently, the R-square of `x3` is close to one:
#' 
#'  library(lavaan)
#'  mod <- "f1 =~ x1 + x2 + x3
#'          f2 =~ x4 + x5 + x6"
#'  fit <- cfa(mod, cfa_evar_near_zero)
#'  summary(fit, standardized = TRUE, rsquare = TRUE)
#' }
"cfa_evar_near_zero"