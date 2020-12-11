#'@title Check whether the sem_out passed to semlbci is
#'        acceptable
#'
#'@description Check whether the sem_out passed to semlbci
#'             contains a model and estimation method 
#'             that are supported by the current version of
#'              [`semlbci`].
#'
#'@details
#' Currenctly supported estimation methods ([`estimator`] in
#'     [`lavaan`]):
#'
#'    - Maximum likelihood [`ML`]
#'
#'    - Full information maximum likelihood with missing data [`fiml`]
#'
#'    - Generalized least squares [`GLS`]
#'
#'    - Weighted least squares (a.k.a. ADF) [`WLS`]
#'
#' Unsupported estimation methods ([`estimator`]):est
#'
#'    - Unweighted least squares [`ULS`]
#' 
#'    - Diagonally weighted least squares [`DWLS`]
#'
#'    - Variants with robust standard errors and/or robust 
#'      test statistics:
#' 
#'       - [`MLM`], [`MLMV`], [`MLMVS`], [`MLF`], and [`MLR`]
#'
#'        - [`WLSM`], [`WLSMV`]
#'
#'        - [`ULSM`], [`ULSMV`]
#' 
#' Currenctly supported models:
#' 
#'    - Single group models with continuous variables.
#' 
#' Models not tested:
#' 
#'    - Multigroup models.
#' 
#'    - Models with categorical variables.
#' 
#' Models not supported:
#' 
#'    - Models with formative factors
#' 
#' 
#' 
#'@return
#' A numeric vector of one element. If 0, the model and 
#' estimation method are formally supported. If larger than zero,
#' then the model and method are not formally supported. If less
#' than zero, then the model and/or the method are formally
#' not supported.
#' 
#' The attributes [`info`] contains the reason for a value other
#' than zero.
#' 
#' @param sem_out The output from an SEM analysis. Currently only
#'                supports a [`lavaan`] object.
#'
#'@examples
#' library(lavaan)
#' data(cfa_two_factors)
#' mod <- 
#' "
#' f1 =~ x1 + x2 + a*x3
#' f2 =~ x4 + a*x5 + equal('f1=~x2')*x6
#' f1 ~~ 0*f2
#' asq := a^2
#' "
#' fit <- sem(mod, cfa_two_factors)


check_sem_out <- function(sem_out) {
    sem_options <- lavaan::lavInspect(sem_out, "options")
    sem_estimator <- sem_options$estimator
    sem_missing   <- sem_options$missing 
    sem_converged <- lavaan::lavInspect(sem_out, "converged")
    sem_post_check <- lavaan::lavInspect(sem_out, "post.check")
    sem_lavaan_ver <- lavaan::lavInspect(sem_out, "version")
    sem_lavaan_oreder <- lavaan::lavInspect(sem_out, "ordered")
    sem_ngroups <- lavaan::lavInspect(sem_out, "ngroups")
    sem_nlevels <- lavaan::lavInspect(sem_out, "nlevels")
  }  
