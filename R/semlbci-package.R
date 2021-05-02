#' semlbci: Likelihood Based Confidence Interval for SEM
#'
#' @description
#'
#' The main function in this package is for forming the likelihood based
#' confidence intervals (LBCIs) for parameters in structural equation modeling.
#'
#' The main function is [semlbci()], which receives an SEM output and
#' finds the LBCIs for selected parameters.
#'
#' The function [ci_bound_nm_i()] and [ci_bound_wn_i()] are the workhorses, which
#' find the confidence limit of one parameter.
#'
#' Currently, the preferred and default approach is the one proposed by 
#' Neale and Miller (1997). The first approach we tried is the one proposed in
#' Wu and Neale (2012, see also Pek & Wu, 2015). It is much easier to program
#' but is slower in optimization. It is still avaiable but is of low priority
#' in future development.
#'
#' Another package, [OpenMx::OpenMx], is much faster in finding the LBCI of a parameter
#' (Pritikin, Rappaport, & Neale, 2017).
#' If a user is already using [OpenMx::OpenMx] for doing SEM, then we also recommend 
#' using [OpenMx::OpenMx] instead of this pacakge.
#' 
#' This package is for users who are using [lavaan] to do SEM and want to find
#' the LBCI for selected parameters (e.g., the indirect effect of a variable on
#' another variable), but not yet want to migrate to [OpenMx::OpenMx]. The present 
#' package is slow, but should be sufficient for this purpose when the cost to
#' learn another package does not justify the benefit.
#'
#' @references
#' 
#' Neale, M. C., & Miller, M. B. (1997). The use of likelihood-based confidence
#' intervals in genetic models. *Behavior Genetics, 27*(2), 113–120.
#' \url{https://doi.org/10.1023/A:1025681223921}
#'
#' Pek, J., & Wu, H. (2015). Profile likelihood-based confidence intervals and
#'  regions for structural equation models. *Psychometrika, 80*(4), 1123–1145.
#'  \url{https://doi.org/10.1007/s11336-015-9461-1}
#'
#' Pritikin, J. N., Rappaport, L. M., & Neale, M. C. (2017). Likelihood-based
#' confidence intervals for a parameter with an upper or lower bound.
#' *Structural Equation Modeling: A Multidisciplinary Journal, 24*(3), 395–401.
#' \url{https://doi.org/10.1080/10705511.2016.1275969}
#'
#' Wu, H., & Neale, M. C. (2012). Adjusted confidence intervals for a bounded
#'  parameter. *Behavior Genetics, 42*(6), 886–898.
#'  \url{https://doi.org/10.1007/s10519-012-9560-z}
#' 
#' @seealso
#' [semlbci()]
#' 
#' @docType package
#' @name semlbci-package
#' 
NULL
