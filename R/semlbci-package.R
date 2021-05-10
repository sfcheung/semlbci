#' semlbci: Likelihood Based Confidence Interval for SEM
#'
#' @description
#'
#' The main function in this package is for forming the likelihood based
#' confidence intervals (LBCIs) for parameters in structural equation modeling.
#'
#' The main function is [semlbci()], which receives an SEM output and
#' finds the LBCIs for selected parameters. It will call [ci_i()] for each
#' parameter. [ci_i()] will then call [ci_bound_wn_i()] twice, each time
#' find one of the two confidence limits (lower and upper limits).
#'
#' Currently, the supported approach is the one proposed by
#' Wu and Neale (2012, see also Pek & Wu, 2015). This method is easy to program
#' and the illustration by Pek and Wu (2015) is one of the factor that motivates
#' this project.
#'
#' Another package, [OpenMx::OpenMx], is much faster in finding the LBCI of a
#' parameter
#' (Pritikin, Rappaport, & Neale, 2017).
#' If a user is already using [OpenMx::OpenMx] for doing SEM, then we also
#' recommend
#' using [OpenMx::OpenMx] instead of this pacakge.
#'
#' This package is for users who are using [lavaan] to do SEM and want to find
#' the LBCI for selected parameters (e.g., the indirect effect of a variable on
#' another variable), but do not plan to migrate to [OpenMx::OpenMx]. The
#' present
#' package is slower, but should be sufficient for this purpose when the cost to
#' learn another package does not justify the benefit.
#'
#' @references
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
