#' semlbci: Likelihood Based Confidence Interval for SEM
#'
#' @description
#'
#' This package is for forming the likelihood-based confidence
#' intervals (LBCIs) for parameters in structural equation modeling.
#'
#' The main function is [semlbci()], which receives an SEM output and
#' finds the LBCIs for selected parameters. It will call [ci_i_one()] for each
#' parameter. [ci_i_one()] will then call [ci_bound_wn_i()] twice, each time
#' find one of the two confidence limits (lower and upper limits).
#'
#' Currently, the supported approach is the one proposed by Wu and
#' Neale (2012, see also Pek & Wu, 2015). This method is easy to
#' program and the illustration by Pek and Wu (2015) is one of the
#' factor that motivates this project.
#'
#' Another package, [OpenMx::OpenMx], natively support LBCIs
#' (Pritikin, Rappaport, & Neale, 2017). However, it is not yet
#' available in [lavaan], another popular SEM package.
#'
#' This package is for users who are using [lavaan] to do SEM and want
#' to find the LBCIs for selected parameters (e.g., the indirect
#' effect of a variable on another variable), but do not plan to
#' migrate to [OpenMx::OpenMx]. The present package is slower than
#' [OpenMx::OpenMx], but should be sufficient for this purpose when
#' the cost to learn another package does not justify the benefit.
#'
#' @references
#'
#' Falk, C. F. (2018). Are robust standard errors the best approach
#'  for interval estimation with nonnormal data in structural equation
#'  modeling? *Structural Equation Modeling: A Multidisciplinary
#'  Journal, 25*(2), 244-266.
#'  \doi{10.1080/10705511.2017.1367254}
#'
#' Pek, J., & Wu, H. (2015). Profile likelihood-based confidence
#'  intervals and regions for structural equation models.
#'  *Psychometrika, 80*(4), 1123-1145.
#'  \doi{10.1007/s11336-015-9461-1}
#'
#' Pritikin, J. N., Rappaport, L. M., & Neale, M. C. (2017).
#'  Likelihood-based confidence intervals for a parameter with an
#'  upper or lower bound. *Structural Equation Modeling: A
#'  Multidisciplinary Journal, 24*(3), 395-401.
#'  \doi{10.1080/10705511.2016.1275969}
#'
#' Wu, H., & Neale, M. C. (2012). Adjusted confidence intervals for a
#'  bounded parameter. *Behavior Genetics, 42*(6), 886-898.
#'  \doi{10.1007/s10519-012-9560-z}
#'
#' @seealso
#' [semlbci()]
#'
#' @docType package
#' @name semlbci-package
#'
NULL
utils::globalVariables(c("theta", "loglike", "pvalue"))
