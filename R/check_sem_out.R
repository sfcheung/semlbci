#' @title Check the SEM output in semlbci
#'
#' @description Check whether the model and the estimation method in the
#'             `sem_out`
#'             object passed to [semlbci()]
#'              are supported by the current version of
#'              [semlbci()].
#'
#' @details
#' This function is to be used by [semlbci()] but is exported such that
#' the compatibility of an SEM output can be checked independently.
#'
#' Estimation methods (`estimator` in
#'     [lavaan::lavaan()]) currently supported:
#'
#'    - Maximum likelihood (`ML`)
#'
#'    - Full information maximum likelihood with missing data (`fiml`)
#'
#'    - Generalized least squares (`GLS`)
#'
#'    - Weighted least squares (a.k.a.
#'         asymptotically distribution free [ADF]) (`WLS`)
#'
#' Estimation methods not yet supported:
#'
#'    - Unweighted least squares (`ULS`)
#'
#'    - Diagonally weighted least squares (`DWLS`)
#'
#'    - Variants with robust standard errors and/or robust
#'      test statistics:
#'
#'       - `MLM`, `MLMV`, `MLMVS`, `MLF`, and `MLR`.
#'
#'        - `WLSM`, `WLSMV`.
#'
#'        - `ULSM`, `ULSMV`.
#'
#' Currently supported models:
#'
#'    - Single group models with continuous variables.
#'
#' Models not tested:
#'
#'    - Models with categorical variables.
#'
#' Models not yet supported:
#'
#'    - Multigroup models.
#'
#'    - Models with formative factors
#'
#'    - Multilevel models
#'
#' @return
#' A numeric vector of one element. If 0, the model and
#' estimation method are officially supported. If larger than zero,
#' then the model and method are not officially supported. If less
#' than zero, then the model and/or the method are officially
#' not supported.
#'
#' The attributes `info` contains the reason for a value other
#' than zero.
#'
#' @param sem_out The output from an SEM analysis. Currently only
#'                supports a [lavaan::lavaan-class] object.
#'
#' @examples
#' library(lavaan)
#' data(cfa_two_factors)
#' mod <-
#' "
#' f1 =~ x1 + x2 + x3
#' f2 =~ x4 + x5 + x6
#' "
#'
#' fit <- sem(mod, cfa_two_factors)
#'
#' # Should be 0
#' check_sem_out(fit)
#'
#' fit2 <- sem(mod, cfa_two_factors, estimator = "MLR")
#'
#' # Should be negative because MLR is officially not supported
#' check_sem_out(fit2)
#'
#' @export

check_sem_out <- function(sem_out) {
    p_table <- lavaan::parameterTable(sem_out)

    sem_options <- lavaan::lavInspect(sem_out, "options")
    sem_estimator <- sem_options$estimator
    sem_se <- sem_options$se
    sem_test <- sem_options$test
    sem_missing   <- sem_options$missing 
    sem_converged <- lavaan::lavInspect(sem_out, "converged")
    sem_post_check <- lavaan::lavInspect(sem_out, "post.check")
    sem_lavaan_ver <- lavaan::lavInspect(sem_out, "version")
    sem_lavaan_ordered <- lavaan::lavInspect(sem_out, "ordered")
    sem_ngroups <- lavaan::lavInspect(sem_out, "ngroups")
    sem_nlevels <- lavaan::lavInspect(sem_out, "nlevels")
    sem_max_nclusters <- max(unlist(lavaan::lavInspect(sem_out, "nclusters")))

    estimators_supported <- c("ML",
                              "GLS",
                              "WLS")
    estimators_unsupported <- c("ULS",
                                "DWLS",
                                "MLM",
                                "MLMV",
                                "MLMVS",
                                "MLF",
                                "MLR",
                                "WLSM",
                                "WLSMV",
                                "ULSM",
                                "ULSMV",
                                "PML")
    se_supported <- c("standard")
    se_unsupported <- c("robust.sem",
                        "robust.huber.white",
                        "robust",
                        "boot",
                        "bootstrap")
    test_supported <- c("standard")
    test_unsupported <- c("Satorra.Bentler",
                          "Yuan.Bentler",
                          "Yuan.Bentler.Mplus",
                          "scaled.shifted",
                          "boot",
                          "bootstrap",
                          "Bollen.Stine")
    missing_supported <- c("listwise",
                           "ml", "fiml", "direct",
                           "ml.x", "fiml.x", "direct.x")
    missing_unsupported <- c("two.stages", "robust.two.stages",
                             "pairwise",
                             "available.cases",
                             "doubly.robust")

    estimator_ok <- (tolower(sem_estimator) %in% tolower(estimators_supported))
    missing_ok <- (tolower(sem_missing) %in% tolower(missing_supported))
    se_ok <- (tolower(sem_se) %in% tolower(se_supported))
    test_ok <- (tolower(sem_test) %in% tolower(test_supported))

    model_formative_factor <- "<~" %in% p_table$op
    model_multilevel <- (sem_nlevels > 1)
    model_multicluster <- (sem_max_nclusters > 1)
    model_multigroup <- (sem_ngroups > 1)
    model_ordered <- (length(sem_lavaan_ordered) > 0)

    optim_converged <- sem_converged
    optim_admissible <- sem_post_check

    out <- 0
    msg <- NULL

    if (!estimator_ok) {
          out <- ifelse(out >= 0, -1, out - 1)
          msg <- c(msg, paste("Estimator", sem_estimator, "is not yet supported."))
        }

    if (!missing_ok) {
          out <- ifelse(out >= 0, -1, out - 1)
          msg <- c(msg, paste("Missing handling method", sem_estimator, "is not yet supported."))
        }

    if (!se_ok) {
          out <- ifelse(out >= 0, -1, out - 1)
          msg <- c(msg, paste("Standard error method", sem_se, "is not yet supported."))
        }

    if (!test_ok) {
          out <- ifelse(out >= 0, -1, out - 1)
          msg <- c(msg, paste("Test method", sem_test, "is not yet supported."))
        }

    if (model_formative_factor) {
          out <- ifelse(out >= 0, -1, out - 1)
          msg <- c(msg, "Models with formative factor(s) are not yet supported.")
        }

    if (model_multilevel) {
          out <- ifelse(out >= 0, -1, out - 1)
          msg <- c(msg, "Multilevel models are not yet supported.")
        }

    if (model_multicluster) {
          out <- ifelse(out >= 0, -1, out - 1)
          msg <- c(msg, "Clustered models are not yet supported.")
        }

    if (model_multigroup) {
          out <- ifelse(out >= 0, -1, out - 1)
          msg <- c(msg, "Multigroup models are not yet supported.")
        }

    if (model_ordered) {
          out <- ifelse(out >= 0, out + 1, out)
          msg <- c(msg, "Not fully tested on models with ordered variables. Use the function at your own risk.")
        }

    if (!optim_converged) {
          out <- ifelse(out >= 0, -1, out - 1)
          msg <- c(msg, "The estimation has not converged. Fix the estimation first.")
        }

    if (!optim_admissible) {
          out <- ifelse(out >= 0, -1, out - 1)
          msg <- c(msg, "The solution is not admissible by lavaan post.check. Check the SEM results first.")
        }

    attr(out, "info") <- msg
    out
  }
