#' @title Pre-analysis Check For 'semlbci'
#'
#' @description Check the output passed to [semlbci()]
#'
#' @details It checks whether the model and the estimation method in
#'  the `sem_out` object passed to [semlbci()] are supported by the
#'  current version of [semlbci()]. This function is to be used by
#'  [semlbci()] but is exported such that the compatibility of an SEM
#'  output can be checked directly.
#'
#' Estimation methods (`estimator` in [lavaan::lavaan()]) currently
#'     supported:
#'
#'    - Maximum likelihood (`ML`) and its variants (e.g., `MLM`, `MLR`).
#'      For methods with robust test statistics (e.g., `MLR`),
#'      only robust LBCIs (`robust = "satorra.2000"` in calling [semlbci()])
#'      can be requested.
#'
#' Estimation methods not yet supported:
#'
#'    - Generalized least squares (`GLS`).
#'
#'    - Weighted least squares (a.k.a. asymptotically distribution
#'         free) (`WLS`) and its variants (e.g., `WLSMV`).
#'
#'    - Unweighted least squares (`ULS`).
#'
#'    - Diagonally weighted least squares (`DWLS`).
#'
#'    - Other methods not listed.
#'
#' Models supported:
#'
#'    - Single-group models with continuous variables.
#'
#'    - Multiple-group models with continuous variables.
#'
#' Models not tested:
#'
#'    - Models with categorical variables.
#'
#' Models not yet supported:
#'
#'    - Models with formative factors.
#'
#'    - Multilevel models.
#'
#' @return A numeric vector of one element. If 0, the model and
#'  estimation method are officially supported. If larger than zero,
#'  then the model and method are not officially supported but users
#'  can still try to use [semlbci()] on it at their own risks. If less
#'  than zero, then the model and/or the method are officially not
#'  supported.
#'
#' The attributes `info` contains the reason for a value other than
#' zero.
#'
#' @param sem_out The output from an SEM analysis. Currently only
#'  supports a [lavaan::lavaan-class] object.
#'
#' @param robust Whether the LBCI based on robust likelihood ratio
#'  test is to be found. Only "satorra.2000" in [lavaan::lavTestLRT()]
#'  is supported for now. If `"none"`, the default, then likelihood
#'  ratio test based on maximum likelihood estimation will be used.
#'
#' @param multigroup_ok If `TRUE`, will not check whether the model is a
#'  multiple-group model. Default is `TRUE`.
#'
#' @seealso
#' [semlbci()], [ci_i_one()]
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
#' fit2 <- sem(mod, cfa_two_factors, estimator = "DWLS")
#'
#' # Should be negative because DWLS is officially not supported
#' check_sem_out(fit2)
#'
#' fit3 <- sem(mod, cfa_two_factors, estimator = "MLR")
#'
#' # Should be negative because MLR is supported only if
#' # robust is set to "satorra.2000"
#'
#' check_sem_out(fit3, robust = "satorra.2000")
#'
#' # Should be zero because robust is set to "satorra.2000"
#'
#' @export

check_sem_out <- function(sem_out,
                          robust = c("none", "satorra.2000"),
                          multigroup_ok = TRUE) {
    robust <- match.arg(robust)
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
    sem_likelihood <- sem_options$likelihood

    # Only check against methods explicitly supported
    # Note that it checks the `estimator` in options.
    # E.g., if estimator = "MLM" in the call, the `estimator` in options is
    # still "ML".
    # Therefore, variants of ML, e.g., MLR and mLR, are supported.
    # Documented estimator: DLS, DWLS, GLS, ML, PML, ULS, WLS
    estimators_supported <- c("ML")
    estimators_unsupported <- c("GLS",
                                "WLS",
                                "DLS",
                                "DWLS",
                                "ULS",
                                "PML")
    # Documented se: standard, robust.huber.white, robust.se, boot
    # If robust LBCI requested, this will be ignored.
    # Not used. Included just in case this need to be checked in the future.
    se_supported <- c("standard")

    # If normal LBCI is requested, test must be "standard".
    # If robust LBCI is requested, another test will be conducted.
    test_supported <- c("standard")
    test_unsupported <- c("Satorra.Bentler",
                          "Yuan.Bentler",
                          "Yuan.Bentler.Mplus",
                          "scaled.shifted",
                          "boot",
                          "bootstrap",
                          "Bollen.Stine")

    # Note that it checks the `missing` in options.
    missing_supported <- c("listwise",
                           "ml", "fiml", "direct",
                           "ml.x", "fiml.x", "direct.x")
    missing_unsupported <- c("two.stages", "robust.two.stages",
                             "pairwise",
                             "available.cases",
                             "doubly.robust")

    estimator_ok <- (tolower(sem_estimator) %in% tolower(estimators_supported))
    missing_ok <- (tolower(sem_missing) %in% tolower(missing_supported))
    # se checked but not used
    se_ok <- (tolower(sem_se) %in% tolower(se_supported))

    scaled <- any(names(lavaan::lavInspect(sem_out, "test")) %in%
                        c("satorra.bentler",
                          "yuan.bentler",
                          "yuan.bentler.mplus",
                          "mean.var.adjusted",
                          "scaled.shifted"))

    if (robust == "satorra.2000") {
        # If robust LBCI requested, at least one scaled test must be used.
        if (scaled) {
          robust_ok <- TRUE
          test_ok <- TRUE
        } else {
          robust_ok <- FALSE
          test_ok <- FALSE
        }
      } else {
        # If normal LBCI requested, the test must be "standard".
        robust_ok <- NA
        test_ok <- identical(tolower(sem_test), "standard")
      }


    model_formative_factor <- "<~" %in% p_table$op
    model_multilevel <- (sem_nlevels > 1)
    model_multicluster <- (sem_max_nclusters > 1)
    model_multigroup <- (sem_ngroups > 1)
    model_ordered <- (length(sem_lavaan_ordered) > 0)

    optim_converged <- sem_converged
    optim_admissible <- sem_post_check

    # out < 0 if there is at least one problem
    out <- 0
    msg <- NULL

    if (!estimator_ok) {
          out <- ifelse(out >= 0, -1, out - 1)
          msg <- c(msg, paste("Estimator", sem_estimator,
                                "is not yet supported."))
        }

    if (!missing_ok) {
          out <- ifelse(out >= 0, -1, out - 1)
          msg <- c(msg, paste("Missing handling method", sem_missing,
                                "is not yet supported."))
        }

    if (robust == "satorra.2000") {
        if (!test_ok) {
            out <- ifelse(out >= 0, -1, out - 1)
            msg <- c(msg, paste("Robust LBCIs are requested.",
                                "However, test method(s)",
                                paste0(sem_test, collapse = ", "),
                                "is/are not yet supported."))
            }
      } else {
        if (!test_ok) {
            out <- ifelse(out >= 0, -1, out - 1)
            msg <- c(msg, paste("Test method(s)",
                                paste0(sem_test, collapse = ", "),
                                "is/are not yet supported when 'robust' is 'none'."))
            }
      }

    if (model_formative_factor) {
          out <- ifelse(out >= 0, -1, out - 1)
          msg <- c(msg,
                    "Models with formative factor(s) are not yet supported.")
        }

    if (model_multilevel) {
          out <- ifelse(out >= 0, -1, out - 1)
          msg <- c(msg, "Multilevel models are not yet supported.")
        }

    if (model_multicluster) {
          out <- ifelse(out >= 0, -1, out - 1)
          msg <- c(msg, "Clustered models are not yet supported.")
        }

    if (model_multigroup && !multigroup_ok) {
          out <- ifelse(out >= 0, -1, out - 1)
          msg <- c(msg, "Multigroup models are not yet supported.")
        }

    if (model_ordered) {
          out <- ifelse(out >= 0, out + 1, out)
          msg <- c(msg,
                    paste("Not fully tested on models with ordered variables.",
                           "Use the function at your own risk."))
        }

    if (!optim_converged) {
          out <- ifelse(out >= 0, -1, out - 1)
          msg <- c(msg,
                  "The estimation has not converged. Fix the estimation first.")
        }

    if (!optim_admissible) {
          out <- ifelse(out >= 0, -1, out - 1)
          msg <- c(msg,
                paste("The solution is not admissible by lavaan post.check.",
                        "Check the SEM results first."))
        }

    if (sem_likelihood != "normal") {
          out <- ifelse(out >= 0, -1, out - 1)
          msg <- c(msg,
                paste("Only support models fitted with likelihood set to 'normal'."))
        }

    attr(out, "info") <- msg
    out
  }
