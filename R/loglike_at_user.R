#' @param standardized Logical. Whether
#' the parameter requested is in the
#' standardized solution. Default is
#' `FALSE`.
#'
#' @param loadbalancing Logical. When
#' using parallel processing, whether
#' load balancing is used. Default is
#' `TRUE`.
#'
#' @describeIn loglikelihood Generates
#' points for log profile likelihood and
#' quadratic approximation using root
#' finding, by calling the helper
#' functions [loglike_range_ur()] and
#' [loglike_quad_range_ur()].
#'
#' @order 6
#' @export

loglike_compare_ur <- function(sem_out,
                               semlbci_out = NULL,
                               par_i,
                               confidence = .95,
                               n_points = 21,
                               standardized = FALSE,
                               parallel = FALSE,
                               ncpus = parallel::detectCores(logical = FALSE) - 1,
                               use_pbapply = TRUE,
                               loadbalancing = TRUE) {
    if (is.character(par_i)) {
        par_i <- syntax_to_i(par_i, sem_out)
        if (length(par_i) != 1) {
            stop("par_i must denote one parameter.")
          }
      }
    par_i_name <- i_to_name(par_i, sem_out)
    if (standardized) {
        ptable <- lavaan::standardizedSolution(sem_out)
        est <- ptable$est.std[par_i]
        se <- ptable$se[par_i]
      } else {
        ptable <- lavaan::parameterTable(sem_out)
        est <- ptable$est[par_i]
        se <- ptable$se[par_i]
      }
    z <- stats::qnorm(1 - (1 - confidence) / 2)
    zs <- seq(-z, z, length.out = n_points)
    thetas_q <- est + se * zs
    if (is.null(semlbci_out)) {
        lbci_i <- semlbci(sem_out,
                          pars = par_i,
                          standardized = standardized,
                          ciperc = confidence,
                          method = "ur",
                          parallel = FALSE,
                          use_pbapply = use_pbapply)
      } else {
        lbci_i <- semlbci_out
      }
    lbci_range <- unlist(unname(stats::confint(lbci_i)[1, ]))
    thetas_l <- seq(lbci_range[1], lbci_range[2], length.out = n_points)
    thetas_0 <- sort(c(thetas_q, thetas_l))
    int_q <- thetas_0[which(thetas_0 == min(thetas_q)):which(thetas_0 == max(thetas_q))]
    int_l <- thetas_0[which(thetas_0 == min(thetas_l)):which(thetas_0 == max(thetas_l))]

    # Generate the user-parameter function
    # est_i_func <- gen_est_i(i = par_i,
    #                         sem_out = sem_out,
    #                         standardized = standardized)
    # fit_i <- add_func(func = est_i_func,
    #                   sem_out = sem_out)
    # tmp <- loglik_user(x = 1.2,
    #                    sem_out_userp = fit_i,
    #                    sem_out = sem_out)

    ll_q <- loglike_quad_range_ur(sem_out,
                                  par_i = par_i,
                                  standardized = standardized,
                                  interval = int_q,
                                  parallel = parallel,
                                  ncpus = ncpus,
                                  use_pbapply = use_pbapply,
                                  loadbalancing = loadbalancing)
    ll <- loglike_range_ur(sem_out,
                           par_i = par_i,
                           standardized = standardized,
                           interval = int_l,
                           parallel = parallel,
                           ncpus = ncpus,
                           use_pbapply = use_pbapply,
                           loadbalancing = loadbalancing)
    pvalue_q_lb <- loglike_point_ur(ll_q[1, "theta"],
                                    sem_out = sem_out,
                                    par_i = par_i,
                                    standardized = standardized)$pvalue
    pvalue_q_ub <- loglike_point_ur(ll_q[nrow(ll_q), "theta"],
                                    sem_out = sem_out,
                                    par_i = par_i,
                                    standardized = standardized)$pvalue
    pvalue_l_lb <- loglike_point_ur(ll[1, "theta"],
                                    sem_out = sem_out,
                                    par_i = par_i,
                                    standardized = standardized)$pvalue
    pvalue_l_ub <- loglike_point_ur(ll[nrow(ll), "theta"],
                                    sem_out = sem_out,
                                    par_i = par_i,
                                    standardized = standardized)$pvalue
    out <- list(quadratic = ll_q,
                loglikelihood = ll,
                pvalue_quadratic = c(pvalue_q_lb, pvalue_q_ub),
                pvalue_loglikelihood = c(pvalue_l_lb, pvalue_l_ub),
                par_name = par_i_name,
                est = est)
    class(out) <- "loglike_compare"
    out
  }

#' @describeIn loglikelihood Find the
#' log profile likelihood for a range of
#' values using root finding.
#'
#' @order 7
#' @export

loglike_range_ur <- function(sem_out,
                             par_i,
                             standardized = FALSE,
                             confidence = .95,
                             n_points = 21,
                             interval = NULL,
                             verbose = FALSE,
                             parallel = FALSE,
                             ncpus = parallel::detectCores(logical = FALSE) - 1,
                             use_pbapply = TRUE,
                             loadbalancing = TRUE) {
    if (use_pbapply && loadbalancing) {
        pboptions_old <- pbapply::pboptions(use_lb = TRUE)
        # Restore pboptions on exit
        on.exit(pbapply::pboptions(pboptions_old))
      }
    if (is.character(par_i)) {
        par_i <- syntax_to_i(par_i, sem_out)
        if (length(par_i) != 1) {
            stop("par_i must denote one parameter only.")
          }
      }
    if (is.null(interval)) {
        # Use Wald-type CI to determine the interval
        if (standardized) {
            ptable <- lavaan::parameterTable(sem_out)
            est <- ptable$est[par_i]
            se <- ptable$se[par_i]
          } else {
            ptable <- lavaan::standardizedSolution(sem_out)
            est <- ptable$est.std[par_i]
            se <- ptable$se[par_i]
          }
        z <- stats::qnorm(1 - (1 - confidence) / 2)
        zs <- seq(-z, z, length.out = n_points)
        thetas <- est + se * zs
      } else {
        if (length(interval) == 2) {
            # Form n_points points in the interval
            thetas <- seq(interval[1], interval[2], length.out = n_points)
          } else {
            # Use the interval as-is
            thetas <- interval
          }
      }
    if (parallel) {
        # Parallel
        cl <- parallel::makeCluster(ncpus)
        # Rebuild the environment
        pkgs <- .packages()
        pkgs <- rev(pkgs)
        parallel::clusterExport(cl, "pkgs", envir = environment())
        parallel::clusterEvalQ(cl, {sapply(pkgs,
                        function(x) library(x, character.only = TRUE))
                      })
        parallel::clusterExport(cl, ls(envir = parent.frame()),
                                       envir = parent.frame())
        parallel::clusterExport(cl, ls(envir = environment()),
                                       envir = environment())
        if (requireNamespace("pbapply", quietly = TRUE) &&
                    use_pbapply) {
            # Use pbapply
            if(use_pbapply) {
                cat("\n", "Finding p-values for LR test", "\n",
                    sep = "")
                utils::flush.console()
              }
            out <- pbapply::pblapply(thetas,
                                     semlbci::loglike_point_ur,
                                     sem_out = sem_out,
                                     par_i = par_i,
                                     verbose = verbose,
                                     standardized = standardized,
                                     cl = cl)
          } else {
            # No progress bar
            out <- parallel::parLapplyLB(cl = cl,
                                         thetas,
                                         semlbci::loglike_point_ur,
                                         sem_out = sem_out,
                                         par_i = par_i,
                                         verbose = verbose,
                                         standardized = standardized)
          }
        parallel::stopCluster(cl)
      } else {
        # Serial
        if (requireNamespace("pbapply", quietly = TRUE) &&
                    use_pbapply) {
            # Use pbapply
            if(use_pbapply) {
                cat("\n", "Finding p-values for LR test", "\n",
                    sep = "")
                utils::flush.console()
              }
            out <- pbapply::pblapply(thetas,
                                     loglike_point_ur,
                                     sem_out = sem_out,
                                     par_i = par_i,
                                     verbose = verbose,
                                     standardized = standardized)
          } else {
            # No progress bar
            out <- lapply(thetas,
                          FUN = loglike_point_ur,
                          sem_out = sem_out,
                          par_i = par_i,
                          verbose = verbose,
                          standardized = standardized)
          }
      }
    out_final <- data.frame(theta = thetas,
                            loglike = sapply(out, function(x) x$loglike),
                            pvalue = sapply(out, function(x) x$pvalue))
    out_final
  }

#' @describeIn loglikelihood Find the
#' log likelihood at a value.
#'
#' @order 8
#' @export

loglike_point_ur <- function(theta0,
                             sem_out,
                             par_i,
                             standardized = FALSE,
                             verbose = FALSE) {
    if (is.character(par_i)) {
        par_i <- syntax_to_i(par_i, sem_out)
        if (length(par_i) != 1) {
            stop("par_i must denote one parameter only.")
          }
      }
    est_i_func <- gen_est_i(i = par_i,
                            sem_out = sem_out,
                            standardized = standardized)
    fit_i <- add_func(func = est_i_func,
                      sem_out = sem_out)
    out <- loglik_user(x = theta0,
                       sem_out_userp = fit_i,
                       sem_out = sem_out)
    lrt <- attr(out, "lrt")
    if (verbose) print(lrt)
    loglike <- as.vector(out)
    p <- lrt[2, "Pr(>Chisq)"]
    out <- list(loglike = loglike,
                pvalue = p,
                fit = attr(out, "sem_out_userp_x"),
                lrt = lrt)
    out
  }

#' @describeIn loglikelihood Find the
#' approximated log likelihood for a
#' range of values using root finding.
#'
#' @order 9
#' @export

loglike_quad_range_ur <- function(sem_out,
                                  par_i,
                                  confidence = .95,
                                  standardized = FALSE,
                                  n_points = 21,
                                  interval = NULL,
                                  parallel = FALSE,
                                  ncpus = parallel::detectCores(logical = FALSE) - 1,
                                  use_pbapply = TRUE,
                                  loadbalancing = TRUE) {
    if (use_pbapply && loadbalancing) {
        pboptions_old <- pbapply::pboptions(use_lb = TRUE)
        # Restore pboptions on exit
        on.exit(pbapply::pboptions(pboptions_old))
      }
    if (is.character(par_i)) {
        par_i <- syntax_to_i(par_i, sem_out)
        if (length(par_i) != 1) {
            stop("par_i must denote one parameter only.")
          }
      }
    if (is.null(interval)) {
        ptable <- lavaan::parameterTable(sem_out)
        est <- ptable$est[par_i]
        se <- ptable$se[par_i]
        z <- stats::qnorm(1 - (1 - confidence) / 2)
        zs <- seq(-z, z, length.out = n_points)
        thetas <- est + se * zs
      } else {
        if (length(interval) == 2) {
            thetas <- seq(interval[1], interval[2], length.out = n_points)
          } else {
            thetas <- interval
          }
      }
    out <- sapply(thetas,
                  FUN = loglike_quad_point_ur,
                  sem_out = sem_out,
                  par_i = par_i,
                  standardized = standardized)
    if (parallel) {
        # Parallel
        cl <- parallel::makeCluster(ncpus)
        # Rebuild the environment
        pkgs <- .packages()
        pkgs <- rev(pkgs)
        parallel::clusterExport(cl, "pkgs", envir = environment())
        parallel::clusterEvalQ(cl, {sapply(pkgs,
                        function(x) library(x, character.only = TRUE))
                      })
        parallel::clusterExport(cl, ls(envir = parent.frame()),
                                       envir = parent.frame())
        parallel::clusterExport(cl, ls(envir = environment()),
                                       envir = environment())
        if (requireNamespace("pbapply", quietly = TRUE) &&
                    use_pbapply) {
            # Use pbapply
            if(use_pbapply) {
                cat("\n", "Finding p-values for quadratic approximation", "\n",
                    sep = "")
                utils::flush.console()
              }
            pvalues <- pbapply::pbsapply(thetas,
                                  function(x,
                                           sem_out,
                                           par_i,
                                           standardized = standardized) {
                                      semlbci::loglike_point_ur(x,
                                          sem_out = sem_out,
                                          par_i = par_i,
                                          standardized = standardized)$pvalue
                                    },
                                sem_out = sem_out,
                                par_i = par_i,
                                standardized = standardized,
                                cl = cl)
          } else {
            # No progress bar
            pvalues <- parallel::parSapplyLB(cl = cl,
                                  thetas,
                                  function(x,
                                           sem_out,
                                           par_i,
                                           standardized = standardized) {
                                      semlbci::loglike_point_ur(x,
                                          sem_out = sem_out,
                                          par_i = par_i,
                                          standardized = standardized)$pvalue
                                    },
                                sem_out = sem_out,
                                par_i = par_i,
                                standardized = standardized)
          }
        parallel::stopCluster(cl)
      } else {
        # Serial
        if (requireNamespace("pbapply", quietly = TRUE) &&
                    use_pbapply) {
            # Use pbapply
            if(use_pbapply) {
                cat("\n", "Finding p-values for quadratic approximation", "\n",
                    sep = "")
                utils::flush.console()
              }
            pvalues <- pbapply::pbsapply(thetas, function(x) {
                                  loglike_point_ur(theta0 = x,
                                                   sem_out = sem_out,
                                                   par_i = par_i,
                                                   standardized = standardized)$pvalue
                                })
          } else {
            # No progress bar
            pvalues <- sapply(thetas, function(x) {
                                  loglike_point_ur(x,
                                                   sem_out = sem_out,
                                                   par_i = par_i,
                                                   standardized = standardized)$pvalue
                                })
          }
      }

    out_final <- data.frame(theta = thetas,
                            loglike = out,
                            pvalue = pvalues)
    out_final
  }

#' @describeIn loglikelihood Find the
#' approximated log likelihood at a
#' value. Support a parameter in the
#' standardized solution.
#'
#' @order 10
#' @export

loglike_quad_point_ur <- function(theta0,
                                  sem_out,
                                  par_i,
                                  standardized = FALSE) {
    if (is.character(par_i)) {
        par_i <- syntax_to_i(par_i, sem_out)
        if (length(par_i) != 1) {
            stop("par_i must denote one parameter only.")
          }
      }
    if (standardized) {
        est0 <- lavaan::standardizedSolution(sem_out)[par_i, ]
        est <- as.vector(est0[, "est.std"])
      } else {
        est0 <- lavaan::parameterEstimates(sem_out)[par_i, ]
        est <- as.vector(est0[, "est"])
      }
    p_info <- 1 / est0[, "se"]^2
    -.5 * p_info * (theta0 - est) ^ 2
  }

# #' @describeIn loglikelihood Fit a
# #' parameter to a value and compare the
# #' restricted model with the original
# #' model by a likelihood ratio test.
# #' A helper function for other
# #'
# #' @order 11
# #' @export
#' @noRd

loglik_user <- function(x,
                        sem_out_userp,
                        sem_out,
                        lrt_method = "default",
                        ...) {
    sem_out_userp_tmp <- sem_out_userp_run(target = x,
                                           object = sem_out_userp,
                                           ...)
    lrt_x <- tryCatch(lavaan::lavTestLRT(sem_out_userp_tmp,
                                         sem_out,
                                         method = lrt_method),
                      error = function(e) e,
                      warning = function(w) w)
    if (inherits(lrt_x, "warning")) {
        tmp <- as.character(lrt_x)
        if (grepl("scaling factor is negative",
                  tmp, fixed = TRUE)) {
            lrt_x <- tryCatch(lavaan::lavTestLRT(sem_out_userp_tmp,
                                                 sem_out,
                                                 method = "satorra.2000"),
                              error = function(e) e,
                              warning = function(w) w)
          }
      }
    out <- lrt_x[2, "Chisq diff"] / (-2)
    attr(out, "sem_out_userp_x") <- sem_out_userp_tmp
    attr(out, "lrt") <- lrt_x
    out
  }