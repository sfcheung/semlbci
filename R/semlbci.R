#'@title Find the LBCIs for all free parameters in an SEM output
#'
#'@description Find the LBCIs for all free parameters in an SEM output
#'
#'@details
#'
#' Currently supports \code{lavaan} output only.
#'
#'@return
#' The LBCIs for all free parameters
#'
#' @param sem_out The SEM output. Currently \code{lavaan} output only.
#' @param pars Positions of the parameters for which the LBCI to be found.
#'              Use the position as appeared on the parameter tables of the fit object.
#'              If NULL, the default, then LBCIs will be found for all free parameters.
#'              Can also be a vector of strings to indicate the paramters on the 
#'              parameter table.
#' @param ciperc The proportion of coverage for the confidence interval. Default
#'               is .95.
#' @param standardized If TRUE, the LBCI is for the standardized estimate.
#' @param method The approach to be used. Can be "wn" (Wu-Neale-2012) or "nm" 
#'               (Neale-Miller-1997). Default is "wn".
#' @param ... Arguments to be passed to \code{ci_bound_i}.
#' @param parallel If \code{TRUE}, will use parallel. Currently disabled.
#'                  Need to find out how to make \code{lavaan::udpate} works
#'                  currently in the workers.
#' @param ncpu The number of workers, if parallel is TRUE.
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
#'@export

semlbci <- function(sem_out,
                    pars = NULL,
                    ciperc = .95,
                    standardized = FALSE,
                    method = "wn",
                    ...,
                    parallel = FALSE,
                    ncpu = 2) {
    if (!inherits(sem_out, "lavaan")) {
        stop("sem_out is not a supported object.")
      }
    ptable <- lavaan::parameterTable(sem_out)
    # Do not check for 
    i <- ptable$free > 0
    #i_id <- ptable$id[i]
    i_id <- ptable$id
    # pars must be the position as in the lavaan parameterTable.
    if (!is.null(pars)) {
        if (is.character(pars)) {
            pars <- syntax_to_i(pars, sem_out)
          }
        i_selected <- i_id[pars]
      } else {
        pars <- seq_len(sum(i))
        i_selected <- i_id[pars]
      }
    npar <- sum(i)
    # environment(set_constraint) <- parent.frame()
    if (method == "wn") {
        f_constr <- eval(set_constraint(sem_out = sem_out, ciperc = ciperc),
                        envir = parent.frame())
      } else {
        f_constr <- NULL
      }
    if (parallel) {
        message("Parallel processing is currently disabled.")
      }
    if (FALSE) {        
        # cl <- parallel::makeCluster(ncpu)
        # pkgs <- .packages()
        # pkgs <- rev(pkgs)
        # parallel::clusterExport(cl, "pkgs", envir = environment())
        # parallel::clusterEvalQ(cl, {sapply(pkgs, 
        #                 function(x) library(x, character.only = TRUE))
        #               })
        # parallel::clusterExport(cl, "f_constr")
        # parallel::clusterExport(cl, "npar")
        # out <- parallel::parLapply(cl,
        #                  pars,
        #                   function(x, ...) tryCatch(
        #                     semlbci::ci_i(x,
        #                       npar = npar,
        #                       sem_out = sem_out,
        #                       debug = FALSE,
        #                       f_constr = f_constr),
        #                       error = function(e) e
        #                   ),
        #                   ...
        #                  )
        # parallel::stopCluster(cl)
        # out_error <- sapply(out, inherits, what = "error")
        # if (any(out_error)) {
        #     stop("Error occured in parallel mode. Try setting parallel = FALSE")
        #   }
      } else {
        out_raw <- lapply(pars, ci_i, 
                      npar = npar,
                      sem_out = sem_out,
                      standardized = standardized,
                      debug = FALSE,
                      f_constr = f_constr,
                      method = method,
                      ciperc = ciperc,
                      ...)
      }
    out <- do.call(rbind, out_raw)
    out_p <- ptable[, c("id", "lhs", "op", "rhs")]
    out_p$lbci_lb <- NA
    if (standardized) {
        pstd <- lavaan::standardizedSolution(sem_out)
        out_p <- merge(out_p, pstd[, c("lhs", "op", "rhs", "est.std")], 
                by = c("lhs", "op", "rhs"), all.x = TRUE, sort = FALSE)
      } else {
        out_p$est <- ptable[, c("est")]        
      }
    out_p$lbci_ub <- NA
    
    out_p[i_selected, "lbci_lb"] <- out[, 1]
    out_p[i_selected, "lbci_ub"] <- out[, 2]

    # Collect diagnostic info

    lb_diag <- lapply(out_raw, attr, which = "lb_diag")
    ub_diag <- lapply(out_raw, attr, which = "ub_diag")
    p_names <- mapply(paste0, out_p$lhs, out_p$op, out_p$rhs,
                      USE.NAMES = FALSE)
    names(lb_diag) <- p_names
    names(ub_diag) <- p_names
    
    attr(out_p, "lb_diag") <- lb_diag
    attr(out_p, "ub_diag") <- ub_diag

    # Append diagnostic info

    out_p$status_lb <- sapply(lb_diag, function(x) x$status)
    out_p$status_ub <- sapply(ub_diag, function(x) x$status)
    out_p$ci_org_lb <- sapply(lb_diag, function(x) x$ci_org_limit)
    out_p$ci_org_ub <- sapply(ub_diag, function(x) x$ci_org_limit)
    out_p$ratio_lb <- sapply(lb_diag, function(x) x$ci_limit_ratio)
    out_p$ratio_ub <- sapply(ub_diag, function(x) x$ci_limit_ratio)
    out_p$post_check_lb <- sapply(lb_diag, function(x) x$fit_post_check)
    out_p$post_check_ub <- sapply(ub_diag, function(x) x$fit_post_check)

    class(out_p) <- c("semlbci", class(out_p))
    out_p
  }