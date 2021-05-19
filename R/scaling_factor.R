#' @title Find the scaling factor used in satorra-2000 test
#'
#' @description Find the scaling factor used in satorra-2000
#'              test.
#'
#' @return
#' The scaling factor
#'
#' @param sem_out The source fit object.
#' @param i The position of the parameter as appeared in the
#'          parameter table.
#' @param standardized If `TRUE`, the limit to be foound is for the standardized
#'                     solution. Default is `FALSE`.
#' @param pertubation_factor A factor to modify the original estimate. Default
#'                           is .98.
#'
#' @examples
#' # TODO
#'
#' @keywords internal

scaling_factor <- function(sem_out,
                           i,
                           standardized = FALSE,
                           pertubation_factor = .98
                           ) {
    # This function will NOT check whether the SEM was done with robust model
    # test. This check should be done before calling this function.
    p_table <- lavaan::parameterTable(sem_out)
    npar <- sum(p_table$free > 0)
    i_op <- p_table[i, "op"]
    i_lor <- get_lhs_op_rhs(i, sem_out, more = TRUE)
    i_labelled <- nchar(p_table[i, "label"]) > 0

    # Get original point estiamte and CI
    if (standardized) {
        p_est <- lavaan::standardizedSolution(sem_out,
                    type = "std.all",
                    se = TRUE,
                    zstat = FALSE,
                    pvalue = FALSE,
                    ci = TRUE,
                    remove.eq = FALSE,
                    remove.ineq = FALSE,
                    remove.def = FALSE,
                    output = "data.frame")
        i_est <- p_est[i, "est.std"]
        i_se <- p_est[i, "se"]
        i_org_ci_lower <- p_est[i, "ci.lower"]
        i_org_ci_upper <- p_est[i, "ci.upper"]
      } else {
        p_est <- lavaan::parameterEstimates(sem_out,
                                            se = TRUE,
                                            zstat = FALSE,
                                            fmi = FALSE,
                                            rsquare = TRUE,
                                            output = "data.frame")
        i_est <- p_est[i, "est"]
        i_se <- p_est[i, "se"]
        i_org_ci_lower <- p_est[i, "ci.lower"]
        i_org_ci_upper <- p_est[i, "ci.upper"]
      }

    p_table_fit <- p_table

    if (standardized) {

        # Standardized. User defined or free does not matter.

        i_label <- gen_unique_name(get_names_from_ptable(p_table_fit))

        # Need to define this function here to prevent possible scoping issues
        gen_fct <- function(fit, i) {
            force(fit)
            force(i)
            fit_pt <- lavaan::parameterTable(fit)
            tmpfct <- function(...) {
                .x. <- get(".x.", envir = parent.frame())
                fit@Model <- lavaan::lav_model_set_parameters(
                                  fit@Model, .x.
                                )
                fit_pt2 <- fit_pt
                nfree <- sum(fit_pt$free > 0)
                fit_pt2[fit_pt$free > 0, "est"] <- .x.[seq_len(nfree)]
                fit@ParTable <- as.list(fit_pt2)
                std <- lavaan::standardizedSolution(
                                  fit,
                                  se = FALSE,
                                  zstat = FALSE,
                                  pvalue = FALSE,
                                  ci = FALSE,
                                  cov.std = FALSE,
                                  remove.eq = FALSE,
                                  remove.ineq = FALSE,
                                  remove.def = FALSE,
                                  )
                std[i, "est.std"]
              }
            return(tmpfct)
          }

        # Using `<<-` is not a desired approach. If we found a solution using
        # environment, we should use that solution rather than `<<-`.
        # geteststd <- gen_fct(fit = sem_out, i = i)
        # browser()
        # geteststd_name <- gen_unique_name(ls(pos = .GlobalEnv))
        # update_env <- new.env(parent = .GlobalEnv)
        # assign(geteststd_name, gen_fct(fit = sem_out, i = i),
        #        pos = update_env)
        # browser()
        update_env <- .GlobalEnv
        geteststd_name <- "geteststd"
        while (geteststd_name %in% names(update_env)) {
            geteststd_name <- paste(geteststd_name, sample(letters, 1))
          }
        assign(geteststd_name, gen_fct(fit = sem_out, i = i),
               pos = update_env)
        fit0 <- lavaan::update(sem_out,
                               model = p_table_fit,
                              #  add = paste0(i_label, " := ",
                              #               "get_std_i(i = ",
                              #               i,
                              #               ")"),
                              #  add = paste0(i_label, " := ",
                              #               geteststd_name, "()"),
                               add = paste0(i_label, " := geteststd()"),
                               do.fit = FALSE,
                               baseline = FALSE,
                               h1 = FALSE,
                               se = "none",
                               test = "satorra.bentler")
        fit0 <- lavaan::update(fit0,
                               add = paste0(i_label, " == ",
                                            i_est * pertubation_factor),
                               do.fit = FALSE)
        fit0 <- lavaan::update(fit0,
                               add = "0 < 1",
                               do.fit = FALSE)
        p_table0 <- lavaan::parameterTable(fit0)
        i_std <- which(p_table0$lhs == i_label & p_table0$op == ":=")
        i_constr <- which(p_table0$lhs == i_label & p_table0$op == "==")
        i_extra <- which(p_table0$lhs == "0" & p_table0$rhs == "1")
        # Not sure why we have to manually set this constraint to free
        p_table0[i_std, "free"] <- 0
        p_table0[i_constr, "free"] <- 0
        p_table0[i_extra, "free"] <- 0
        p_table0[p_table0$free > 0, "start"] <-
                                          p_table[p_table0$free > 0, "est"]
        p_table0[p_table0$free > 0, "est"] <-
                                          p_table[p_table0$free > 0, "est"]
        fit1 <- lavaan::update(fit0,
                               model = p_table0,
                               do.fit = TRUE,
                               optim.force.converged = TRUE,
                               optim.dx.tol = .01,
                               warn = FALSE,
                               control = list(
                                    eval.max = 2,
                                    iterations = 1,
                                    control.outer = list(tol = 1e-02,
                                                         itmax = 1)
                                )
                              )
      } else {

        # Unstandardized. User defined or not does not matter
        if (!i_labelled) {
            i_label <- gen_unique_name(get_names_from_ptable(p_table_fit))
            p_table_fit[i, "label"] <- i_label
          } else {
            i_label <- p_table_fit[i, "label"]
          }

        fit0 <- lavaan::update(sem_out, model = p_table_fit,
                               add = paste0(i_label, " == ",
                                            i_est * pertubation_factor),
                               do.fit = FALSE,
                               baseline = FALSE,
                               h1 = FALSE,
                               se = "none")
        p_table0 <- lavaan::parameterTable(fit0)
        # # Not sure why we have to manually set this constraint to fixed
        p_table0[p_table0$lhs == i_label, "free"] <- 0
        i_constr <- which(p_table0$lhs == i_label & p_table0$op == "==")
        # p_table0[p_table0$free > 0, "est"] <- lavaan::coef(sem_out)
        fit1 <- lavaan::update(fit0,
                               model = p_table0,
                               do.fit = TRUE,
                               optim.force.converged = TRUE,
                               optim.dx.tol = .01,
                               warn = FALSE,
                               control = list(
                                    eval.max = 2,
                                    iterations = 1,
                                    control.outer = list(tol = 1e-02,
                                                         itmax = 1)
                                )
                              )
      }

    # Do the LR test

    lrt_out <- lavaan::lavTestLRT(sem_out,
                                  fit1,
                                  method = "satorra.2000",
                                  A.method = "exact")
    diff_from_p <- stats::qchisq(lrt_out[2, "Pr(>Chisq)"],
                                1,
                                lower.tail = FALSE)
    chisq_1 <- lrt_out["fit1", "Chisq"]
    chisq_0 <- lrt_out["sem_out", "Chisq"]
    chisq_diff_c <- chisq_1 - chisq_0
    chisq_diff_p <- stats::qchisq(lrt_out[2, "Pr(>Chisq)"],
                                  1,
                                  lower.tail = FALSE)
    chisq_diff_r <- lrt_out["fit1", "Chisq diff"]
    out <-
      data.frame(chisq_1 = chisq_1,
        chisq_0 = chisq_0,
        chisq_diff_c = chisq_diff_c,
        chisq_diff_r = chisq_diff_r,
        chisq_diff_p = chisq_diff_p,
        c_p = chisq_diff_c / chisq_diff_p,
        c_r = chisq_diff_c / chisq_diff_r)

    if (standardized) {
        # Try to make sure that only the function created above is removed
        tmp <- get(geteststd_name, pos = update_env)
        tmpl <- as.list(body(tmp))
        if (identical(as.character(tmpl[[length(tmpl)]]),
                      c("[", "std", "i", "est.std"))) {
            rm(list = geteststd_name, pos = update_env)
          }
      }

    return(out)
  }
