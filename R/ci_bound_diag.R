#'@title Print diagnotic informantion of a confidence limit
#'
#'@description Print diagnotic informantion of a confidence limit
#'
#'@details Print diagnotic informantion of a confidence limit
#'
#'@return
#'  Nothing
#'
#'@param out The output of ci_bound_xx_i function.
#'
#'@examples
#' # TODO
#' 
#' @export

ci_bound_diag <- function(out, digits = 5, ...) {
    call_org <- attr(out, "call")
    out_diag <- attr(out, "diag")
    ci_method <- switch(out_diag$method, wn = "Wu-Neale-2012")
        if (is.null(out_diag$standardized)) {
            std <- "No"
          } else {
            std <- ifelse(out_diag$standardized, "Yes", "No")
          }
    lor <- out_diag$i_lor
    lor2 <- paste0(lor$lhs, " ", lor$op, " ", lor$rhs, " (group = ",
                   lor$group, ", block = ", lor$block, ")")
    cat(paste0("Target Parameter:\t", lor2))
    cat(paste0("\nPosition:\t\t", out_diag$i))
    cat(paste0("\nWhich limit:\t\t", switch(out_diag$which, 
                                                  lbound = "Lower limit",
                                                  ubound = "Upper limit")))
    cat(paste0("\nMethod:\t\t\t", ci_method))
    cat(paste0("\nConfidence Level:\t", out_diag$ciperc))
    cat(paste0("\nAchieved Level:\t\t", out_diag$ciperc_final))
    cat(paste0("\nStandardized:\t\t", std))
    cat(paste0("\nLikelihood-Based Limit:\t", ifelse(is.na(out), "Not valid", round(as.numeric(out), digits))))
    cat(paste0("\nWald limit:\t\t", round(out_diag$ci_org_limit, digits)))
    cat(paste0("\nPoint Estimate:\t\t", round(out_diag$est_org, digits)))
    cat(paste0("\nRatio to Wald limit:\t", ifelse(is.na(out), "Not valid", round(out_diag$ci_limit_ratio, digits))))
    # cat("\n\t(Distance from the point estiamte)")
    cat(paste0("\n\n-- Check --"))
    ciperc_diff <- abs(out_diag$ciperc - out_diag$ciperc_final)
    cat(paste0("\nLevel achieved?\t\t", ifelse(ciperc_diff < 1e-5, "Yes", "No"), " (", ciperc_diff, ")"))
    cat(paste0("\nSolution admissible?\t", ifelse(out_diag$fit_post_check, "Yes", "No")))
    if (is.na(out)) {
        direct_valid <- "Not valid"
      } else {
        direct_valid <- switch(out_diag$which,
                          lbound = ifelse(out < out_diag$est_org, "Yes", "No"),
                          ubound = ifelse(out > out_diag$est_org, "Yes", "No")
                        )
      }
    cat(paste0("\nDirection valid?\t", direct_valid))
    cat("\n")

    cat("\n-- Optimization Information --")
    cat(paste0("\nSolver Status:\t\t", out_diag$optim_status))
    cat(paste0("\nConvergence Message:\t", out_diag$optim_message))
    cat(paste0("\nIterations:\t\t", out_diag$optim_iterations))
    t_cond <- out_diag$optim_termination_conditions
    t_cond2 <- strsplit(t_cond, "\t")[[1]]
    t_cond2 <- gsub("maxeval: ", "maxeval:  ", t_cond2)
    tmp <- paste0(sapply(t_cond2, function(x) paste0("\t", x)), collapse = "\n")
    cat("\nTermination Conditions:\n")
    cat(tmp)
    coef_final <- out_diag$final_values
    coef_names <- names(coef_final)
    coef_c <- rbind(out_diag$start_values,
                    coef_final,
                    coef_final - out_diag$start_values)
    coef_c <- round(coef_c, digits)
    rownames(coef_c) <- c("Start", "Final", "Change")
    cat("\n")
    cat("\n-- Parameter Estimates --\n")
    print(coef_c)
        
    cat(paste0("\nLimit before check:\t", round(out_diag$bound_unchecked, digits)))
    cat(paste0("\nStatus Code:\t\t", out_diag$status))
    cat("\nCall: ")
    print(call_org)
    cat("\n")
  }
