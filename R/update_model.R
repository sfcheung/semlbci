#'@title Update a lavModel objec using user-supplied estimates
#'
#'@description Update a \code{lavaan} model using user-supplied estimates
#'
#'@details 
#' 
#' Modified from lav_model_set_parameters in lavaan.
#' 
#' Obviously, supports \code{lavaan} output only.
#'
#'@return
#' An updated lavModel object.
#' 
#' @param model A lavModel object to be updated.
#' @param est User supplied parameter estimates, as in the parameter table.
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

update_model <- function(model, est) {
  # Modified from lav_model_set_parameters in lavaan
    if (!inherits(model, "lavModel")) {
        stop("model must be of the class lavModel in the lavaan package.")
      }
    out <- model@GLIST
    if (model@categorical) {
        stop("Not yet support a model with categorical variables.")
      }
    for (i in seq_len(length(model@GLIST))) {
        m_free <- model@m.free.idx[[i]]
        x_free <- model@x.free.idx[[i]]
        out[[i]][m_free] <- est[x_free]
      }
    model@GLIST <- out
    model
  }  
