% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loglike_at.R
\name{loglike_compare}
\alias{loglike_compare}
\alias{loglike_range}
\alias{loglike_point}
\alias{loglike_quad_range}
\alias{loglike_quad_point}
\alias{loglikelihood}
\title{Log Profile likelihood of a Parameter}
\usage{
loglike_compare(
  sem_out,
  semlbci_out = NULL,
  par_i,
  confidence = 0.95,
  n_points = 21,
  start = "default",
  try_k_more = 5,
  parallel = FALSE,
  ncpus = parallel::detectCores(logical = FALSE) - 1,
  use_pbapply = TRUE
)

loglike_range(
  sem_out,
  par_i,
  confidence = 0.95,
  n_points = 21,
  interval = NULL,
  verbose = FALSE,
  start = "default",
  try_k_more = 5,
  parallel = FALSE,
  ncpus = parallel::detectCores(logical = FALSE) - 1,
  use_pbapply = TRUE
)

loglike_point(
  theta0,
  sem_out,
  par_i,
  verbose = FALSE,
  start = "default",
  try_k_more = 5
)

loglike_quad_range(
  sem_out,
  par_i,
  confidence = 0.95,
  n_points = 21,
  interval = NULL,
  parallel = FALSE,
  ncpus = parallel::detectCores(logical = FALSE) - 1,
  use_pbapply = TRUE,
  try_k_more = 5,
  start = "default"
)

loglike_quad_point(theta0, sem_out, par_i)
}
\arguments{
\item{sem_out}{The SEM output. Currently the outputs
of \code{\link[lavaan:lavaan]{lavaan::lavaan()}} or its wrappers, such as \code{\link[lavaan:sem]{lavaan::sem()}}
and \code{\link[lavaan:cfa]{lavaan::cfa()}} are supported.}

\item{semlbci_out}{The output of \code{\link[=semlbci]{semlbci()}}.
If supplied, it will extract the likelihood-based confidence
interval from the output. If not, it will call \code{\link[=semlbci]{semlbci()}}.}

\item{par_i}{The row number of the parameter in the output of
\code{\link[lavaan:lavParTable]{lavaan::parameterTable()}}. Can also be a \link[lavaan:model.syntax]{lavaan::model.syntax}
specification for a parameter, e.g., \code{"y ~ x"} or \verb{ab := }.
It will be converted to the row number by \code{\link[=syntax_to_i]{syntax_to_i()}}. Refer to
\code{\link[=syntax_to_i]{syntax_to_i()}} for details.}

\item{confidence}{The level of confidence of the Wald-type
confidence interval. If \code{interval} is \code{NULL}, this confidence is
used to form the interval.}

\item{n_points}{The number of points to be evaluated in the
interval. Default is 21.}

\item{start}{How the start values are set in \code{\link[lavaan:lavaan]{lavaan::lavaan()}}.
See \code{\link[lavaan:lavOptions]{lavaan::lavOptions()}} on this argument. Default is
\code{"default"}. If the plot is too irregular, try setting it to
\code{"simple"}.}

\item{try_k_more}{How many more times to try finding the p-values,
by randomizing the starting values. Default is 5. Try increasing
this number if the plot is too irregular.}

\item{parallel}{If \code{TRUE}, parallel processing will be used. A
cluster will be created by \code{\link[parallel:makeCluster]{parallel::makeCluster()}}, with the
number of workers equal to \code{ncpus}. Parallel processing, though
not enabled by default, is recommended because it can speed up
the computation a lot.}

\item{ncpus}{The number of workers if \code{parallel} is \code{TRUE}. Default
is \code{parallel::detectCores(logical = FALSE) - 1}, the number of
physical cores minus 1.}

\item{use_pbapply}{If \code{TRUE} and \link[pbapply:pbapply]{pbapply::pbapply} is installed,
\link[pbapply:pbapply]{pbapply::pbapply} will be used to display the progress in
computing the log profile likelihood. Default is \code{TRUE}.}

\item{interval}{A vector of numbers. If provided and has two
elements, this will be used as the end points of the interval. If
it has more than two elements, the elements will be used directly
to form the values in the interval. Default is \code{NULL}.}

\item{verbose}{Whether some diagnostic information will be printed.
Default is \code{FALSE}.}

\item{theta0}{The value at which the parameter is fixed to.}
}
\value{
\code{\link[=loglike_compare]{loglike_compare()}} calls \code{\link[=loglike_range]{loglike_range()}} and
\code{\link[=loglike_quad_range]{loglike_quad_range()}} and returns their results in a
\code{loglike_compare}-class object, a list
with these elements:
\itemize{
\item \code{quadratic}: The output of \code{\link[=loglike_quad_range]{loglike_quad_range()}}.
\item \code{loglikelihood}: The output of \code{\link[=loglike_range]{loglike_range()}}.
\item \code{pvalue_quadratic}: The likelihood ratio test \emph{p}-values at the
quadratic approximation confidence bounds.
\item \code{pvalue_loglikelihood}: The likelihood ratio test \emph{p}-values at
the likelihood-based confidence bounds.
\item \code{est}: The point estimate of the parameter in \code{sem_out}.
}

\code{loglike_compare}-class object has a \code{plot} method (\code{\link[=plot.loglike_compare]{plot.loglike_compare()}})
that can be used to plot the log profile likelihood.

\code{\link[=loglike_point]{loglike_point()}} returns a list with these elements:
\itemize{
\item \code{loglike}: The log profile likelihood of the parameter when it is
fixed to \code{theta0}.
\item \code{pvalue}: The \emph{p}-values based on the likelihood ratio difference
test between the original model and the model with the
parameter fixed to \code{theta0}.
\item \code{fit}: A \link[lavaan:lavaan-class]{lavaan::lavaan} object. The original model with
the parameter fixed to \code{theta0}.
\item \code{lrt}: The output of \code{\link[lavaan:lavTestLRT]{lavaan::lavTestLRT()}}, comparing the
original model to the model with the parameter fixed to
\code{theta0}.
}

\code{\link[=loglike_quad_range]{loglike_quad_range()}} returns a data frame with these
columns:
\itemize{
\item \code{theta}: The values to which the parameter is fixed to.
\item \code{loglike}: The log profile likelihood values of the parameter
using quadratic approximation.
\item \code{pvalue}: The \emph{p}-values based on the likelihood ratio difference
test between the original model and the model with the
parameter fixed to \code{theta}.
}

\code{\link[=loglike_quad_point]{loglike_quad_point()}} returns a single number of the class
\code{lavaan.vector} (because it is the output of
\code{\link[lavaan:fitMeasures]{lavaan::fitMeasures()}}). This number is the quadratic
approximation of the log profile likelihood when the parameter is
fixed to \code{theta0}.

\code{\link[=loglike_range]{loglike_range()}} returns a data frame with these columns:
\itemize{
\item \code{theta}: The values to which the parameter is fixed to.
\item \code{loglike}: The log profile likelihood at \code{theta}.
\item \code{pvalue}: The \emph{p}-values based on the likelihood ratio difference
test between the original model and model with the
parameter fixed to \code{theta}.
}
}
\description{
These functions compute the log profile likelihood of
a parameter when it is fixed to a value or a range of values
}
\details{
It uses the methods presented in Pawitan (2013) to
compute and visualize the log profile likelihood of a parameter in
a structural equation model when this parameter is fixed to a value or
a range
of values. \code{\link[=loglike_range]{loglike_range()}} and \code{\link[=loglike_point]{loglike_point()}} compute the
so-called "true" log profile likelihood, while
\code{\link[=loglike_quad_range]{loglike_quad_range()}} and \code{\link[=loglike_quad_point]{loglike_quad_point()}} approximate the log
profile likelihood by a quadratic function.

These functions are for creating illustrative examples and learning
only, not for research use. Therefore, they are not as versatile as
\code{\link[=semlbci]{semlbci()}} in the types of models and parameters supported. They
can be used for free parameters and user-defined parameters not
involved in any constraints. Only a model fitted by maximum
likelihood is supported.

They will not check whether the computation is appropriate for a
model. It is the responsibility of the users to ensure that the
computation is appropriate for the model and parameter.
}
\section{Functions}{
\itemize{
\item \code{loglike_compare()}: Generates points for log profile likelihood and
quadratic approximation, by calling the helper functions \code{loglike_range()}
and \code{loglike_quad_range()}.

\item \code{loglike_range()}: Find the log profile likelihood for a range of values.

\item \code{loglike_point()}: Find the log likelihood at a value.

\item \code{loglike_quad_range()}: Find the approximated log likelihood for a range of values.

\item \code{loglike_quad_point()}: Find the approximated log likelihood at a value.

}}
\examples{

## loglike_compare

library(lavaan)
data(simple_med)
dat <- simple_med
mod <-
"
m ~ a * x
y ~ b * m
ab := a * b
"
fit <- lavaan::sem(mod, simple_med, fixed.x = FALSE)

# 4 points are used just for illustration
# At least 21 points should be used for a smooth plot
# Remove try_k_more in real applications. It is set
# to zero such that this example does not take too long to run.
# use_pbapply can be removed or set to TRUE to show the progress.
ll_a <- loglike_compare(fit, par_i = "m ~ x", n_points = 4,
                        try_k_more = 0,
                        use_pbapply = FALSE)
plot(ll_a)

# See the vignette "loglike" for an example for the
# indirect effect.


## loglike_range

# Usually not to be used directly.
# Used by loglike_compare().
# 3 points are used just for illustration
ll_1 <- loglike_range(fit, par_i = "y ~ m", n_points = 2)
head(ll_1)


## loglike_point

# Usually not to be used directly.
# Used by loglike_compare().
llp_1 <- loglike_point(theta0 = 0.3, sem_out = fit, par_i = "y ~ m")
llp_1$loglike
llp_1$pvalue
llp_1$lrt



## loglike_quad_range

# Usually not to be used directly.
# Used by loglike_compare().
# 2 points are used just for illustration
lq_1 <- loglike_quad_range(fit, par_i = "y ~ m", n_points = 2)
head(lq_1)



## loglike_quad_point

# Usually not to be used directly.
# Used by loglike_compare().
lqp_1 <- loglike_quad_point(theta0 = 0.3, sem_out = fit, par_i = "y ~ m")
lqp_1


}
\references{
Pawitan, Y. (2013). \emph{In all likelihood: Statistical
modelling and inference using likelihood}. Oxford University Press.
}
\seealso{
\code{\link[=plot.loglike_compare]{plot.loglike_compare()}}
}
