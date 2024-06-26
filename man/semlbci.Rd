% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/semlbci.R
\name{semlbci}
\alias{semlbci}
\title{Likelihood-Based Confidence Interval}
\usage{
semlbci(
  sem_out,
  pars = NULL,
  include_user_pars = TRUE,
  remove_variances = TRUE,
  remove_intercepts = TRUE,
  ciperc = 0.95,
  standardized = FALSE,
  method = c("wn", "ur"),
  robust = c("none", "satorra.2000"),
  try_k_more_times = 2,
  semlbci_out = NULL,
  check_fit = TRUE,
  ...,
  parallel = FALSE,
  ncpus = 2,
  use_pbapply = TRUE,
  loadbalancing = TRUE
)
}
\arguments{
\item{sem_out}{The SEM output. Currently supports
\link[lavaan:lavaan-class]{lavaan::lavaan} outputs only.}

\item{pars}{The positions of the parameters for which the LBCIs are
to be searched. Use the position as appeared on the parameter
tables of the \code{sem_out}. If \code{NULL}, the default, then LBCIs for
all free parameters will be searched. Can also be a vector of
strings to indicate the parameters on the parameter table. The
parameters should be specified in \code{\link[lavaan:lavaan]{lavaan::lavaan()}} syntax. The
vector of strings will be converted by \code{\link[=syntax_to_i]{syntax_to_i()}} to
parameter positions. See \code{\link[=syntax_to_i]{syntax_to_i()}} on how to specify the parameters.}

\item{include_user_pars}{Logical. Whether all user-defined parameters
are automatically included when \code{pars} is not set. Default is \code{TRUE}.
If \code{pars} is explicitly set, this argument will be ignored.}

\item{remove_variances}{Logical. Whether variances and error variances
will be removed. Default is \code{TRUE}, removing all variances and error
variances even if specified in \code{pars}.}

\item{remove_intercepts}{Logical. Whether intercepts will be removed.
Default is \code{TRUE}, removing all intercepts (parameters with operator \code{~1}).
Intercepts are not yet supported in standardized solution and so will
always be removed if \code{standardized = TRUE}.}

\item{ciperc}{The proportion of coverage for the confidence
interval. Default is .95, requesting a 95 percent confidence
interval.}

\item{standardized}{If \code{TRUE}, the LBCI is for the standardized estimates.}

\item{method}{The method to be used to search for the confidence
bounds. Supported methods are\code{"wn"} (Wu-Neale-2012), the default,
and \code{"ur"} (root finding by \code{\link[stats:uniroot]{stats::uniroot()}}).}

\item{robust}{Whether the LBCI based on robust likelihood ratio
test is to be found. Only \code{"satorra.2000"} in \code{\link[lavaan:lavTestLRT]{lavaan::lavTestLRT()}}
is supported for now, implemented by the method proposed by Falk
(2018). If \code{"none"}, the default, then likelihood ratio test based
on maximum likelihood estimation will be used.}

\item{try_k_more_times}{How many more times to try if failed.
Default is 2.}

\item{semlbci_out}{An \code{semlbci-class} object. If provided, parameters already
with LBCIs formed will be excluded from \code{pars}.}

\item{check_fit}{If \code{TRUE} (default), the input (\code{sem_out}) will
be checked by \code{\link[=check_sem_out]{check_sem_out()}}. If not
supported, an error will be raised. If \code{FALSE},
the check will be skipped and the LBCIs will be
searched even for a model or parameter not
supported. Set to \code{TRUE} only for testing.}

\item{...}{Arguments to be passed to \code{\link[=ci_bound_wn_i]{ci_bound_wn_i()}}.}

\item{parallel}{If \code{TRUE}, will use parallel processing to do the search.}

\item{ncpus}{The number of workers, if \code{parallel} is \code{TRUE}.
Default is 2. This number should not be larger than the number CPU
cores.}

\item{use_pbapply}{If \code{TRUE} and \code{pbapply}
is installed, \code{\link[pbapply:pbapply]{pbapply::pbapply()}} will be used to display a
progress bar when finding the intervals. Default is \code{TRUE}.
Ignored if \code{pbapply} is not installed.}

\item{loadbalancing}{Whether load
balancing is used when \code{parallel}
is \code{TRUE} and \code{use_pbapply} is
\code{TRUE}.}
}
\value{
A \code{semlbci}-class object similar to the parameter table
generated by \code{\link[lavaan:parameterEstimates]{lavaan::parameterEstimates()}}, with the LBCIs for
selected parameters added. Diagnostic information, if requested,
will be included in the attributes. See \code{\link[=print.semlbci]{print.semlbci()}} for options
available.
}
\description{
Find the likelihood-based confidence intervals (LBCIs) for
selected free parameters in an SEM output.
}
\details{
\code{\link[=semlbci]{semlbci()}} finds the positions of the selected parameters
in the parameter table and then calls \code{\link[=ci_i_one]{ci_i_one()}} once for each
of them. For the technical details, please see \code{\link[=ci_i_one]{ci_i_one()}} and
the functions it calls to find a confidence bound, currently
\code{\link[=ci_bound_wn_i]{ci_bound_wn_i()}}. \code{\link[=ci_bound_wn_i]{ci_bound_wn_i()}} uses the approach proposed by
Wu and Neale (2012) and illustrated by Pek and Wu (2015).

It supports updating an output of \code{\link[=semlbci]{semlbci()}} by setting
\code{semlbci_out}. This allows forming LBCIs for some parameters after
those for some others have been formed.

If possible, parallel processing should be used (see \code{parallel} and
\code{ncpus}), especially for a model with many parameters.

If the search for some of the confidence bounds failed, with \code{NA} for the
bounds, try increasing \code{try_k_more_times}.

The SEM output will first be checked by \code{\link[=check_sem_out]{check_sem_out()}} to see
whether the model and the estimation method are supported. To skip this
test (e.g., for testing or experimenting with some models and estimators),
set \code{check_fit} to \code{FALSE}.

Examples and technical details can be found at Cheung
and Pesigan (2023), the website of the \code{semlbci}
package (https://sfcheung.github.io/semlbci/),
and the technical appendices at
(https://sfcheung.github.io/semlbci/articles/).

It currently supports \link[lavaan:lavaan-class]{lavaan::lavaan} outputs only.
}
\examples{

library(lavaan)
mod <-
"
m ~ a*x
y ~ b*m
ab := a * b
"
fit_med <- sem(mod, simple_med, fixed.x = FALSE)
p_table <- parameterTable(fit_med)
p_table
lbci_med <- semlbci(fit_med,
                    pars = c("m ~ x",
                             "y ~ m",
                             "ab :="))
lbci_med

}
\references{
Cheung, S. F., & Pesigan, I. J. A. (2023). \emph{semlbci}:
An R package for forming likelihood-based confidence
intervals for parameter estimates, correlations,
indirect effects, and other derived parameters.
\emph{Structural Equation Modeling: A Multidisciplinary Journal},
\emph{30}(6), 985--999.
\doi{10.1080/10705511.2023.2183860}

Falk, C. F. (2018). Are robust standard errors the best approach
for interval estimation with nonnormal data in structural equation
modeling? \emph{Structural Equation Modeling: A Multidisciplinary
Journal, 25}(2), 244-266.
\doi{10.1080/10705511.2017.1367254}

Pek, J., & Wu, H. (2015). Profile likelihood-based confidence
intervals and regions for structural equation models.
\emph{Psychometrika, 80}(4), 1123-1145.
\doi{10.1007/s11336-015-9461-1}

Wu, H., & Neale, M. C. (2012). Adjusted confidence intervals for a
bounded parameter. \emph{Behavior Genetics, 42}(6), 886-898.
\doi{10.1007/s10519-012-9560-z}

Pritikin, J. N., Rappaport, L. M., & Neale, M. C. (2017). Likelihood-based
confidence intervals for a parameter with an upper or lower bound.
\emph{Structural Equation Modeling: A Multidisciplinary Journal, 24}(3), 395-401.
\doi{10.1080/10705511.2016.1275969}
}
\seealso{
\code{\link[=print.semlbci]{print.semlbci()}}, \code{\link[=confint.semlbci]{confint.semlbci()}}, \code{\link[=ci_i_one]{ci_i_one()}}, \code{\link[=ci_bound_wn_i]{ci_bound_wn_i()}}
}
\author{
Shu Fai Cheung \url{https://orcid.org/0000-0002-9871-9448}
}
