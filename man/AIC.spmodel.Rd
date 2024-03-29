% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AIC.R, R/AIC_glm.R
\name{AIC.spmodel}
\alias{AIC.spmodel}
\alias{AIC.splm}
\alias{AIC.spautor}
\alias{AIC.spglm}
\alias{AIC.spgautor}
\alias{AICc}
\alias{AICc.splm}
\alias{AICc.spautor}
\alias{AICc.spglm}
\alias{AICc.spgautor}
\title{Compute AIC and AICc of fitted model objects}
\usage{
\method{AIC}{splm}(object, ..., k = 2)

\method{AIC}{spautor}(object, ..., k = 2)

\method{AIC}{spglm}(object, ..., k = 2)

\method{AIC}{spgautor}(object, ..., k = 2)

AICc(object, ..., k = 2)

\method{AICc}{splm}(object, ..., k = 2)

\method{AICc}{spautor}(object, ..., k = 2)

\method{AICc}{spglm}(object, ..., k = 2)

\method{AICc}{spgautor}(object, ..., k = 2)
}
\arguments{
\item{object}{A fitted model object from \code{\link[=splm]{splm()}}, \code{\link[=spautor]{spautor()}}, \code{\link[=spglm]{spglm()}}, or \code{\link[=spgautor]{spgautor()}}
where \code{estmethod} is \code{"ml"} or \code{"reml"}.}

\item{...}{Optionally more fitted model objects.}

\item{k}{The penalty parameter, taken to be 2. Currently not allowed to differ
from 2 (needed for generic consistency).}
}
\value{
If just one object is provided, a numeric value with the corresponding
AIC or AICc.

If multiple objects are provided, a \code{data.frame} with rows corresponding
to the objects and columns representing the number of parameters estimated
(\code{df}) and the AIC or AICc.
}
\description{
Compute AIC and AICc for one or
several fitted model objects for which a log-likelihood
value can be obtained.
}
\details{
When comparing models fit by maximum or restricted maximum
likelihood, the smaller the AIC or AICc, the better the fit. The AICc contains
a correction to AIC for small sample sizes. The theory of
AIC and AICc requires that the log-likelihood has been maximized, and hence,
no AIC or AICc methods exist for models where \code{estmethod} is not
\code{"ml"} or \code{"reml"}. Additionally, AIC and AICc comparisons between \code{"ml"}
and \code{"reml"} models are meaningless -- comparisons should only be made
within a set of models estimated using \code{"ml"} or a set of models estimated
using \code{"reml"}. AIC and AICc comparisons for \code{"reml"} must
use the same fixed effects. To vary the covariance parameters and
fixed effects simultaneously, use \code{"ml"}.

Hoeting et al. (2006) defines that spatial AIC as
\eqn{-2loglik + 2(estparams)} and the spatial AICc as
\eqn{-2loglik + 2n(estparams) / (n - estparams - 1)}, where \eqn{n} is the sample size
and \eqn{estparams} is the number of estimated parameters. For \code{"ml"}, \eqn{estparams} is
the number of estimated covariance parameters plus the number of estimated
fixed effects. For \code{"reml"}, \eqn{estparams} is the number of estimated covariance
parameters.
}
\examples{
spmod <- splm(z ~ water + tarp,
  data = caribou,
  spcov_type = "exponential", xcoord = x, ycoord = y
)
AIC(spmod)
AICc(spmod)
}
