% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model.frame.R, R/model.frame_glm.R
\name{model.frame.spmodel}
\alias{model.frame.spmodel}
\alias{model.frame.splm}
\alias{model.frame.spautor}
\alias{model.frame.spglm}
\alias{model.frame.spgautor}
\title{Extract the model frame from a fitted model object}
\usage{
\method{model.frame}{splm}(formula, ...)

\method{model.frame}{spautor}(formula, ...)

\method{model.frame}{spglm}(formula, ...)

\method{model.frame}{spgautor}(formula, ...)
}
\arguments{
\item{formula}{A fitted model object from \code{\link[=splm]{splm()}}, \code{\link[=spautor]{spautor()}}, \code{\link[=spglm]{spglm()}}, or \code{\link[=spgautor]{spgautor()}}.}

\item{...}{Other arguments. Not used (needed for generic consistency).}
}
\value{
A model frame that contains the variables used by the formula
for the fitted model object.
}
\description{
Extract the model frame from a fitted model object.
}
\examples{
spmod <- splm(z ~ water + tarp,
  data = caribou,
  spcov_type = "exponential", xcoord = x, ycoord = y
)
model.frame(spmod)
}
\seealso{
\code{\link[stats:model.frame]{stats::model.frame()}}
}
