% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model.frame.R
\name{model.frame.spmod}
\alias{model.frame.spmod}
\title{Extract the model frame from a fitted model object}
\usage{
\method{model.frame}{spmod}(formula, ...)
}
\arguments{
\item{formula}{A fitted model object from \code{\link[=splm]{splm()}} or \code{\link[=spautor]{spautor()}}.}

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
