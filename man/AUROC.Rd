% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AUROC.R
\name{AUROC}
\alias{AUROC}
\alias{AUROC.spglm}
\alias{AUROC.spgautor}
\title{Area Under Receiver Operating Characteristic Curve}
\usage{
AUROC(object, ...)

\method{AUROC}{spglm}(object, ...)

\method{AUROC}{spgautor}(object, ...)
}
\arguments{
\item{object}{A fitted model object from \code{\link[=spglm]{spglm()}} or \code{\link[=spgautor]{spgautor()}}) where \code{family = "binomial"}
and the response values are binary, representing a single success or failure for each datum.}

\item{...}{Additional arguments to \code{\link[pROC:auc]{pROC::auc()}}.}
}
\value{
The area under the receiver operating characteristic curve.
}
\description{
Compare area under the receiver operating characteristic curve (AUROC) for binary (e.g.,
logistic) models. The area under the ROC curve provides a measure of the
model's classification accuracy averaged over all possible threshold values.
}
\examples{
spgmod <- spglm(presence ~ elev,
  family = "binomial", data = moose,
  spcov_type = "exponential"
)
AUROC(spgmod)
}
\references{
Robin, X., Turck, N., Hainard, A., Tiberti, N., Lisacek, F., Sanchez, J. C., & Müller, M.
(2011). pROC: an open-source package for R and S+ to analyze and compare ROC curves.
\emph{BMC bioinformatics}, 12, 1-8.
}
