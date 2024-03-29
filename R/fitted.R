#' Extract model fitted values
#'
#' @description Extract fitted values from fitted model objects. \code{fitted.values}
#'   is an alias.
#'
#' @param object A fitted model object from [splm()], [spautor()], [spglm()], or [spgautor()].
#' @param type \code{"response"} for fitted values of the response, \code{"spcov"}
#'   for fitted values of the spatial random errors, or \code{"randcov"} for
#'   fitted values of the random effects. If from \code{spglm()} or \code{spgautor()},
#'   \code{"link"} for fitted values on the link scale. The default is \code{"response"}.
#' @param ... Other arguments. Not used (needed for generic consistency).
#'
#' @details When \code{type} is \code{"response"}, the fitted values
#'   for each observation are the standard fitted values \eqn{X \hat{\beta}}.
#'   When \code{type} is \code{"spcov"} the fitted values for each observation
#'   are (generally) the best linear unbiased predictors of the spatial dependent and spatial
#'   independent random error. When \code{type} is \code{"randcov"}, the fitted
#'   values for each level of each random effect are (generally) the best linear unbiased
#'   predictors of the corresponding random effect. The fitted values for \code{type}
#'   \code{"spcov"} and \code{"randcov"} can generally be used to check assumptions
#'   for each component of the fitted model object (e.g., check a Gaussian assumption).
#'
#' @return The fitted values according to \code{type}.
#'
#' @name fitted.spmodel
#' @method fitted splm
#' @export
#'
#' @examples
#' spmod <- splm(z ~ water + tarp,
#'   data = caribou,
#'   spcov_type = "exponential", xcoord = x, ycoord = y
#' )
#' fitted(spmod)
#' fitted.values(spmod)
#' fitted(spmod, type = "spcov")
fitted.splm <- function(object, type = "response", ...) {
  if (type == "response") {
    fitted_val <- object$fitted$response
  } else if (type == "spcov") {
    fitted_val <- object$fitted$spcov
  } else if (type == "randcov") {
    fitted_val <- object$fitted$randcov
  } else {
    stop("Invalid type argument. The type argument must be \"response\", \"spcov\", or \"randcov\".", call. = FALSE)
  }
  fitted_val
}
#' @rdname fitted.spmodel
#' @method fitted.values splm
#' @export
fitted.values.splm <- fitted.splm

#' @rdname fitted.spmodel
#' @method fitted spautor
#' @export
fitted.spautor <- fitted.splm

#' @rdname fitted.spmodel
#' @method fitted.values spautor
#' @export
fitted.values.spautor <- fitted.spautor
