#' Simulate a spatial gamma random variable
#'
#' @description Simulate a spatial gamma random variable with a specific
#'   mean and covariance structure.
#'
#' @param spcov_params An [spcov_params()] object.
#' @param dispersion The dispersion value.
#' @param mean A numeric vector representing the mean. \code{mean} must have length 1
#'   (in which case it is recycled) or length equal
#'   to the number of rows in \code{data}. The default is \code{0}.
#' @param samples The number of independent samples to generate. The default
#'   is \code{1}.
#' @param data A data frame or \code{sf} object containing spatial information.
#' @param randcov_params A [randcov_params()] object.
#' @param partition_factor A formula indicating the partition factor.
#' @param ... Additional arguments passed to [sprnorm()].
#'
#' @details The values of \code{spcov_params}, \code{mean}, and \code{randcov_params}
#'   are assumed to be on the link scale. They are used to simulate a latent normal (Gaussian)
#'   response variable using [sprnorm()]. This latent variable is the
#'   conditional mean used with \code{dispersion} to simulate a gamma random variable.
#'
#' @return If \code{samples} is 1, a vector of random variables for each row of \code{data}
#'   is returned. If \code{samples} is greater than one, a matrix of random variables
#'   is returned, where the rows correspond to each row of \code{data} and the columns
#'   correspond to independent samples.
#'
#' @export
#'
#' @examples
#' spcov_params_val <- spcov_params("exponential", de = 0.2, ie = 0.1, range = 1)
#' sprgamma(spcov_params_val, data = caribou, xcoord = x, ycoord = y)
#' sprgamma(spcov_params_val, samples = 5, data = caribou, xcoord = x, ycoord = y)
sprgamma <- function(spcov_params, dispersion = 1, mean = 0, samples = 1, data, randcov_params, partition_factor, ...) {
  n <- NROW(data)
  call_val <- match.call()
  call_val[[1]] <- as.symbol("sprnorm")
  call_list <- as.list(call_val)
  if ("dispersion" %in% names(call_list)) {
    call_list <- call_list[-which(names(call_list) == "dispersion")]
  }
  call_val <- as.call(call_list)
  sprnorm_val <- eval(call_val, envir = parent.frame())
  mu <- exp(sprnorm_val)

  if (is.matrix(mu)) {
    mu_list <- split(t(mu), seq_len(NCOL(mu)))
    sprgamma_val <- vapply(mu_list, function(x) rgamma(n, shape = dispersion, scale = x / dispersion), numeric(n))
  } else {
    sprgamma_val <- rgamma(n, shape = dispersion, scale = mu / dispersion)
  }
  sprgamma_val
}
