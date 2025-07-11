#' Fit spatial autoregressive models
#'
#' @description Fit spatial linear models for areal data (i.e., spatial autoregressive models)
#'   using a variety of estimation methods, allowing for random effects,
#'   partition factors, and row standardization.
#'
#' @param formula A two-sided linear formula describing the fixed effect structure
#'   of the model, with the response to the left of the \code{~} operator and
#'   the terms, separated by \code{+} operators, on the right.
#' @param data A data frame or \code{sf} object that contains
#'   the variables in \code{fixed}, \code{random}, and \code{partition_factor},
#'   as well as potentially geographical information.
#' @param spcov_type The spatial covariance type. Available options include
#'   \code{"car"}, \code{"sar"}, and \code{"none"}. Parameterizations of each spatial covariance type are
#'   available in Details. When \code{spcov_type} is specified, relevant spatial
#'   covariance parameters are assumed unknown, requiring estimation.
#'   \code{spcov_type} is not required (and is
#'   ignored) if \code{spcov_initial} is provided.  Multiple values can be
#'   provided in a character vector. Then \code{spautor()} is called iteratively
#'   for each element and a list is returned for each model fit.
#'   The default for \code{spcov_type} is \code{"car"}. When \code{spcov_type}
#'   is \code{"none"}, [splm()] is called.
#' @param spcov_initial An object from [spcov_initial()] specifying initial and/or
#'   known values for the spatial covariance parameters.
#'   Not required if \code{spcov_type} is provided. Multiple [spcov_initial()]
#'   objects can be provided in a list. Then \code{spautor()} is called iteratively
#'   for each element and a list is returned for each model fit.
#' @param estmethod The estimation method. Available options include
#'   \code{"reml"} for restricted maximum likelihood and \code{"ml"} for maximum
#'   likelihood The default is
#'   \code{"reml"}.
#' @param random A one-sided linear formula describing the random effect structure
#'   of the model. Terms are specified to the right of the \code{~ operator}.
#'   Each term has the structure \code{x1 + ... + xn | g1/.../gm}, where \code{x1 + ... + xn}
#'   specifies the model for the random effects and \code{g1/.../gm} is the grouping
#'   structure. Separate terms are separated by \code{+} and must generally
#'   be wrapped in parentheses. Random intercepts are added to each model
#'   implicitly when at least  one other variable is defined.
#'   If a random intercept is not desired, this must be explicitly
#'   defined (e.g., \code{x1 + ... + xn - 1 | g1/.../gm}). If only a random intercept
#'   is desired for a grouping structure, the random intercept must be specified
#'   as \code{1 | g1/.../gm}. Note that \code{g1/.../gm} is shorthand for \code{(1 | g1/.../gm)}.
#'   If only random intercepts are desired and the shorthand notation is used,
#'   parentheses can be omitted.
#' @param randcov_initial An optional object specifying initial and/or
#'   known values for the random effect variances.
#' @param partition_factor A one-sided linear formula with a single term
#'   specifying the partition factor.  The partition factor assumes observations
#'   from different levels of the partition factor are uncorrelated.
#' @param W Weight matrix specifying the neighboring structure used.
#'   Not required if \code{data} is an \code{sf} object with \code{POLYGON} geometry,
#'   as \code{W} is calculated internally using queen contiguity. If calculated internally,
#'   \code{W} is computed using \code{sf::st_intersects()}. Also not required if \code{data}
#'   is an \code{sf} object with \code{POINT} geometry as long as \code{cutoff} is specified.
#' @param row_st A logical indicating whether row standardization be performed on
#'   \code{W}. The default is \code{TRUE}.
#' @param M \code{M} matrix satisfying the car symmetry condition. The car
#'   symmetry condition states that \eqn{(I - range * W)^{-1}M} is symmetric, where
#'   \eqn{I} is an identity matrix, \eqn{range} is a constant that controls the
#'   spatial dependence, \code{W} is the weights matrix,
#'   and \eqn{^{-1}} represents the inverse operator.
#'   \code{M} is required for car models
#'   when \code{W} is provided and \code{row_st} is \code{FALSE}.  When \code{M},
#'   is required, the default is the identity matrix. \code{M} must be diagonal
#'   or given as a vector or one-column matrix assumed to be the diagonal.
#' @param range_positive Whether the range should be constrained to be positive.
#'   The default is \code{TRUE}.
#' @param cutoff The numeric, distance-based cutoff used to determine \code{W}
#'   when \code{W} is not specified. For an \code{sf} object with \code{POINT} geometry,
#'   two locations are considered neighbors if the distance between them is less
#'   than or equal to \code{cutoff}.
#' @param ... Other arguments to \code{stats::optim()}.
#'
#' @details The spatial linear model for areal data (i.e., spatial autoregressive model)
#'   can be written as
#'   \eqn{y = X \beta + \tau + \epsilon}, where \eqn{X} is the fixed effects design
#'   matrix, \eqn{\beta} are the fixed effects, \eqn{\tau} is random error that is
#'   spatially dependent, and \eqn{\epsilon} is random error that is spatially
#'   independent. Together, \eqn{\tau} and \eqn{\epsilon} are modeled using
#'   a spatial covariance function, expressed as
#'   \eqn{de * R + ie * I}, where \eqn{de} is the dependent error variance, \eqn{R}
#'   is a matrix that controls the spatial dependence structure among observations,
#'   \eqn{ie} is the independent error variance, and \eqn{I} is
#'   an identity matrix. Note that \eqn{de} and \eqn{ie} must be non-negative while \eqn{range}
#'   must be between the reciprocal of the maximum
#'   eigenvalue of \code{W} and the reciprocal of the minimum eigenvalue of
#'   \code{W}.
#'
#'   \code{spcov_type} Details: Parametric forms for \eqn{R} are given below:
#'   \itemize{
#'     \item car: \eqn{(I - range * W)^{-1}M}, weights matrix \eqn{W},
#'      symmetry condition matrix \eqn{M}
#'     \item sar: \eqn{[(I - range * W)(I - range * W)^T]^{-1}},
#'      weights matrix \eqn{W}, \eqn{^T} indicates matrix transpose
#'     \item none: \eqn{0}
#'   }
#'   If there are observations with no neighbors, they are given a unique variance
#'   parameter called \code{extra}, which must be non-negative.
#'
#'  \code{estmethod} Details: The various estimation methods are
#'   \itemize{
#'     \item \code{reml}: Maximize the restricted log-likelihood.
#'     \item \code{ml}: Maximize the log-likelihood.
#'   }
#'
#'   By default, all spatial covariance parameters except \code{ie}
#'   as well as all random effect variance parameters
#'   are assumed unknown, requiring estimation. \code{ie} is assumed zero and known by default
#'   (in contrast to models fit using [splm()], where \code{ie} is assumed
#'   unknown by default). To change this default behavior, specify \code{spcov_initial}
#'   (an \code{NA} value for \code{ie} in \code{spcov_initial} to assume
#'   \code{ie} is unknown, requiring estimation).
#'
#'  \code{random} Details: If random effects are used, the model
#'   can be written as \eqn{y = X \beta + Z1u1 + ... Zjuj + \tau + \epsilon},
#'   where each Z is a random effects design matrix and each u is a random effect.
#'
#' \code{partition_factor} Details:  The partition factor can be represented in matrix form as \eqn{P}, where
#'   elements of \eqn{P} equal one for observations in the same level of the partition
#'   factor and zero otherwise. The covariance matrix involving only the
#'   spatial and random effects components is then multiplied element-wise
#'   (Hadmard product) by \eqn{P}, yielding the final covariance matrix.
#'
#'   Observations with \code{NA} response values are removed for model
#'   fitting, but their values can be predicted afterwards by running
#'   \code{predict(object)}. This is the only way to perform prediction for
#'   \code{spautor()} models (i.e., the prediction locations must be known prior
#'   to estimation).
#'
#' @return A list with many elements that store information about
#'   the fitted model object. If \code{spcov_type} or \code{spcov_initial} are
#'   length one, the list has class \code{spautor}. Many generic functions that
#'   summarize model fit are available for \code{spautor} objects, including
#'   \code{AIC}, \code{AICc}, \code{anova}, \code{augment}, \code{BIC}, \code{coef},
#'   \code{cooks.distance}, \code{covmatrix}, \code{deviance}, \code{fitted}, \code{formula},
#'   \code{glance}, \code{glances}, \code{hatvalues}, \code{influence},
#'   \code{labels}, \code{logLik}, \code{loocv}, \code{model.frame}, \code{model.matrix},
#'   \code{plot}, \code{predict}, \code{print}, \code{pseudoR2}, \code{summary},
#'   \code{terms}, \code{tidy}, \code{update}, \code{varcomp}, and \code{vcov}. If
#'   \code{spcov_type} or \code{spcov_initial} are length greater than one, the
#'   list has class \code{spautor_list} and each element in the list has class
#'   \code{spautor}. \code{glances} can be used to summarize \code{spautor_list}
#'   objects, and the aforementioned \code{spautor} generics can be used on each
#'   individual list element (model fit).
#'
#' @note This function does not perform any internal scaling. If optimization is not
#'   stable due to large extremely large variances, scale relevant variables
#'   so they have variance 1 before optimization.
#'
#' @export
#'
#' @examples
#' spmod <- spautor(log_trend ~ 1, data = seal, spcov_type = "car")
#' summary(spmod)
spautor <- function(formula, data, spcov_type, spcov_initial, estmethod = "reml",
                    random, randcov_initial, partition_factor, W, row_st = TRUE, M, range_positive = TRUE,
                    cutoff, ...) {

  # set car as default if nothing specified
  if (missing(spcov_type) && missing(spcov_initial)) {
    spcov_type <- "car"
    message("No spatial covariance type provided. Assuming \"car\".")
  }

  if (!missing(spcov_type) && !missing(spcov_initial)) {
    message("Both spcov_type and spcov_initial provided. spcov_initial overriding spcov_type.")
  }

  # iterate if needed
  if (!missing(spcov_initial) && is.list(spcov_initial[[1]])) {
    call_list <- as.list(match.call())[-1]
    penv <- parent.frame()
    spautor_out <- lapply(spcov_initial, function(x) {
      call_list$spcov_initial <- x
      do.call("spautor", call_list, envir = penv)
    })
    names(spautor_out) <- paste("spcov_initial", seq_along(spcov_initial), sep = "_")
    new_spautor_out <- structure(spautor_out, class = "spautor_list")
    return(new_spautor_out)
  } else if (!missing(spcov_type) && length(spcov_type) > 1) {
    call_list <- as.list(match.call())[-1]
    penv <- parent.frame()
    spautor_out <- lapply(spcov_type, function(x) {
      call_list$spcov_type <- x
      do.call("spautor", call_list, envir = penv)
    })
    names(spautor_out) <- spcov_type
    new_spautor_out <- structure(spautor_out, class = "spautor_list")
    return(new_spautor_out)
  }



  # call splm if spcov_type is none (works on an individual element of spcov_type or initial)
  if ((!missing(spcov_type) && spcov_type %in% c("none", "ie")) || (!missing(spcov_initial) && inherits(spcov_initial, c("none", "ie")))) {
    call_list <- as.list(match.call())[-1]
    # remove spautor specific arguments
    args_remove <- c("W", "row_st", "range_positive", "cutoff")
    call_list <- call_list[!names(call_list) %in% args_remove]
    penv <- parent.frame()
    splm_out <- do.call("splm", call_list, envir = penv)
    return(splm_out)
  }

  # replace initial values with appropriate NA's
  if (missing(spcov_initial)) {
    spcov_initial <- spmodel::spcov_initial(spcov_type)
  }

  spautor_checks(class(spcov_initial), !missing(W), data, estmethod)

  # set partition factor if necessary
  if (missing(W)) {
    W <- NULL
  }

  if (missing(M)) {
    M <- NULL
  }

  # set random NULL if necessary
  if (missing(random)) {
    random <- NULL
  }

  # set rancov_initial NULL if necessary
  if (missing(randcov_initial)) {
    randcov_initial <- NULL
  }

  # set partition factor if necessary
  if (missing(partition_factor)) {
    partition_factor <- NULL
  }

  if (missing(cutoff)) {
    cutoff <- NULL
  }

  # get data object
  data_object <- get_data_object_spautor(
    formula, data, spcov_initial,
    estmethod, W, M, random, randcov_initial,
    partition_factor, row_st, range_positive, cutoff, ...
  )


  cov_est_object <- switch(estmethod,
    "reml" = cov_estimate_gloglik_spautor(data_object, formula, spcov_initial, estmethod,
      optim_dotlist = get_optim_dotlist(...)
    ),
    "ml" = cov_estimate_gloglik_spautor(data_object, formula, spcov_initial, estmethod,
      optim_dotlist = get_optim_dotlist(...)
    )
  )


  model_stats <- get_model_stats_spautor(cov_est_object, data_object, estmethod)

  output <- list(
    coefficients = model_stats$coefficients,
    fitted = model_stats$fitted,
    hatvalues = model_stats$hatvalues,
    residuals = model_stats$residuals,
    cooks_distance = model_stats$cooks_distance,
    vcov = model_stats$vcov,
    deviance = model_stats$deviance,
    pseudoR2 = model_stats$pseudoR2,
    p = data_object$p,
    n = data_object$n,
    npar = model_stats$npar,
    formula = formula,
    terms = data_object$terms,
    call = match.call(),
    estmethod = estmethod,
    data = data_object$data,
    newdata = data_object$newdata,
    optim = cov_est_object$optim_output,
    is_known = cov_est_object$is_known,
    W = data_object$W,
    M = data_object$M,
    partition_factor = partition_factor,
    random = random,
    observed_index = data_object$observed_index,
    missing_index = data_object$missing_index,
    contrasts = data_object$contrasts,
    xlevels = data_object$xlevels,
    is_sf = data_object$is_sf,
    sf_column_name = data_object$sf_column_name,
    crs = data_object$crs,
    diagtol = data_object$diagtol
  )

  new_output <- structure(output, class = "spautor")
  new_output
}
