#' Fit spatial generalized linear models
#'
#' @description Fit spatial generalized linear models for point-referenced data (i.e.,
#'   generalized geostatistical models) using
#'   a variety of estimation methods, allowing for random effects,
#'   anisotropy, partition factors, and big data methods.
#'
#' @param formula A two-sided linear formula describing the fixed effect structure
#'   of the model, with the response to the left of the \code{~} operator and
#'   the terms on the right, separated by \code{+} operators.
#' @param family The generalized linear model family describing the distribution
#'   of the response variable to be used. Available options
#'   \code{"poisson"}, \code{"nbinomial"}, \code{"binomial"},
#'   \code{"beta"}, \code{"Gamma"}, and \code{"inverse.gaussian"}.
#'   Can be quoted or unquoted. Note that the \code{family} argument
#'   only takes a single value, rather than the list structure used by [stats::glm].
#'   See Details for more.
#' @param data A data frame or \code{sf} object object that contains
#'   the variables in \code{fixed}, \code{random}, and \code{partition_factor}
#'   as well as geographical information. If an \code{sf} object is
#'   provided with \code{POINT} geometries, the x-coordinates and y-coordinates
#'   are used directly. If an \code{sf} object is
#'   provided with \code{POLYGON} geometries, the x-coordinates and y-coordinates
#'   are taken as the centroids of each polygon.
#' @param spcov_type The spatial covariance type. Available options include
#'   \code{"exponential"}, \code{"spherical"}, \code{"gaussian"},
#'   \code{"triangular"}, \code{"circular"}, \code{"cubic"},
#'   \code{"pentaspherical"}, \code{"cosine"}, \code{"wave"},
#'   \code{"jbessel"}, \code{"gravity"}, \code{"rquad"},
#'   \code{"magnetic"}, \code{"matern"}, \code{"cauchy"}, \code{"pexponential"},
#'   \code{"ie"}, and \code{"none"}. Parameterizations of each spatial covariance type are
#'   available in Details. Multiple spatial covariance types can be provided as
#'   a character vector, and then \code{spglm()} is called iteratively for each
#'   element and a list is returned for each model fit. The default for
#'   \code{spcov_type} is \code{"exponential"}. When \code{spcov_type} is
#'   specified, all unknown spatial covariance parameters are estimated.
#'   \code{spcov_type} is ignored if \code{spcov_initial} is provided.
#' @param xcoord The name of the column in \code{data} representing the x-coordinate.
#'   Can be quoted or unquoted. Not required if \code{data} is an \code{sf} object.
#' @param ycoord The name of the column in \code{data} representing the y-coordinate.
#'   Can be quoted or unquoted. Not required if \code{data} is an \code{sf} object.
#' @param spcov_initial An object from [spcov_initial()] specifying initial and/or
#'   known values for the spatial covariance parameters. Multiple [spcov_initial()]
#'   objects can be provided in a list. Then \code{spglm()} is called iteratively
#'   for each element and a list is returned for each model fit.
#' @param dispersion_initial An object from [dispersion_initial()] specifying
#'   initial and/or known values for the dispersion parameter for the
#'   \code{"nbinomial"}, \code{"beta"}, \code{"Gamma"}, and \code{"inverse.gaussian"} families.
#'   \code{family} is ignored if \code{dispersion_initial} is provided.
#' @param estmethod The estimation method. Available options include
#'   \code{"reml"} for restricted maximum likelihood and \code{"ml"} for maximum
#'   likelihood. The default is
#'   \code{"reml"}.
#' @param anisotropy A logical indicating whether (geometric) anisotropy should
#'   be modeled. Not required if \code{spcov_initial} is provided with 1) \code{rotate}
#'   assumed unknown or assumed known and non-zero or 2) \code{scale} assumed unknown
#'   or assumed known and less than one. When \code{anisotropy} is \code{TRUE},
#'   computational times can significantly increase. The default is \code{FALSE}.
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
#' @param local An optional logical or list controlling the big data approximation.
#'   If omitted, \code{local} is set
#'   to \code{TRUE} or \code{FALSE} based on the sample size (the number of
#'   non-missing observations in \code{data}) -- if the sample size exceeds 3,000,
#'   \code{local} is set to \code{TRUE}. Otherwise it is set to \code{FALSE}.
#'   If \code{FALSE}, no big data approximation is implemented.
#'   If a list is provided, the following arguments detail the big
#'   data approximation:
#'   \itemize{
#'     \item \code{index: }The group indexes. Observations in different
#'       levels of \code{index} are assumed to be uncorrelated for the
#'       purposes of estimation. If \code{index} is not provided, it is
#'       determined by specifying \code{method} and either \code{size} or \code{groups}.
#'     \item \code{method}: The big data approximation method used to determine \code{index}. Ignored
#'       if \code{index} is provided. If \code{method = "random"},
#'       observations are randomly assigned to \code{index} based on \code{size}.
#'       If \code{method = "kmeans"}, observations assigned to \code{index}
#'       based on k-means clustering on the coordinates with \code{groups} clusters. The default
#'       is \code{"kmeans"}. Note that both methods have a random component, which
#'       means that you may get different results from separate model fitting calls.
#'       To ensure consistent results, specify \code{index} or set a seed via
#'       \code{base::set.seed()}.
#'     \item \code{size}: The number of observations in each \code{index} group
#'       when \code{method} is \code{"random"}. If the number of observations
#'       is not divisible by \code{size}, some levels get \code{size - 1} observations.
#'       The default is 100.
#'     \item \code{groups: }The number of \code{index} groups. If \code{method}
#'       is \code{"random"}, \code{size} is \eqn{ceiling(n / groups)}, where
#'       \eqn{n} is the sample size. Automatically determined if \code{size}
#'       is specified. If \code{method} is \code{"kmeans"}, \code{groups}
#'       is the number of clusters.
#'     \item \code{var_adjust: }The approach for adjusting the variance-covariance
#'       matrix of the fixed effects. \code{"none"} for no adjustment, \code{"theoretical"}
#'       for the theoretically-correct adjustment,
#'       \code{"pooled"} for the pooled adjustment, and \code{"empirical"} for the
#'       empirical adjustment. The default is \code{"theoretical"} for samples sizes
#'       up to 100,000 and \code{"none"} for samples sizes exceeding 100,000.
#'     \item \code{parallel}: If \code{TRUE}, parallel processing via the
#'       parallel package is automatically used. The default is \code{FALSE}.
#'     \item \code{ncores}: If \code{parallel = TRUE}, the number of cores to
#'       parallelize over. The default is the number of available cores on your machine.
#'   }
#'   When \code{local} is a list, at least one list element must be provided to
#'   initialize default arguments for the other list elements.
#'   If \code{local} is \code{TRUE}, defaults for \code{local} are chosen such
#'   that \code{local} is transformed into
#'   \code{list(size = 100, method = "kmeans", var_adjust = "theoretical", parallel = FALSE)}.
#' @param range_constrain An optional logical that indicates whether the range
#'   should be constrained to enhance numerical stability. If \code{range_constrain = TRUE},
#'   the maximum possible range value is 4 times the maximum distance in the domain.
#'   If \code{range_constrain = FALSE}, then maximum possible range is unbounded.
#'   The default is \code{FALSE}.
#'   Note that if \code{range_constrain = TRUE} and the value of \code{range} in \code{spcov_initial}
#'   is larger than \code{range_constrain}, then \code{range_constrain} is set to
#'   \code{FALSE}.
#' @param ... Other arguments to [esv()] or [stats::optim()].
#'
#' @details The spatial generalized linear model for point-referenced data
#'   (i.e., generalized geostatistical model) can be written as
#'   \eqn{g(\mu) = \eta = X \beta + \tau + \epsilon}, where \eqn{\mu} is the expectation
#'   of the response (\eqn{y}) given the random errors, \eqn{g(.)} is called
#'   a link function which links together the \eqn{\mu} and \eqn{\eta},
#'   \eqn{X} is the fixed effects design
#'   matrix, \eqn{\beta} are the fixed effects, \eqn{\tau} is random error that is
#'   spatially dependent, and \eqn{\epsilon} is random error that is spatially
#'   independent.
#'
#'   There are six generalized linear model
#'   families available: \code{poisson} assumes \eqn{y} is a Poisson random variable
#'   \code{nbinomial} assumes \eqn{y} is a negative binomial random
#'   variable, \code{binomial} assumes \eqn{y} is a binomial random variable,
#'   \code{beta} assumes \eqn{y} is a beta random variable,
#'   \code{Gamma} assumes \eqn{y} is a gamma random
#'   variable, and \code{inverse.gaussian} assumes \eqn{y} is an inverse Gaussian
#'   random variable.
#'
#'   The supports for \eqn{y} for each family are given below:
#'   \itemize{
#'     \item family: support of \eqn{y}
#'     \item poisson: \eqn{0 \le y}; \eqn{y} an integer
#'     \item nbinomial: \eqn{0 \le y}; \eqn{y} an integer
#'     \item binomial: \eqn{0 \le y}; \eqn{y} an integer
#'     \item beta: \eqn{0 < y < 1}
#'     \item Gamma: \eqn{0 < y}
#'     \item inverse.gaussian: \eqn{0 < y}
#'   }
#'
#'   The generalized linear model families
#'   and the parameterizations of their link functions are given
#'   below:
#'   \itemize{
#'     \item family: link function
#'     \item poisson: \eqn{g(\mu) = log(\eta)} (log link)
#'     \item nbinomial: \eqn{g(\mu) = log(\eta)} (log link)
#'     \item binomial: \eqn{g(\mu) = log(\eta / (1 - \eta))} (logit link)
#'     \item beta: \eqn{g(\mu) = log(\eta / (1 - \eta))} (logit link)
#'     \item Gamma: \eqn{g(\mu) = log(\eta)} (log link)
#'     \item inverse.gaussian: \eqn{g(\mu) = log(\eta)} (log link)
#'   }
#'
#'   The variance function of an individual \eqn{y} (given \eqn{\mu})
#'   for each generalized linear model family is given below:
#'   \itemize{
#'     \item family: \eqn{Var(y)}
#'     \item poisson: \eqn{\mu \phi}
#'     \item nbinomial: \eqn{\mu + \mu^2 / \phi}
#'     \item binomial: \eqn{n \mu (1 - \mu) \phi}
#'     \item beta: \eqn{\mu (1 - \mu) / (1 + \phi)}
#'     \item Gamma: \eqn{\mu^2 / \phi}
#'     \item inverse.gaussian: \eqn{\mu^2 / \phi}
#'   }
#'   The parameter \eqn{\phi} is a dispersion parameter that influences \eqn{Var(y)}.
#'   For the \code{poisson} and \code{binomial} families, \eqn{\phi} is always
#'   one. Note that this inverse Gaussian parameterization is different than a
#'   standard inverse Gaussian parameterization, which has variance \eqn{\mu^3 / \lambda}.
#'   Setting \eqn{\phi = \lambda / \mu} yields our parameterization, which is
#'   preferred for computational stability. Also note that the dispersion parameter
#'   is often defined in the literature as \eqn{V(\mu) \phi}, where \eqn{V(\mu)} is the variance
#'   function of the mean. We do not use this parameterization, which is important
#'   to recognize while interpreting dispersion estimates.
#'   For more on generalized linear model constructions, see McCullagh and
#'   Nelder (1989).
#'
#'   Together, \eqn{\tau} and \eqn{\epsilon} are modeled using
#'   a spatial covariance function, expressed as
#'   \eqn{de * R + ie * I}, where \eqn{de} is the dependent error variance, \eqn{R}
#'   is a correlation matrix that controls the spatial dependence structure among observations,
#'   \eqn{ie} is the independent error variance, and \eqn{I} is
#'   an identity matrix. Recall that \eqn{\tau} and \eqn{\epsilon} are modeled on the link scale,
#'   not the inverse link (response) scale. Random effects are also modeled on the link scale.
#'
#'  \code{spcov_type} Details: Parametric forms for \eqn{R} are given below, where \eqn{\eta = h / range}
#'   for \eqn{h} distance between observations:
#'   \itemize{
#'     \item exponential: \eqn{exp(- \eta )}
#'     \item spherical: \eqn{(1 - 1.5\eta + 0.5\eta^3) * I(h <= range)}
#'     \item gaussian: \eqn{exp(- \eta^2 )}
#'     \item triangular: \eqn{(1 - \eta) * I(h <= range)}
#'     \item circular: \eqn{(1 - (2 / \pi) * (m * sqrt(1 - m^2) + sin^{-1}(m))) * I(h <= range), m = min(\eta, 1)}
#'     \item cubic: \eqn{(1 - 7\eta^2 + 8.75\eta^3 - 3.5\eta^5 + 0.75\eta^7) * I(h <= range)}
#'     \item pentaspherical: \eqn{(1 - 1.875\eta + 1.25\eta^3 - 0.375\eta^5) * I(h <= range)}
#'     \item cosine: \eqn{cos(\eta)}
#'     \item wave: \eqn{sin(\eta) / \eta * I(h > 0) + I(h = 0)}
#'     \item jbessel: \eqn{Bj(h * range)}, Bj is Bessel-J function
#'     \item gravity: \eqn{(1 + \eta^2)^{-0.5}}
#'     \item rquad: \eqn{(1 + \eta^2)^{-1}}
#'     \item magnetic: \eqn{(1 + \eta^2)^{-1.5}}
#'     \item matern: \eqn{2^{1 - extra}/ \Gamma(extra) * \alpha^{extra} * Bk(\alpha, extra)}, \eqn{\alpha = (2extra * \eta)^{0.5}}, Bk is Bessel-K function with order \eqn{1/5 \le extra \le 5}
#'     \item cauchy: \eqn{(1 + \eta^2)^{-extra}}, \eqn{extra > 0}
#'     \item pexponential: \eqn{exp(h^{extra}/range)}, \eqn{0 < extra \le 2}
#'     \item none: \eqn{0}
#'     \item ie: \eqn{0} (see below for differences compared to none)
#'   }
#'
#'   There is a slight difference between \code{none} and \code{ie} (the \code{spcov_type}) from above. \code{none}
#'   fixes the \code{"ie"} covariance parameter at zero, which should yield a model that has very similar
#'   parameter estimates as one from [stats::glm]. Note that \code{spglm()} uses
#'   a different likelihood, so direct likelihood-based comparisons (e.g., \code{AIC})
#'   should not be made to compare an \code{spglm()} model to a \code{glm()} one
#'   (though deviance can be used). \code{ie} allows the \code{"ie"} covariance
#'   parameter to vary. If the \code{"ie"} covariance parameter is estimated near
#'   zero, then \code{none} and \code{ie} yield similar models.
#'
#'
#'   All spatial covariance functions are valid in one spatial dimension. All
#'   spatial covariance functions except \code{triangular} and \code{cosine} are
#'   valid in two dimensions.
#'
#' \code{estmethod} Details: The various estimation methods are
#'   \itemize{
#'     \item \code{reml}: Maximize the restricted log-likelihood.
#'     \item \code{ml}: Maximize the log-likelihood.
#'   }
#'   Note that the likelihood being optimized is obtained using the Laplace approximation.
#'
#' \code{anisotropy} Details: By default, all spatial covariance parameters except \code{rotate}
#'   and \code{scale} as well as all random effect variance parameters
#'   are assumed unknown, requiring estimation. If either \code{rotate} or \code{scale}
#'   are given initial values other than 0 and 1 (respectively) or are assumed unknown
#'   in [spcov_initial()], \code{anisotropy} is implicitly set to \code{TRUE}.
#'   (Geometric) Anisotropy is modeled by transforming a covariance function that
#'   decays differently in different directions to one that decays equally in all
#'   directions via rotation and scaling of the original coordinates. The rotation is
#'   controlled by the \code{rotate} parameter in \eqn{[0, \pi]} radians. The scaling
#'   is controlled by the \code{scale} parameter in \eqn{[0, 1]}. The anisotropy
#'   correction involves first a rotation of the coordinates clockwise by \code{rotate} and then a
#'   scaling of the coordinates' minor axis by the reciprocal of \code{scale}. The spatial
#'   covariance is then computed using these transformed coordinates.
#'
#'  \code{random} Details: If random effects are used, the model
#'   can be written as \eqn{y = X \beta + Z1u1 + ... Zjuj + \tau + \epsilon},
#'   where each Z is a random effects design matrix and each u is a random effect.
#'
#'  \code{partition_factor} Details: The partition factor can be represented in matrix form as \eqn{P}, where
#'   elements of \eqn{P} equal one for observations in the same level of the partition
#'   factor and zero otherwise. The covariance matrix involving only the
#'   spatial and random effects components is then multiplied element-wise
#'   (Hadmard product) by \eqn{P}, yielding the final covariance matrix.
#'
#' \code{local} Details: The big data approximation works by sorting observations into different levels
#'   of an index variable. Observations in different levels of the index variable
#'   are assumed to be uncorrelated for the purposes of model fitting. Sparse matrix methods are then implemented
#'   for significant computational gains. Parallelization generally further speeds up
#'   computations when data sizes are larger than a few thousand. Both the \code{"random"} and \code{"kmeans"} values of \code{method}
#'   in \code{local} have random components. That means you may get slightly different
#'   results when using the big data approximation and rerunning \code{spglm()} with the same code. For consistent results,
#'   either set a seed via \code{base::set.seed()} or specify \code{index} to \code{local}.
#'
#'   Observations with \code{NA} response values are removed for model
#'   fitting, but their values can be predicted afterwards by running
#'   \code{predict(object)}.
#'
#' @return A list with many elements that store information about
#'   the fitted model object. If \code{spcov_type} or \code{spcov_initial} are
#'   length one, the list has class \code{spglm}. Many generic functions that
#'   summarize model fit are available for \code{spglm} objects, including
#'   \code{AIC}, \code{AICc}, \code{anova}, \code{augment}, \code{AUROC}, \code{BIC}, \code{coef},
#'   \code{cooks.distance}, \code{covmatrix}, \code{deviance}, \code{fitted}, \code{formula},
#'   \code{glance}, \code{glances}, \code{hatvalues}, \code{influence},
#'   \code{labels}, \code{logLik}, \code{loocv}, \code{model.frame}, \code{model.matrix},
#'   \code{plot}, \code{predict}, \code{print}, \code{pseudoR2}, \code{summary},
#'   \code{terms}, \code{tidy}, \code{update}, \code{varcomp}, and \code{vcov}. If
#'   \code{spcov_type} or \code{spcov_initial} are length greater than one, the
#'   list has class \code{spglm_list} and each element in the list has class
#'   \code{spglm}. \code{glances} can be used to summarize \code{spglm_list}
#'   objects, and the aforementioned \code{spglm} generics can be used on each
#'   individual list element (model fit).
#'
#' @note This function does not perform any internal scaling. If optimization is not
#'   stable due to large extremely large variances, scale relevant variables
#'   so they have variance 1 before optimization.
#'
#' @export
#'
#' @examples
#' spgmod <- spglm(presence ~ elev,
#'   family = "binomial", data = moose,
#'   spcov_type = "exponential"
#' )
#' summary(spgmod)
#' @references
#' McCullagh P. and Nelder, J. A. (1989) \emph{Generalized Linear Models}. London: Chapman and Hall.
spglm <- function(formula, family, data, spcov_type, xcoord, ycoord, spcov_initial,
                  dispersion_initial, estmethod = "reml", anisotropy = FALSE,
                  random, randcov_initial, partition_factor, local, range_constrain, ...) {

  # set exponential as default if nothing specified
  if (missing(spcov_type) && missing(spcov_initial)) {
    spcov_type <- "exponential"
    message("No spatial covariance type provided. Assuming \"exponential\".")
  }

  if (!missing(spcov_type) && !missing(spcov_initial)) {
    message("Both spcov_type and spcov_initial provided. spcov_initial overriding spcov_type.")
  }

  if (!missing(family) && !missing(dispersion_initial)) {
    message("Both family and dispersion_initial provided. dispersion_initial overriding family.")
  }

  # iterate if needed
  if (!missing(spcov_initial) && is.list(spcov_initial[[1]])) {
    call_list <- as.list(match.call())[-1]
    penv <- parent.frame()
    spglm_out <- lapply(spcov_initial, function(x) {
      call_list$spcov_initial <- x
      do.call("spglm", call_list, envir = penv)
    })
    names(spglm_out) <- paste("spcov_initial", seq_along(spcov_initial), sep = "_")
    new_spglm_out <- structure(spglm_out, class = "spglm_list")
    return(new_spglm_out)
  } else if (!missing(spcov_type) && length(spcov_type) > 1) {
    call_list <- as.list(match.call())[-1]
    penv <- parent.frame()
    spglm_out <- lapply(spcov_type, function(x) {
      call_list$spcov_type <- x
      do.call("spglm", call_list, envir = penv)
    })
    names(spglm_out) <- spcov_type
    new_spglm_out <- structure(spglm_out, class = "spglm_list")
    return(new_spglm_out)
  }

  # set dispersion initial
  if (missing(dispersion_initial)) dispersion_initial <- NULL else family <- class(dispersion_initial)

  # fix family
  if (missing(family)) {
    stop("The family argument must be specified.", call. = FALSE)
  }
  if (is.symbol(substitute(family))) { # or is.language
    family <- deparse1(substitute(family))
  }

  # Call splm if necessary (deprecated)
  # if (family == "gaussian") {
  #   call_val <- match.call()
  #   call_val[[1]] <- as.symbol("splm")
  #   call_list <- as.list(call_val)
  #   call_list <- call_list[-which(names(call_list) %in% c("family", "dispersion_initial"))]
  #   call_val <- as.call(call_list)
  #   object <- eval(call_val, envir = parent.frame())
  #   return(object)
  # }

  # set spcov_initial
  if (missing(spcov_initial)) {
    spcov_initial <- spmodel::spcov_initial(spcov_type)
  }

  # perform checks to return errors
  spglm_checks(family, spcov_initial, !missing(xcoord), !missing(ycoord), estmethod, anisotropy, !missing(random))

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

  if (missing(local)) {
    local <- NULL
  }

  if (missing(range_constrain)) {
    range_constrain <- FALSE
  }
  # make this default of TRUE later

  # non standard evaluation for x and y coordinates
  xcoord <- substitute(xcoord)
  ycoord <- substitute(ycoord)

  # get data object
  data_object <- get_data_object_spglm(
    formula, family, data, spcov_initial, xcoord, ycoord,
    estmethod, anisotropy, random, randcov_initial,
    partition_factor, local, range_constrain, ...
  )

  # parallel cluster if necessary
  if (data_object$parallel) {
    data_object$cl <- parallel::makeCluster(data_object$ncores)
    # invisible(clusterEvalQ(data_object$cl, library(Matrix)))
  }

  cov_est_object <- cov_estimate_laploglik_spglm(data_object, formula,
    spcov_initial, dispersion_initial, estmethod,
    optim_dotlist = get_optim_dotlist(...)
  )

  model_stats <- get_model_stats_spglm(cov_est_object, data_object, estmethod)

  # parallel cluster if necessary
  if (data_object$parallel) {
    data_object$cl <- parallel::stopCluster(data_object$cl) # makes it NULL
  }

  # store index if necessary
  if (is.null(local)) { # local was stored as NULL in previous function call
    local_index <- NULL
  } else {
    local_index <- data_object$local_index
  }

  if (inherits(spcov_initial, c("triangular", "circular"))) {
    data_object <- replace_data_object_dimcoords1(data_object)
  }

  output <- list(
    coefficients = model_stats$coefficients,
    fitted = model_stats$fitted,
    hatvalues = model_stats$hatvalues,
    residuals = model_stats$residuals,
    cooks_distance = model_stats$cooks_distance,
    vcov = model_stats$vcov,
    deviance = model_stats$deviance,
    pseudoR2 = model_stats$pseudoR2,
    esv = cov_est_object$esv,
    p = data_object$p,
    n = data_object$n,
    npar = model_stats$npar,
    formula = formula,
    terms = data_object$terms,
    call = match.call(),
    estmethod = estmethod,
    obdata = data_object$obdata,
    newdata = data_object$newdata,
    xcoord = as.character(data_object$xcoord),
    ycoord = as.character(data_object$ycoord),
    anisotropy = data_object$anisotropy,
    dim_coords = data_object$dim_coords,
    random = random,
    optim = cov_est_object$optim_output,
    is_known = cov_est_object$is_known,
    partition_factor = partition_factor,
    max_dist = 2 * data_object$max_halfdist,
    observed_index = data_object$observed_index,
    missing_index = data_object$missing_index,
    local_index = local_index,
    contrasts = data_object$contrasts,
    xlevels = data_object$xlevels,
    is_sf = data_object$is_sf,
    sf_column_name = data_object$sf_column_name,
    crs = data_object$crs,
    family = family,
    y = model_stats$y,
    size = model_stats$size,
    diagtol = data_object$diagtol
  )

  new_output <- structure(output, class = "spglm")
  new_output
}
