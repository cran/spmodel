get_data_object_splm <- function(formula, data, spcov_initial, xcoord, ycoord, estmethod,
                                 anisotropy, random, randcov_initial, partition_factor, local,
                                 range_constrain, ...) {


  # covert sp to sf
  attr_sp <- attr(class(data), "package")
  if (!is.null(attr_sp) && length(attr_sp) == 1 && attr_sp == "sp") {
    stop("sf objects must be used instead of sp objects. To convert your sp object into an sf object, run sf::st_as_sf().", call. = FALSE)
  }


  # convert sf to data frame (point geometry) (1d objects obsolete)
  ## see if data has sf class
  if (inherits(data, "sf")) {
    # set is_sf
    is_sf <- TRUE
    sf_column_name <- attributes(data)$sf_column
    crs <- attributes(data[[sf_column_name]])$crs
    data_sf <- suppressWarnings(sf::st_centroid(data))
    # store as data frame
    data <- sf_to_df(data_sf)
    ## name xcoord ".xcoord" to be used later
    xcoord <- ".xcoord"
    ## name ycoord ".ycoord" to be used later
    ycoord <- ".ycoord"
  } else {
    is_sf <- FALSE
    sf_column_name <- NULL
    crs <- NULL
    data_sf <- NULL
  }

  if (!is_sf && missing(xcoord) && !inherits(spcov_initial, c("none", "ie"))) {
    stop("The xcoord argument must be specified.", call. = FALSE)
  }

  if (!missing(xcoord)) {
    if (!as.character(xcoord) %in% colnames(data)) {
      stop("The xcoord argument must match the name of a variable in data.", call. = FALSE)
    }
  }

  if (!missing(ycoord)) {
    if (!as.character(ycoord) %in% colnames(data)) {
      stop("The ycoord argument must match the name of a variable in data.", call. = FALSE)
    }
  }


  # setting ycoord orig val for use with circular or triangular
  ycoord_orig_name <- NULL
  ycoord_orig_val <- NULL
  # find coordinate dimension and set defaults
  if (inherits(spcov_initial, c("none", "ie")) && estmethod %in% c("reml", "ml")) {
    dim_coords <- 0
    if (missing(xcoord)) {
      xcoord <- ".xcoord"
      data[[xcoord]] <- 0
    }
    if (missing(ycoord)) {
      ycoord <- ".ycoord"
      if (as.character(xcoord) == ".ycoord") {
        ycoord <- ".ycoord2"
      }
      data[[ycoord]] <- 0
    }
  } else if (missing(ycoord) || inherits(spcov_initial, c("triangular", "cosine"))) { # for some reason nse arguments are passed as missing
    dim_coords <- 1
    if (!missing(ycoord)) {
      ycoord_orig_name <- ycoord
      ycoord_orig_val <- data[[ycoord]]
    }
    ycoord <- ".ycoord"
    if (as.character(xcoord) == ".ycoord") {
      ycoord <- ".ycoord2"
    }
    data[[ycoord]] <- 0
  } else {
    dim_coords <- 2
  }

  # check missing coordinates (missing coordinates can't be in sf objects)
  if (any(is.na(c(data[[xcoord]], data[[ycoord]])))) {
    stop("Missing values in coordinates not allowed.", call. = FALSE)
  }

  # check coordinates proper type
  if (any(!is.numeric(data[[xcoord]]), !is.numeric(data[[ycoord]]))) {
    stop("Coordinates must be numeric.", call. = FALSE)
  }

  # check if coordinates are projected
  if (is_sf) {
    if (!is.na(st_is_longlat(crs)) && st_is_longlat(crs)) {
      warning("Coordinates are in a geographic coordinate system. For the most accurate results, please ensure
            coordinates are in a projected coordinate system (e.g., via sf::st_transform()).", call. = FALSE)
    }
  } else {
    # possible revisit this later and add explicit warning
    # if (any(abs(c(data[[xcoord]], data[[ycoord]])) <= 360)) {
    #   warning("Coordinates may be in a geographic coordinate system. For the most accurate results, please ensure
    #         coordinates are in a projected coordinate system (e.g., via sf::st_transform()).", call. = FALSE)
    # }
  }

  # subsetting by na and not na values
  ## find response variabale name
  na_index <- is.na(data[[all.vars(formula)[1]]])
  # store observed index
  observed_index <- which(!na_index)
  missing_index <- which(na_index)

  # find small and newdata
  if (any(na_index)) {
    ## find newdata to be used in prediction later
    if (is_sf) {
      newdata <- data_sf[na_index, , drop = FALSE] # keep as sf object is users want that
    } else {
      newdata <- data[na_index, , drop = FALSE]
    }
    ## subset original data
    obdata <- data[!na_index, , drop = FALSE]
  } else {
    obdata <- data
    newdata <- NULL
  }

  # finding model frame
  obdata_model_frame <- model.frame(formula, obdata, drop.unused.levels = TRUE, na.action = na.pass)
  # finding contrasts as ...
  dots <- list(...)
  if (!"contrasts" %in% names(dots)) {
    dots$contrasts <- NULL
  }
  # model matrix with potential NA
  X <- model.matrix(formula, obdata_model_frame, contrasts = dots$contrasts)
  # finding rows w/out NA
  ob_predictors <- complete.cases(X)
  if (any(!ob_predictors)) {
    stop("Cannot have NA values in predictors.", call. = FALSE)
  }
  # subset obdata by nonNA predictors
  obdata <- obdata[ob_predictors, , drop = FALSE]

  # new model frame
  obdata_model_frame <- model.frame(formula, obdata, drop.unused.levels = TRUE, na.action = na.omit)
  # find terms
  terms_val <- terms(obdata_model_frame)
  # find X
  X <- model.matrix(formula, obdata_model_frame, contrasts = dots$contrasts)
  # find induced contrasts and xlevels
  dots$contrasts <- attr(X, "contrasts")
  xlevels <- .getXlevels(terms_val, obdata_model_frame)
  # find p
  p <- as.numeric(Matrix::rankMatrix(X, method = "qr"))
  if (p < NCOL(X)) {
    warning("There are perfect collinearities detected in X (the matrix of explanatory variables). This may make the model fit unreliable or may cause an error while model fitting. Consider removing redundant explanatory variables and refitting the model.", call. = FALSE)
  }
  # find sample size
  n <- NROW(X)
  # find response
  y <- as.matrix(model.response(obdata_model_frame), ncol = 1)
  # adjust response to reflect offset
  offset <- model.offset(obdata_model_frame)
  if (!is.null(offset)) {
    offset <- as.matrix(offset, ncol = 1)
    y <- y - offset
  }

  # see if response is numeric
  if (!is.numeric(y)) {
    stop("Response variable must be numeric", call. = FALSE)
  }

  # error if no variance
  if (var(y) == 0) {
    stop("The response has no variability. Model fit unreliable.", call. = FALSE)
  }

  # error if p >= n
  if (p >= n) {
    stop("The number of fixed effects is at least as large as the number of observations (p >= n). Consider reducing the number of fixed effects and rerunning splm().", call. = FALSE)
  }

  # find s2 for initial values
  # s2 <- summary(lm(data_object$formula, do.call("rbind", data_object$obdata_list)))$sigma^2
  # s2 <- summary(lm(formula, obdata))$sigma^2
  qr_val <- qr(X)
  R_val <- qr.R(qr_val)
  betahat <- backsolve(R_val, qr.qty(qr_val, y))
  resid <- y - X %*% betahat
  s2 <- sum(resid^2) / (n - p)
  diagtol <- 0


  # storing max halfdist
  x_range <- range(obdata[[xcoord]])
  y_range <- range(obdata[[ycoord]])
  max_halfdist <- sqrt((max(x_range) - min(x_range))^2 + (max(y_range) - min(y_range))^2) / 2

  # if (is.logical(range_constrain)) {
  #   if (range_constrain) {
  #     range_constrain_value <- 2 * max_halfdist * 5
  #   } else {
  #     range_constrain_value <- Inf
  #   }
  # } else if (is.numeric(range_constrain)) {
  #   range_constrain <- TRUE
  #   range_constrain_value <- 0
  # } else {
  #   stop("range_constrain must be logical or numeric.", call. = FALSE)
  # }

  # range constrain
  max_range_scale <- 4
  range_constrain_value <- 2 * max_halfdist * max_range_scale
  if ("range" %in% names(spcov_initial$is_known)) {
    if (spcov_initial$is_known[["range"]] || (spcov_initial$initial[["range"]] > range_constrain_value)) {
      range_constrain <- FALSE
    }
  }

  if (inherits(spcov_initial, c("none", "ie"))) {
    range_constrain <- FALSE
  }

  if (is.logical(range_constrain)) {
    if (!range_constrain) {
      range_constrain_value <- NULL
    }
  } else {
    stop("range_constrain must be logical.", call. = FALSE)
  }

  # override anisotropy argument if needed
  anisotropy <- get_anisotropy_corrected(anisotropy, spcov_initial)

  # coerce to factor
  if (!is.null(partition_factor)) {
    partition_factor_labels <- labels(terms(partition_factor))
    if (length(partition_factor_labels) > 1) {
      stop("Only one variable can be specified in partition_factor.", call. = FALSE)
    }
    partition_mf <- model.frame(partition_factor, obdata)
    if (any(!attr(terms(partition_mf), "dataClasses") %in% c("character", "factor", "ordered"))) {
      stop("Partition factor variable must be categorical or factor.", call. = FALSE)
    }
    partition_factor <- reformulate(partition_factor_labels, intercept = FALSE)
    # partition_factor <- reformulate(paste0("as.character(", partition_factor_labels, ")"), intercept = FALSE)
  }


  # find index
  if (is.null(local)) {
    if (n > 5000) {
      local <- TRUE
      message("Because the sample size exceeds 5,000, we are setting local = TRUE to perform computationally efficient approximations. To override this behavior and compute the exact solution, rerun with local = FALSE. Be aware that setting local = FALSE may result in exceedingly long computational times.")
    } else {
      local <- FALSE
    }
  }
  local <- get_local_list_estimation(local, obdata, xcoord, ycoord, n, partition_factor)

  # store data list
  obdata_list <- split.data.frame(obdata, local$index)

  # store X and y
  X_list <- split.data.frame(X, local$index)
  y_list <- split.data.frame(y, local$index)
  ones_list <- lapply(obdata_list, function(x) matrix(rep(1, nrow(x)), ncol = 1))

  # organize offset (as a one col matrix)
  if (!is.null(offset)) {
    offset <- do.call("rbind", (split.data.frame(offset, local$index)))
  }

  # store random effects list
  if (is.null(random)) {
    randcov_initial <- NULL
    randcov_list <- NULL
    randcov_names <- NULL
  } else {
    randcov_names <- get_randcov_names(random)
    randcov_xlevs <- lapply(randcov_names, get_randcov_xlev, obdata)
    names(randcov_xlevs) <- randcov_names
    randcov_list <- lapply(obdata_list, function(x) {
      get_randcov_Zs(x, randcov_names, xlev_list = randcov_xlevs)
    })
    # old code that computed randcov_Zs before spatial indexing organizing
    # randcov_Zs <- get_randcov_Zs(obdata, randcov_names)
    # randcov_list <- get_randcov_list(local$index, randcov_Zs, randcov_names)
    if (is.null(randcov_initial)) {
      randcov_initial <- spmodel::randcov_initial()
    } else {
      randcov_given_names <- unlist(lapply(
        names(randcov_initial$initial),
        function(x) labels(terms(reformulate(x)))
      ))
      randcov_initial_names <- unique(unlist(lapply(randcov_given_names, get_randcov_name)))
      if (length(randcov_initial_names) != length(names(randcov_initial$initial))) {
        stop("No / can be specified in randcov_initial(). Please specify starting
             values for each variable (e.g., a/b = a + a:b)", call. = FALSE)
      }
      names(randcov_initial$initial) <- randcov_initial_names
      names(randcov_initial$is_known) <- randcov_initial_names
    }
  }

  # store partition matrix list
  if (!is.null(local$partition_factor)) {
    partition_list <- lapply(obdata_list, function(x) partition_matrix(local$partition_factor, x))
  } else {
    partition_list <- NULL
  }

  # store order
  order <- unlist(split(seq_len(n), local$index), use.names = FALSE)

  # return appropriate list
  list(
    anisotropy = anisotropy, contrasts = dots$contrasts, crs = crs,
    dim_coords = dim_coords, formula = formula, is_sf = is_sf, local_index = local$index,
    obdata = obdata, obdata_list = obdata_list,
    observed_index = observed_index, offset = offset, ones_list = ones_list, order = order, n = n,
    max_halfdist = max_halfdist, missing_index = missing_index, ncores = local$ncores,
    newdata = newdata, p = p, parallel = local$parallel,
    partition_factor_initial = partition_factor, partition_factor = local$partition_factor,
    partition_list = partition_list, randcov_initial = randcov_initial,
    randcov_list = randcov_list, randcov_names = randcov_names,
    sf_column_name = sf_column_name, terms = terms_val, var_adjust = local$var_adjust,
    X_list = X_list, xcoord = xcoord, xlevels = xlevels, y_list = y_list, ycoord = ycoord,
    ycoord_orig_name = ycoord_orig_name, ycoord_orig_val = ycoord_orig_val, s2 = s2, diagtol = diagtol,
    range_constrain = range_constrain, range_constrain_value = range_constrain_value
  )
}




get_data_object_spautor <- function(formula, data, spcov_initial,
                                    estmethod, W, M, random, randcov_initial,
                                    partition_factor, row_st, range_positive, cutoff, ...) {
  ## convert sp to sf object
  attr_sp <- attr(class(data), "package")
  if (!is.null(attr_sp) && length(attr_sp) == 1 && attr_sp == "sp") {
    stop("sf objects must be used instead of sp objects. To convert your sp object into an sf object, run sf::st_as_sf().", call. = FALSE)
  }

  if (inherits(data, "sf")) {
    is_sf <- TRUE
    sf_column_name <- attributes(data)$sf_column
    crs <- attributes(data[[sf_column_name]])$crs
  } else {
    is_sf <- FALSE
    sf_column_name <- NULL
    crs <- NULL
  }

  # create distance matrix (if not provided) -- sf::st_intersects() assumes
  # units are nieghbors with themselves, so we need to set the diagonal of the
  # matrix equal to zero
  if (is.null(W)) {
    geom_type <- st_geometry_type(data, by_geometry = FALSE)
    if (geom_type == "POINT") {
      if (is.null(cutoff)) {
        stop("cutoff must be specified if using a distance-based neighbor cutoff.", call. = FALSE)
      }
      coords_val <- st_coordinates(data)
      W <- 1 * (as.matrix(dist(coords_val)) <= cutoff)
      diag(W) <- 0
      if (sum(W) == 0) {
        stop("cutoff must be larger than the smallest distance between potential neighbors.", call. = FALSE)
      }
    } else {
      W <- sf::st_intersects(data, sparse = FALSE)
      diag(W) <- 0
    }
  }

  # turn W into a sparse Matrix and logical regardless of whether provided by us or user
  W <- 1 * Matrix::Matrix(W, sparse = TRUE)
  W_rowsums <- Matrix::rowSums(W)
  is_W_connected <- all(W_rowsums > 0)

  # make M if necessary
  if (row_st) {
    if (!is.null(M)) {
      if (inherits(spcov_initial, "car")) {
        warning("Overriding M when row_st = TRUE", call. = FALSE)
      }
      if (inherits(spcov_initial, "sar")) {
        warning("M ignored for sar models", call. = FALSE)
      }
    }
    M <- 1 / W_rowsums # this has not been standardized
  } else {
    if (is.null(M)) {
      M <- rep(1, nrow(W)) # assume identity
    } else {
      if (inherits(spcov_initial, "sar")) {
        warning("M ignored for sar models", call. = FALSE)
      }
      M <- as.matrix(M) # coerce to matrix from vector, matrix, or Matrix
      if (dim(M)[1] == dim(M)[2]) {
        M <- diag(M) # take diagonal of matrix
      } else {
        M <- as.vector(M) # assume diagonal already given as one-column vector
      }
    }
  }

  # row standardize W if necessary
  if (row_st) {
    W_rowsums_val <- W_rowsums # make copy so rowsums are saved later
    W_rowsums_val[W_rowsums_val == 0] <- 1 # not a Matrix object so this subsetting is okay
    W <- W / W_rowsums_val
  }


  if (inherits(spcov_initial, "car") && !isSymmetric(as.matrix((Matrix(diag(nrow(W)), sparse = TRUE) - W) * 1 / M))) {
    stop("W and M must satisfy the CAR symmetry condition", call. = FALSE)
  }

  # find eigenvalues of W for connected sites
  rowsums_nonzero <- which(W_rowsums != 0)
  W_eigen <- Re(eigen(W[rowsums_nonzero, rowsums_nonzero])$values)
  if (range_positive) {
    rho_lb <- 1e-5
  } else {
    rho_lb <- 1 / min(W_eigen) + 1e-5 # rho strictly > lb
  }
  rho_ub <- 1 / max(W_eigen) - 1e-5 # rho strictly < ub


  # subsetting by na and not na values
  ## find response variabale name
  na_index <- is.na(data[[all.vars(formula)[1]]])
  # get indices
  observed_index <- which(!na_index)
  missing_index <- which(na_index)
  if (any(na_index)) {
    ## find newdata to be used in prediction later
    newdata <- data[missing_index, , drop = FALSE]
    ## subset original data
    obdata <- data[observed_index, , drop = FALSE]
  } else {
    obdata <- data
    newdata <- NULL
  }


  # finding model frame
  obdata_model_frame <- model.frame(formula, obdata, drop.unused.levels = TRUE, na.action = na.pass)
  # finding contrasts as ...
  dots <- list(...)
  if (!"contrasts" %in% names(dots)) {
    dots$contrasts <- NULL
  }
  # model matrix with potential NA
  X <- model.matrix(formula, obdata_model_frame, contrasts = dots$contrasts)
  # finding rows w/out NA
  ob_predictors <- complete.cases(X)
  if (any(!ob_predictors)) {
    stop("Cannot have NA values in predictors.", call. = FALSE)
  }


  # store X and y
  obdata_model_frame <- model.frame(formula, obdata, drop.unused.levels = TRUE, na.action = na.omit)
  # store terms
  terms_val <- terms(obdata_model_frame)
  dots <- list(...)
  if (!"contrasts" %in% names(dots)) {
    dots$contrasts <- NULL
  }
  X <- model.matrix(formula, obdata_model_frame, contrasts = dots$contrasts)
  # find induced contrasts and xlevels
  dots$contrasts <- attr(X, "contrasts")
  xlevels <- .getXlevels(terms_val, obdata_model_frame)
  y <- as.matrix(model.response(obdata_model_frame), ncol = 1)
  # adjust response to reflect offset
  offset <- model.offset(obdata_model_frame)
  if (!is.null(offset)) {
    offset <- as.matrix(offset, ncol = 1)
    y <- y - offset
  }

  # see if response is numeric
  if (!is.numeric(y)) {
    stop("Response variable must be numeric", call. = FALSE)
  }

  # error if no variance
  if (var(y) == 0) {
    stop("The response has no variability. Model fit unreliable.", call. = FALSE)
  }

  # store n, p, and ones
  n <- NROW(obdata)
  p <- as.numeric(Matrix::rankMatrix(X, method = "qr"))
  if (p < NCOL(X)) {
    warning("There are perfect collinearities detected in X (the matrix of explanatory variables). This may make the model fit unreliable or may cause an error while model fitting. Consider removing redundant explanatory variables and refitting the model.", call. = FALSE)
  }
  ones <- matrix(1, nrow = n, ncol = 1)

  # error if p >= n
  if (p >= n) {
    stop("The number of fixed effects is at least as large as the number of observations (p >= n). Consider reducing the number of fixed effects and rerunning spautor().", call. = FALSE)
  }

  # find s2 for initial values
  # s2 <- summary(lm(formula, obdata))$sigma^2
  qr_val <- qr(X)
  R_val <- qr.R(qr_val)
  betahat <- backsolve(R_val, qr.qty(qr_val, y))
  resid <- y - X %*% betahat
  s2 <- sum(resid^2) / (n - p)
  diagtol <- 0

  # store random effects list
  if (is.null(random)) {
    randcov_initial <- NULL
    randcov_names <- NULL
    randcov_Zs <- NULL
  } else {
    randcov_names <- get_randcov_names(random)
    randcov_Zs <- get_randcov_Zs(data, randcov_names)
    if (is.null(randcov_initial)) {
      randcov_initial <- spmodel::randcov_initial()
    } else {
      randcov_given_names <- unlist(lapply(
        names(randcov_initial$initial),
        function(x) labels(terms(reformulate(x)))
      ))
      randcov_initial_names <- unlist(lapply(randcov_given_names, get_randcov_name))
      if (length(randcov_initial_names) != length(names(randcov_initial$initial))) {
        stop("No / can be specified in randcov_initial(). Please specify starting
             values for each variable (e.g., a/b = a + a:b)", call. = FALSE)
      }
      names(randcov_initial$initial) <- randcov_initial_names
      names(randcov_initial$is_known) <- randcov_initial_names
    }
  }

  # partition matrix error
  # if (!is.null(partition_factor)) {
  #   partition_factor_labels <- labels(terms(partition_factor))
  #   if (length(partition_factor_labels) > 1) {
  #     stop("Only one variable can be specified in partition_factor.", call. = FALSE)
  #   }
  #   partition_factor <- reformulate(paste0("as.character(", partition_factor_labels, ")"), intercept = FALSE)
  # }
  # coerce to factor
  if (!is.null(partition_factor)) {
    partition_factor_labels <- labels(terms(partition_factor))
    if (length(partition_factor_labels) > 1) {
      stop("Only one variable can be specified in partition_factor.", call. = FALSE)
    }
    partition_mf <- model.frame(partition_factor, obdata)
    if (any(!attr(terms(partition_mf), "dataClasses") %in% c("character", "factor", "ordered"))) {
      stop("Partition factor variable must be categorical or factor.", call. = FALSE)
    }
    partition_factor <- reformulate(partition_factor_labels, intercept = FALSE)
    # partition_factor <- reformulate(paste0("as.character(", partition_factor_labels, ")"), intercept = FALSE)
  }

  # store partition matrix list
  if (!is.null(partition_factor)) {
    partition_matrix <- partition_matrix(partition_factor, data)
  } else {
    partition_matrix <- NULL
  }

  list(
    anisotropy = FALSE, contrasts = dots$contrasts, crs = crs,
    formula = formula, data = data, is_sf = is_sf, is_W_connected = is_W_connected,
    missing_index = missing_index, n = n,
    obdata = obdata, observed_index = observed_index, offset = offset, ones = ones, newdata = newdata, p = p,
    partition_factor = partition_factor, partition_matrix = partition_matrix,
    randcov_initial = randcov_initial, randcov_names = randcov_names, randcov_Zs = randcov_Zs,
    sf_column_name = sf_column_name, terms = terms_val, W = W, W_rowsums = W_rowsums, M = M,
    rho_lb = rho_lb, rho_ub = rho_ub,
    X = X, y = y, xlevels = xlevels, s2 = s2, diagtol = diagtol
  )
}
