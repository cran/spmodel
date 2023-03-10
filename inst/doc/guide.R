## ----setup, include = FALSE---------------------------------------------------
# # jss style
# knitr::opts_chunk$set(prompt=TRUE, echo = TRUE, highlight = FALSE, continue = " + ", comment = "")
# options(replace.assign=TRUE, width=90, prompt="R> ")

# rmd style
knitr::opts_chunk$set(collapse = FALSE, comment = "#>", warning = FALSE, message = FALSE)

# loading
library(ggplot2)
library(spmodel)

## ---- eval = FALSE------------------------------------------------------------
#  library(spmodel)

## -----------------------------------------------------------------------------
citation(package = "spmodel")

## ---- eval = FALSE------------------------------------------------------------
#  library(ggplot2)

## -----------------------------------------------------------------------------
moss

## ----log_zn, fig.cap="Distribution of log zinc concentration in the moss data.", out.width = "65%", fig.align="center"----
ggplot(moss, aes(color = log_Zn)) +
  geom_sf(size = 2) +
  scale_color_viridis_c() +
  theme_gray(base_size = 14)

## ---- eval = FALSE------------------------------------------------------------
#  mapview(moss, zcol = "log_Zn")

## -----------------------------------------------------------------------------
spmod <- splm(log_Zn ~ log_dist2road, moss, spcov_type = "exponential")

## -----------------------------------------------------------------------------
summary(spmod)

## -----------------------------------------------------------------------------
coef(spmod)

## ---- echo = FALSE------------------------------------------------------------
spcov_params_val <- coef(spmod, type = "spcov")
de_val <- as.vector(round(spcov_params_val[["de"]], digits = 3))
ie_val <- as.vector(round(spcov_params_val[["ie"]], digits = 3))
range_val <- as.vector(round(spcov_params_val[["range"]], digits = 0))
eff_range_val <- 3 * range_val

## -----------------------------------------------------------------------------
coef(spmod, type = "spcov")

## ----emp_spcov, fig.cap="Empirical spatial covariance of fitted model.", out.width="75%", fig.align="center"----
plot(spmod, which = 7)

## -----------------------------------------------------------------------------
pseudoR2(spmod)

## -----------------------------------------------------------------------------
lmod <- splm(log_Zn ~ log_dist2road, moss, spcov_type = "none")

## -----------------------------------------------------------------------------
AIC(spmod, lmod)
AICc(spmod, lmod)

## -----------------------------------------------------------------------------
loocv(spmod)
loocv(lmod)

## ---- eval = FALSE------------------------------------------------------------
#  hatvalues(spmod)

## ---- eval = FALSE------------------------------------------------------------
#  fitted(spmod)

## ---- eval = FALSE------------------------------------------------------------
#  residuals(spmod)

## ---- eval = FALSE------------------------------------------------------------
#  residuals(spmod, type = "pearson")

## ---- eval = FALSE------------------------------------------------------------
#  residuals(spmod, type = "standardized")

## ---- results = "hide"--------------------------------------------------------
rstandard(spmod)

## ----r_vs_f, fig.cap="Standardized residuals vs fitted values of fitted model.", out.width="75%", fig.align="center", eval = FALSE----
#  plot(spmod, which = 1) # figure omitted

## ---- eval = FALSE------------------------------------------------------------
#  plot(spmod, which = 2) # figure omitted

## ---- eval = FALSE------------------------------------------------------------
#  cooks.distance(spmod)

## ----d_vs_l, fig.cap="Cook's distance vs leverage of fitted model.", out.width="75%", fig.align="center", eval = FALSE----
#  plot(spmod, which = 6) # figure omitted

## -----------------------------------------------------------------------------
tidy(spmod)

## -----------------------------------------------------------------------------
glance(spmod)

## -----------------------------------------------------------------------------
glances(spmod, lmod)

## -----------------------------------------------------------------------------
augment(spmod)

## -----------------------------------------------------------------------------
seal

## ----log_trend, fig.cap="Distribution of log seal trends in the seal data. Polygons are gray if seal trends are missing.", out.width = "65%", fig.align="center"----
ggplot(seal, aes(fill = log_trend)) +
  geom_sf(size = 0.75) +
  scale_fill_viridis_c() +
  theme_bw(base_size = 14) 

## ---- eval = FALSE------------------------------------------------------------
#  mapview(seal, zcol = "log_trend")

## -----------------------------------------------------------------------------
sealmod <- spautor(log_trend ~ 1, seal, spcov_type = "car")

## -----------------------------------------------------------------------------
summary(sealmod)
tidy(sealmod)
glance(sealmod)
augment(sealmod)

## ---- eval = FALSE------------------------------------------------------------
#  ggplot(sulfate, aes(color = sulfate)) +
#    geom_sf(size = 2.5) +
#    scale_color_viridis_c(limits = c(0, 45)) +
#    theme_gray(base_size = 18)

## -----------------------------------------------------------------------------
sulfmod <- splm(sulfate ~ 1, sulfate, spcov_type = "spherical")

## ---- results = "hide"--------------------------------------------------------
sulfate_preds$preds <- predict(sulfmod, newdata = sulfate_preds)

## ---- eval = FALSE------------------------------------------------------------
#  ggplot(sulfate_preds, aes(color = preds)) +
#    geom_sf(size = 2.5) +
#    scale_color_viridis_c(limits = c(0, 45)) +
#    theme_gray(base_size = 18)

## ----sulfate, fig.cap="Distribution of observed sulfate (left) and sulfate predictions (right) in the conterminous United States.", out.width = "49%", fig.align="center", fig.show="hold", echo = FALSE----
ggplot(sulfate, aes(color = sulfate)) +
  geom_sf(size = 2.5) +
  scale_color_viridis_c(limits = c(0, 45)) +
  theme_gray(base_size = 18)

sulfate_preds$preds <- predict(sulfmod, newdata = sulfate_preds)
ggplot(sulfate_preds, aes(color = preds)) +
  geom_sf(size = 2.5) +
  scale_color_viridis_c(limits = c(0, 45)) +
  theme_gray(base_size = 18)

## ---- eval = FALSE------------------------------------------------------------
#  predict(sulfmod, newdata = sulfate_preds, se.fit = TRUE)

## ---- eval = FALSE------------------------------------------------------------
#  predict(sulfmod, newdata = sulfate_preds, interval = "prediction")

## ---- eval = FALSE------------------------------------------------------------
#  predict(sulfmod, newdata = sulfate_preds, interval = "confidence")

## -----------------------------------------------------------------------------
sulfate_preds$preds <- NULL

## -----------------------------------------------------------------------------
augment(sulfmod, newdata = sulfate_preds, interval = "prediction", level = 0.90)

## -----------------------------------------------------------------------------
sulfate_preds$sulfate <- NA
sulfate_with_NA <- rbind(sulfate, sulfate_preds)

## -----------------------------------------------------------------------------
sulfmod_with_NA <- splm(sulfate ~ 1, sulfate_with_NA, "spherical")

## -----------------------------------------------------------------------------
sulfmod_with_NA$newdata

## ---- eval = FALSE------------------------------------------------------------
#  predict(sulfmod_with_NA)

## ---- eval = FALSE------------------------------------------------------------
#  predict(sulfmod_with_NA, newdata = sulfmod_with_NA$newdata)

## -----------------------------------------------------------------------------
augment(sulfmod_with_NA, newdata = sulfmod_with_NA$newdata)

## ---- eval = FALSE------------------------------------------------------------
#  predict(sealmod)

## -----------------------------------------------------------------------------
augment(sealmod, newdata = sealmod$newdata)

## -----------------------------------------------------------------------------
init <- spcov_initial("exponential", ie = 0, known = "ie")
print(init)

## -----------------------------------------------------------------------------
spmod_red <- splm(log_Zn ~ log_dist2road, moss, spcov_initial = init)

## -----------------------------------------------------------------------------
glances(spmod, spmod_red)

## -----------------------------------------------------------------------------
spmods <- splm(sulfate ~ 1, sulfate, spcov_type = c("exponential", "spherical", "none"))

## -----------------------------------------------------------------------------
glances(spmods)

## ---- eval = FALSE------------------------------------------------------------
#  predict(spmods, newdata = sulfate_preds)

## -----------------------------------------------------------------------------
AIC(spmods$exponential)

## -----------------------------------------------------------------------------
rand1 <- splm(
  log_Zn ~ log_dist2road,
  moss,
  spcov_type = "exponential",
  random = ~ sample
)

## -----------------------------------------------------------------------------
rand2 <- splm(
  log_Zn ~ log_dist2road,
  moss,
  spcov_type = "exponential",
  random = ~ sample + (log_dist2road | year)
)

## -----------------------------------------------------------------------------
glances(rand1, rand2)

## ---- eval = FALSE------------------------------------------------------------
#  part <- splm(
#    log_Zn ~ log_dist2road,
#    moss,
#    spcov_type = "exponential",
#    partition_factor = ~ year
#  )

## ----anisotropy, echo = FALSE, out.width = "49%", fig.show = "hold", fig.cap = "Ellipses for an isotropic (left) and anisotropic (right) covariance function centered at the origin. The black outline of each ellipse is a level curve of equal correlation."----
# PRELIMINARIES 
r <- 1
theta_seq <- seq(0, 2 * pi, length.out = 1000)
x_orig <- r * cos(theta_seq)
y_orig <- r * sin(theta_seq)
df_orig <- data.frame(x = x_orig, y = y_orig)

# FIRST FIGURE
ggplot(df_orig, aes(x = x, y = y)) + 
  geom_point() +
  scale_x_continuous(limits = c(-1, 1), breaks = c(0)) + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(0)) +
  labs(x = "x-distance", y = "y-distance") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18)) +
  coord_fixed() # theme_gray() causes polygon edge not found bug

# SECOND FIGURE
theta <- pi / 4 # (30 degrees)
R <- 1 / 3
scale <- matrix(c(1, 0, 0, R), nrow = 2, byrow = TRUE)
rotate <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), nrow = 2, byrow = TRUE)
transform <- rotate %*% scale
coords <- transform %*% rbind(df_orig[["x"]], df_orig[["y"]])
df <- data.frame(x = coords[1, ], y = coords[2, ])
ggplot(df, aes(x = x, y = y)) + 
  geom_point() + 
  scale_x_continuous(limits = c(-1, 1), breaks = c(0)) + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(0)) +
  labs(x = "x-distance", y = "y-distance") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18)) +
  coord_fixed() # theme_gray() causes polygon edge not found bug

## -----------------------------------------------------------------------------
spmod_anis <- splm(
  log_Zn ~ log_dist2road,
  moss,
  spcov_type = "exponential",
  anisotropy = TRUE
)
summary(spmod_anis)

## ---- anisotropy_fit, echo = FALSE, out.width = "33%", fig.show = "hold", fig.cap = "A visual representation of the anisotropy transformation. In the left figure, the first step is to rotate the anisotropic ellipse clockwise by the \\texttt{rotate} parameter (here \\texttt{rotate} is 0.75 radians or 135 degrees). In the middle figure, the second step is to scale the y axis by the reciprocal of the \\texttt{scale} parameter (here \\texttt{scale} is 0.5). In the right figure, the anisotropic ellipse has been transformed into an isotropic one (i.e., a circle). The transformed coordinates are then used instead of the original coordinates to compute distances and spatial covariances."----
spcov_params_val <- coef(spmod_anis, type = "spcov")
# FIRST FIGURE
theta <- 3 * pi / 4
R <- 1 / 2
scale <- matrix(c(1, 0, 0, R), nrow = 2, byrow = TRUE)
rotate <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), nrow = 2, byrow = TRUE)
transform <- rotate %*% scale
coords <- transform %*% rbind(df_orig[["x"]], df_orig[["y"]])
df <- data.frame(x = coords[1, ], y = coords[2, ])
ggplot(df, aes(x = x, y = y)) + 
  geom_point() + 
  scale_x_continuous(limits = c(-1, 1), breaks = c(0)) + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(0)) +
  labs(x = "", y = "") +
  theme_gray(base_size = 20) +
  coord_fixed() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  geom_curve(aes(x = 0.5, xend = 0.9, y = 0.85, yend = 0.55),
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = -0.45, angle = 90, size = 1.5) +
  annotate("text", x = 0.52, y = 0.65, label = "rotate ", size = 10)

# SECOND FIGURE
theta <- 0
R <- spcov_params_val["scale"]
scale <- matrix(c(1, 0, 0, R), nrow = 2, byrow = TRUE)
rotate <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), nrow = 2, byrow = TRUE)
transform <- rotate %*% scale
coords <- transform %*% rbind(df_orig[["x"]], df_orig[["y"]])
df <- data.frame(x = coords[1, ], y = coords[2, ])
ggplot(df, aes(x = x, y = y)) + 
  geom_point() + 
  scale_x_continuous(limits = c(-1, 1), breaks = c(0)) + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(0)) +
  labs(x = "", y = "") +
  theme_gray(base_size = 20) +
  coord_fixed() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  geom_curve(aes(x = 0.1, xend = 0.1, y = 0.55, yend = 0.99),
             arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = 0, angle = 0, size = 1.5) +
  annotate("text", x = 0.45, y = 0.77, label = "frac(1,scale)", parse = TRUE, size = 10)

# THIRD FIGURE
theta <- 0
R <- 1
scale <- matrix(c(1, 0, 0, R), nrow = 2, byrow = TRUE)
rotate <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), nrow = 2, byrow = TRUE)
transform <- rotate %*% scale
coords <- transform %*% rbind(df_orig[["x"]], df_orig[["y"]])
df <- data.frame(x = coords[1, ], y = coords[2, ])
ggplot(df, aes(x = x, y = y)) + 
  geom_point() + 
  scale_x_continuous(limits = c(-1, 1), breaks = c(0)) + 
  scale_y_continuous(limits = c(-1, 1), breaks = c(0)) +
  labs(x = "", y = "") +
  theme_gray(base_size = 20) +
  coord_fixed() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

## -----------------------------------------------------------------------------
sim_params <- spcov_params("exponential", de = 5, ie = 1, range = 0.5)

## -----------------------------------------------------------------------------
set.seed(0)
n <- 3000
x <- runif(n)
y <- runif(n)
sim_coords <- tibble::tibble(x, y)
sim_response <- sprnorm(sim_params, data = sim_coords, xcoord = x, ycoord = y)
sim_data <- tibble::tibble(sim_coords, sim_response)

## ----sim, fig.align="center", out.width = "75%", fig.cap = "Spatial data simulated in the unit square.", eval = FALSE----
#  ggplot(sim_data, aes(x = x, y = y, color = sim_response)) +
#    geom_point(size = 1.5) +
#    scale_color_viridis_c(limits = c(-7, 7)) +
#    theme_gray(base_size = 18)

## -----------------------------------------------------------------------------
local1 <- splm(sim_response ~ 1, sim_data, spcov_type = "exponential", 
               xcoord = x, ycoord = y, local = TRUE)
summary(local1)

## -----------------------------------------------------------------------------
local2_list <- list(method = "kmeans", groups = 60, var_adjust = "pooled",
                    parallel = TRUE, ncores = 2)
local2 <- splm(sim_response ~ 1, sim_data, spcov_type = "exponential", 
               xcoord = x, ycoord = y, local = local2_list)

## -----------------------------------------------------------------------------
n_pred <- 1000
x <- runif(n_pred)
y <- runif(n_pred)
sim_preds <- tibble::tibble(x = x, y = y)

## ---- results = "hide"--------------------------------------------------------
sim_preds$preds <- predict(local1, newdata = sim_preds, local = TRUE)

## ---- eval = FALSE------------------------------------------------------------
#  ggplot(sim_preds, aes(x = x, y = y, color = preds)) +
#    geom_point(size = 1.5) +
#    scale_color_viridis_c(limits = c(-7, 7)) +
#    theme_gray(base_size = 18)

## ----sim_preds, echo = FALSE, fig.align="center", fig.show = "hold", out.width = "49%", fig.cap = "Observed data and big data predictions at unobserved locations. In the left figure, spatial data are simulated in the unit square. A spatial linear model is fit using the default big data approximation for model-fitting. In the right figure, predictions are made using the fitted model and the default big data approximation for prediction."----

ggplot(sim_data, aes(x = x, y = y, color = sim_response)) +
  geom_point(size = 1.5) +
  scale_color_viridis_c(limits = c(-7, 7)) + 
  theme_gray(base_size = 18)

ggplot(sim_preds, aes(x = x, y = y, color = preds)) +
  geom_point(size = 1.5) +
  scale_color_viridis_c(limits = c(-7, 7)) + 
  theme_gray(base_size = 18)

## ---- results = "hide"--------------------------------------------------------
pred_list <- list(method = "distance", size = 30, parallel = TRUE, ncores = 2)
predict(local1, newdata = sim_preds, local = pred_list)

## -----------------------------------------------------------------------------
sulfate$var <- rnorm(NROW(sulfate))
sulfate_preds$var <- rnorm(NROW(sulfate_preds))

## ---- eval = FALSE------------------------------------------------------------
#  sprfmod <- splmRF(sulfate ~ var, sulfate, spcov_type = "exponential")

## ---- eval = FALSE------------------------------------------------------------
#  predict(sprfmod, newdata = sulfate_preds)

## -----------------------------------------------------------------------------
caribou

## -----------------------------------------------------------------------------
cariboumod <- splm(z ~ water + tarp, data = caribou,
                   spcov_type = "exponential", xcoord = x, ycoord = y)

## -----------------------------------------------------------------------------
tidy(anova(cariboumod))

## -----------------------------------------------------------------------------
coords <- cbind(caribou$x, caribou$y)
dists <- as.matrix(dist(coords))
W <- dists == 1

## -----------------------------------------------------------------------------
W <- W * 1

## -----------------------------------------------------------------------------
cariboumod <- spautor(z ~ water + tarp, data = caribou,
                      spcov_type = "car", W = W)

## -----------------------------------------------------------------------------
tidy(anova(cariboumod))

