## ----setup, include = FALSE---------------------------------------------------
# # jss style
# knitr::opts_chunk$set(prompt=TRUE, echo = TRUE, highlight = FALSE, continue = " + ", comment = "")
# options(replace.assign=TRUE, width=90, prompt="R> ")

# rmd style
knitr::opts_chunk$set(collapse = FALSE, comment = "#>", warning = FALSE, message = FALSE)

# loading
library(ggplot2)
library(spmodel)

## -----------------------------------------------------------------------------
citation(package = "spmodel")

## ----anisotropy, echo = FALSE, out.width = "50%", fig.show = "hold", fig.cap = "In the left figure, the ellipse of an isotropic spatial covariance function centered at the origin is shown. In the right figure, the ellipse of an anisotropic spatial covariance function centered at the origin is shown. The black outline of each ellipse is a level curve of equal correlation. "----
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

## ----anisotropy2, echo = FALSE, out.width = "33%", fig.show = "hold", fig.cap = "In the left figure, the ellipse of an anisotropic spatial covariance function centered at the origin is shown. The blue lines represent the original axes and the red lines the transformed axes. The solid lines represent the x-axes and the dotted lines the y-axes. Note that the solid, red line is the major axis of the ellpise and the dashed, red line is the minor axis of the ellipse. In the center figure, the ellipse has been rotated clockwise by the rotate parameter so the major axis is the transformed x-axis and the minor axis is the transformed y-axis. In the right figure, the minor axis of the ellipse has been scaled by the reciprocal of the scale parameter so that the ellipse becomes a circle, which corresponds to an isotropic spatial covariance function. The transformed coordinates are then used to compute distances and spatial covariances."----
# PRELIMINARIES
r <- 1
theta_seq <- seq(0, 2 * pi, length.out = 1000)
x_orig <- r * cos(theta_seq)
y_orig <- r * sin(theta_seq)
df_orig <- data.frame(x = x_orig, y = y_orig)

# FIRST FIGURE
theta <- pi / 4 # (30 degrees)
R <- 1 / 3 # (minor axis length / major axis length)
scale <- matrix(c(1, 0, 0, R), nrow = 2, byrow = TRUE)
rotate <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), nrow = 2, byrow = TRUE)
transform <- rotate %*% scale
coords <- transform %*% rbind(df_orig[["x"]], df_orig[["y"]])
df <- data.frame(x = coords[1, ], y = coords[2, ])
ggplot(df, aes(x = x, y = y)) + 
  geom_point() + 
  lims(x = c(-1, 1), y = c(-1, 1))  +
  geom_hline(yintercept = 0, col = "blue") + 
  geom_vline(xintercept = 0, col = "blue", lty = "dashed") + 
  geom_abline(intercept = 0, slope = 1, col = "red") +
  geom_abline(intercept = 0, slope = -1, col = "red", lty = "dashed") +
  theme_gray() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  ) +
  coord_fixed()

# SECOND FIGURE
rotate_anis <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), nrow = 2, byrow = TRUE)
coords_anis <- rotate_anis %*% rbind(df[["x"]], df[["y"]])
df_anis <- data.frame(x = coords_anis[1, ], y = coords_anis[2, ])
ggplot(df_anis, aes(x = x, y = y)) + 
  geom_point() +
  lims(x = c(-1, 1), y = c(-1, 1))  +
  geom_hline(yintercept = 0, col = "red") + 
  geom_vline(xintercept = 0, col = "red", lty = "dashed") + 
  geom_abline(intercept = 0, slope = 1, col = "blue", lty = "dashed") +
  geom_abline(intercept = 0, slope = -1, col = "blue") +
  theme_gray() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  ) +
  coord_fixed()

# THIRD FIGURE
unscale <- matrix(c(1, 0, 0, 1 / R), nrow = 2, byrow = TRUE)
coords_iso <- unscale %*% rbind(df_anis[["x"]], df_anis[["y"]])
df_iso <- data.frame(x = coords_iso[1, ], y = coords_iso[2, ])
ggplot(df_iso, aes(x = x, y = y)) + 
  geom_point() +
  lims(x = c(-1, 1), y = c(-1, 1))  +
  geom_hline(yintercept = 0, col = "red") + 
  geom_vline(xintercept = 0, col = "red", lty = "dashed") + 
  geom_abline(intercept = 0, slope = 1, col = "blue", lty = "dashed") +
  geom_abline(intercept = 0, slope = -1, col = "blue") +
  theme_gray() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  ) +
  coord_fixed()

