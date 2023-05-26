## ---- include = FALSE---------------------------------------------------------
# # jss style
# knitr::opts_chunk$set(prompt=TRUE, echo = TRUE, highlight = FALSE, continue = " + ", comment = "")
# options(replace.assign=TRUE, width=90, prompt="R> ")

# rmd style
knitr::opts_chunk$set(collapse = FALSE, comment = "#>", warning = FALSE, message = FALSE)

# load packages
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

## -----------------------------------------------------------------------------
ggplot(moss, aes(color = log_Zn)) +
  geom_sf() +
  scale_color_viridis_c()

## -----------------------------------------------------------------------------
spmod <- splm(log_Zn ~ log_dist2road, data = moss, spcov_type = "exponential")

## -----------------------------------------------------------------------------
print(spmod)

## -----------------------------------------------------------------------------
summary(spmod)

## -----------------------------------------------------------------------------
tidy(spmod)

## -----------------------------------------------------------------------------
glance(spmod)

## -----------------------------------------------------------------------------
lmod <- splm(log_Zn ~ log_dist2road, data = moss, spcov_type = "none")
glances(spmod, lmod)

## -----------------------------------------------------------------------------
augment(spmod)

## -----------------------------------------------------------------------------
ggplot(sulfate, aes(color = sulfate)) +
  geom_sf(size = 2) +
  scale_color_viridis_c(limits = c(0, 45))

## -----------------------------------------------------------------------------
sulfmod <- splm(sulfate ~ 1, data = sulfate, spcov_type =  "spherical")

## -----------------------------------------------------------------------------
sulfate_preds$preds <- predict(sulfmod, newdata = sulfate_preds)

## -----------------------------------------------------------------------------
ggplot(sulfate_preds, aes(color = preds)) +
  geom_sf(size = 2) +
  scale_color_viridis_c(limits = c(0, 45))

## -----------------------------------------------------------------------------
sulfate_preds$preds <- NULL

## -----------------------------------------------------------------------------
augment(sulfmod, newdata = sulfate_preds)

## -----------------------------------------------------------------------------
augment(sulfmod, newdata = sulfate_preds, interval = "prediction")

## -----------------------------------------------------------------------------
caribou

## -----------------------------------------------------------------------------
cariboumod <- splm(z ~ water + tarp, data = caribou,
                   spcov_type = "exponential", xcoord = x, ycoord = y)

## -----------------------------------------------------------------------------
anova(cariboumod)

## -----------------------------------------------------------------------------
moose

## -----------------------------------------------------------------------------
ggplot(moose, aes(color = presence)) +
  scale_color_viridis_d() +
  geom_sf(size = 2) 

## -----------------------------------------------------------------------------
binmod <- spglm(presence ~ elev, family = "binomial",
                data  = moose, spcov_type = "exponential")

## -----------------------------------------------------------------------------
print(binmod)

## -----------------------------------------------------------------------------
summary(binmod)

## -----------------------------------------------------------------------------
tidy(binmod)

## -----------------------------------------------------------------------------
glance(binmod)

## -----------------------------------------------------------------------------
glmod <- spglm(presence ~ elev, family = "binomial", data = moose, spcov_type = "none")
glances(binmod, glmod)

## -----------------------------------------------------------------------------
augment(binmod)

## -----------------------------------------------------------------------------
moose_preds$preds <- predict(binmod, newdata = moose_preds, type = "response")

## -----------------------------------------------------------------------------
ggplot(moose_preds, aes(color = preds)) + 
  geom_sf(size = 2) +
  scale_color_viridis_c(limits = c(0, 1))

## -----------------------------------------------------------------------------
moose_preds$preds <- NULL
augment(binmod, newdata = moose_preds, type = "response", interval = "prediction")

