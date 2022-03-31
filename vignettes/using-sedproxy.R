## ----knitr_setup, echo=FALSE--------------------------------------------------
knitr::opts_chunk$set(dev = "svg")

## ---- eval=FALSE--------------------------------------------------------------
#  if (!require("devtools")) {
#    install.packages("devtools")
#  }
#  
#  devtools::install_github("EarthSystemDiagnostics/sedproxy")

## ----run_shiny, eval=FALSE----------------------------------------------------
#  library(sedproxy)
#  ShinySedproxy()

## ----message = FALSE, warning = FALSE-----------------------------------------
library(tidyverse)
library(knitr)
library(sedproxy)

## -----------------------------------------------------------------------------
N41.proxy.details %>% 
  mutate_if(is.numeric, round, digits = 2) %>% 
  gather() %>% 
  kable(., format = "markdown", digits = 2)

## -----------------------------------------------------------------------------
(N41.t21k.climate[1:5,]-273.15) %>% 
  kable(., format = "markdown", digits = 2)

## -----------------------------------------------------------------------------
kable(head(N41.proxy), format = "markdown")

## ---- fig.show='hold', warning=FALSE------------------------------------------
set.seed(26052017)
clim.in <- N41.t21k.climate[nrow(N41.t21k.climate):1,] - 273.15

# The input climate signal should be a time series object
# The Trace simulation runs to the year 1990 AD, therefore the start time for 
# the input climate is -39 years BP
clim.in <- ts(clim.in, start = -39)

PFM <- ClimToProxyClim(clim.signal = clim.in,
                       timepoints = round(N41.proxy$Published.age),
                       calibration.type = "identity",
                       habitat.weights = N41.G.ruber.seasonality,
                       sed.acc.rate = N41.proxy$Sed.acc.rate.cm.ka,
                       sigma.meas = 0.46,
                       sigma.ind = 0,
                       n.samples = 30,
                       n.replicates = 10)

## -----------------------------------------------------------------------------
PFM$everything

## ----default_plot, fig.width=6, fig.height=5.5--------------------------------
PFM %>% 
  PlotPFMs(max.replicates = 1)

## ----plot_reps, fig.width=6, fig.height=5.5-----------------------------------
PFM %>%
  PlotPFMs(.,
           plot.stages = c("simulated.proxy"),
           max.replicates = 5)

## ----MgCa, fig.show='hold', warning=FALSE, echo=FALSE, eval=TRUE--------------
set.seed(26052017)
PFM_2 <- ClimToProxyClim(clim.signal = clim.in, 
                         timepoints = round(N41.proxy$Published.age),
                         calibration.type = "MgCa",
                         #calibration = "Ten planktonic species_350-500",
                         habitat.weights = N41.G.ruber.seasonality,
                         sed.acc.rate = N41.proxy$Sed.acc.rate.cm.ka,
                         sigma.meas = 0.46,
                         sigma.ind = 0,
                         n.samples = 30,
                         n.replicates = 1)

## ----MgCa_plot, fig.width=6, fig.height=5.5, echo=FALSE, eval=TRUE------------
PFM_2 %>%
  PlotPFMs(.) 

