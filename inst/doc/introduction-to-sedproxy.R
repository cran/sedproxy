## ---- eval=FALSE--------------------------------------------------------------
#  # if (!require("devtools")) {
#  #   install.packages("devtools")
#  # }
#  #
#  # devtools::install_github("EarthSystemDiagnostics/sedproxy")

## ----knitr_options------------------------------------------------------------
knitr::opts_chunk$set(echo=TRUE, message = FALSE, warning = FALSE,
                      fig.width = 6, fig.height = 4)

## ----load_packages------------------------------------------------------------
library(sedproxy)
library(dplyr)
library(tidyr)
library(ggplot2)

## -----------------------------------------------------------------------------
clim.in <- N41.t21k.climate[nrow(N41.t21k.climate):1,] - 273.15

## -----------------------------------------------------------------------------
clim.in <- ts(clim.in, start = -39)

## -----------------------------------------------------------------------------
req.timepoints <- seq(1, 20000, by = 100)

## -----------------------------------------------------------------------------
hab.wts <- 1+cos(seq(pi, 3*pi - 2*pi/12, length.out = 12))
hab.wts <- hab.wts / sum(hab.wts)
plot(hab.wts, type = "b")

## -----------------------------------------------------------------------------
PFM <- ClimToProxyClim(clim.signal = clim.in,
                       timepoints = req.timepoints,
                       habitat.weights = hab.wts,
                       sed.acc.rate = 50,
                       sigma.meas = 0.25,
                       sigma.ind = 1,
                       n.samples = 30,
                       
                       # this controls the resolution at which the input climate
                       # is returned and will be plotted later
                       plot.sig.res = 1)

## -----------------------------------------------------------------------------
PlotPFMs(PFM)

## -----------------------------------------------------------------------------
PFM.5.reps <- ClimToProxyClim(clim.signal = clim.in,
                       timepoints = req.timepoints,
                       habitat.weights = hab.wts,
                       sed.acc.rate = 50,
                       sigma.meas = 0.25,
                       sigma.ind = 1,
                       n.samples = 30,
                       plot.sig.res = 1,
                       n.replicates = 5)

## -----------------------------------------------------------------------------
PlotPFMs(PFM.5.reps, plot.stages = c("simulated.proxy"))

## -----------------------------------------------------------------------------
attributes(PFM.5.reps)
is.list(PFM.5.reps)

## -----------------------------------------------------------------------------
PFM.5.reps$everything

## -----------------------------------------------------------------------------
PFM.5.reps$everything %>% 
  filter(stage == "simulated.proxy",
         replicate <= 3) %>% 
  ggplot(aes(x = timepoints, y = value, colour = factor(replicate))) +
  geom_line()

## -----------------------------------------------------------------------------
reps <- subset(PFM.5.reps$everything, stage == "simulated.proxy")

  plot(value~timepoints,
     col = replicate,
     type = "n",
     data = reps)
  
  n.reps <- length(unique(reps$replicate))
  colrs <- rainbow(n.reps)
  
  for (i in seq_along(unique(reps$replicate))){
    lines(value~timepoints,
     col = colrs[i],
     data = reps[reps$replicate == i,])
  }


## -----------------------------------------------------------------------------
pfm.30 <- ClimToProxyClim(
  clim.signal = clim.in,
  timepoints = req.timepoints,
  #calibration.type = "MgCa",
  sed.acc.rate = 50,
  habitat.weights = hab.wts,
  sigma.meas = 0.26, sigma.ind = 2,
  n.samples = 30,
  n.replicates = 300)

pfm.5 <- ClimToProxyClim(
  clim.signal = clim.in,
  timepoints = req.timepoints,
  #calibration.type = "MgCa",
  sed.acc.rate = 50,
  habitat.weights = hab.wts,
  sigma.meas = 0.26, sigma.ind = 2,
  n.samples = 5,
  n.replicates = 300)


# Here we take a shortcut. Instead of actually simulating the measurement of 6 samples at each timepoint we just shrink sigma.meas by sqrt(6) and multiply the number of forams by 6.

pfm.5x6 <- ClimToProxyClim(
  clim.signal = clim.in,
  timepoints = req.timepoints,
  #calibration.type = "MgCa",
  sed.acc.rate = 50,
  habitat.weights = hab.wts,
  sigma.meas = 0.26 / sqrt(6),
  sigma.ind = 2, n.samples = 6*5,
  n.replicates = 300)

pfm.120 <- ClimToProxyClim(
  clim.signal = clim.in,
  timepoints = req.timepoints,
  #calibration.type = "MgCa",
  sed.acc.rate = 50,
  habitat.weights = hab.wts,
  sigma.meas = 0.26,
  sigma.ind = 2, n.samples = 120,
  n.replicates = 300)

pfm.10x12 <- ClimToProxyClim(
  clim.signal = clim.in,
  timepoints = req.timepoints,
  #calibration.type = "MgCa",
  sed.acc.rate = 50,
  habitat.weights = hab.wts,
  sigma.meas = 0.26 / sqrt(10),
  sigma.ind = 2, n.samples = 10*12,
  n.replicates = 300)

## -----------------------------------------------------------------------------
combined.pfms <- bind_rows(`5 forams` = pfm.5$everything,
                           `30 forams` = pfm.30$everything, 
                           #`6*5 forams` = pfm.5x6$everything, 
                           `120 forams` = pfm.120$everything, 
                           `10x12 forams` = pfm.10x12$everything, 
                           .id = "measurement.strategy") %>% 
  mutate(measurement.strategy = 
           factor(measurement.strategy,
                  ordered = TRUE,
                  levels = c("5 forams", "30 forams", "120 forams", "10x12 forams")))

combined.pfms %>% 
  filter(stage %in% c("simulated.proxy")) %>% 
  ggplot(aes(x = timepoints, y = value, group = factor(replicate))) +
  geom_line(alpha = 0.01) +
  facet_wrap(~measurement.strategy)

## -----------------------------------------------------------------------------
mean.error <- combined.pfms %>% 
  group_by(stage, timepoints, measurement.strategy) %>% 
  summarise(mean = mean(value),
            sd = sd(value))


mean.error %>% 
  filter(stage %in% c("simulated.proxy")) %>% 
  ggplot(aes(x = timepoints, y = mean)) +
  geom_line() +
  geom_ribbon(aes(ymax = mean + sd, ymin = mean - sd),
              alpha = 0.2, colour = NA) +
  facet_wrap(~measurement.strategy) +
  theme_bw()

## -----------------------------------------------------------------------------
mean.error %>% 
  filter(stage %in% "simulated.proxy") %>% 
  group_by(stage, measurement.strategy) %>% 
  summarise(mean.error = mean(sd)) %>% 
  knitr::kable()

