## ----knitr_options------------------------------------------------------------
knitr::opts_chunk$set(echo=TRUE, message = FALSE, warning = FALSE,
                      fig.width = 6, fig.height = 4)

## ----load_packages------------------------------------------------------------
library(sedproxy)
library(dplyr)
library(tidyr)
library(ggplot2)

## ----stepchange_climate-------------------------------------------------------
clim.in <- ts(matrix(rep(c(1, 2), each = 50000)))
timepoints <- seq(45000, 55000, 100) 

## ----experimental_setup-------------------------------------------------------
# setup a data.frame/tibble where each row represents 1 pseudo-proxy
expts.fig1 <- crossing(
  SAR = c(6.66, 10, 20, 50),
  taxon = c("warm_opt", "constant", "cold_opt")
)

expts.fig1

## ----habitat_wts--------------------------------------------------------------
warm_opt.wts <- ifelse(clim.in == 1, 100, 10)
cold_opt.wts <- ifelse(clim.in == 2, 100, 10)

# set these weights constant
no.wts <- clim.in
no.wts[,] <- 1

## ----run_pfm_stepchange-------------------------------------------------------
results.fig1 <- expts.fig1 %>% 
  group_by(SAR, taxon) %>% 
  # create a pseudo-proxy for each combination of SAR and taxon
  do({
    PFM <- ClimToProxyClim(
      clim.signal = clim.in,
      timepoints = timepoints,
      # the function switch can be used to select the correct habitat weights
      # depending on the value of taxon
      # within a do() call, columns of the dataframe are accessed 
      # using .$
      habitat.weights = switch(.$taxon,
                               warm_opt = warm_opt.wts,
                               cold_opt = cold_opt.wts,
                               constant = no.wts),
      sed.acc.rate = .$SAR)
    # the last expression in do() should create or access a data.frame. 
    # One dataframe will be created for each combination of SAR and taxon, 
    # these are then combined together at the end.
    PFM$simulated.proxy
    }) %>% 
  ungroup() %>% 
  mutate(
    # convert taxon and SAR to factors for plotting
    SAR = factor(SAR),
    taxon = factor(taxon, ordered = TRUE,
                   levels = c("constant", "cold_opt", "warm_opt")))

## ----plot_stepchange----------------------------------------------------------
results.fig1 %>% 
  ggplot(aes(x = timepoints, y = simulated.proxy,
             colour = SAR, group = SAR)) +
  geom_line() +
  coord_flip() +
  scale_x_reverse() +
  facet_wrap(~taxon) +
  labs(title = "Step change climate input")

## ----gisp_climate.signal------------------------------------------------------
# load interpolated to annual resolution and rescaled gisp2 ice-core record
gisp2.ann <- sedproxy::gisp2.ann

# Create climate.signal matrix from the GISP2 record
clim.in.gisp <- ts(matrix(gisp2.ann$temperature.rescaled))

# timepoints to model proxy at
timepoints <- seq(1200, 46000, 100) 

## ----gisp2_weights------------------------------------------------------------
wts <- tibble(timepoints = as.numeric(time(clim.in.gisp)),
              warm_opt = dnorm(clim.in.gisp, -2, 1),
              cold_opt = dnorm(clim.in.gisp, -0, 1))

wts.long <- wts %>% 
  gather(taxon, wt, -timepoints)

wts.long %>% 
  ggplot(aes(x = timepoints, y = wt, colour = taxon)) +
  geom_line(alpha = 0.75)+
  scale_color_manual(
    values = c("cold_opt" = "blue",
               "warm_opt" = "red")) +
  theme_bw() +
  labs(title = "Flux for warm and cold adapted taxa")


## ----run_gisp_pfm-------------------------------------------------------------
pfm.warm <- ClimToProxyClim(clim.signal = clim.in.gisp,
                    timepoints = timepoints,
                    habitat.weights = wts$warm_opt,
                    sed.acc.rate = 10)

pfm.cold <- ClimToProxyClim(clim.signal = clim.in.gisp,
                    timepoints = timepoints,
                    habitat.weights = wts$cold_opt,
                    sed.acc.rate = 10)

# Access the bits we want to plot
warm_opt <- pfm.warm$simulated.proxy
warm_opt$taxon <- "warm_opt"

cold_opt <- pfm.cold$simulated.proxy
cold_opt$taxon <- "cold_opt"

pfm.warm.cold <- rbind(warm_opt, cold_opt)

## ----plot_gisp_pfm------------------------------------------------------------
pfm.warm.cold %>% 
  ggplot(aes(x = timepoints, y = simulated.proxy, colour = taxon)) +
  geom_line() +
  # add the original climate signal
  geom_line(data = gisp2.ann,
            aes(x = age.yr.bp, y = temperature.rescaled,
                colour = "gisp2"), linetype = 1) +
  # reverse the y axis so that warm is up
  scale_y_reverse() +
  scale_color_manual(
    values = c("cold_opt" = "blue",
               "warm_opt" = "red",
               "gisp2" = "Darkgrey")) +
  theme_bw() +
  labs(title = "Rescaled GISP2 with pseudo-proxies from warm and cold adapted taxa")

## ----gisp_tidyverse-----------------------------------------------------------
# Experimental setup
expts.gisp <- crossing(
  SAR = c(10, 50),
  taxon = c("warm_opt",
            "cold_opt")
)

# call ClimToProxyClim for each taxon
results <- expts.gisp %>% 
  group_by(SAR, taxon) %>% 
  do({
    ClimToProxyClim(clim.signal = clim.in.gisp,
                    timepoints = timepoints,
                    # use switch to pick the correct weights
                    habitat.weights = switch(.$taxon,
                                             warm_opt = wts$warm_opt,
                                             cold_opt = wts$cold_opt),
                    sed.acc.rate = .$SAR)$simulated.proxy
  }) %>% 
  ungroup() %>% 
  mutate(SAR = factor(SAR),
         # convert taxon to factor for plotting 
         taxon = factor(taxon, ordered = TRUE,
                        levels = c("none", "cold_opt", "warm_opt")))

results %>% 
  ggplot(aes(x = timepoints, y = simulated.proxy, colour = taxon)) +
  geom_line() +
  # add the original climate signal
  geom_line(data = gisp2.ann, aes(x = age.yr.bp, y = temperature.rescaled,
                                  colour = "gisp2"), linetype = 1) +
  scale_y_reverse() +
  scale_color_manual(values = c("cold_opt" = "blue", "warm_opt" = "red",
                                "gisp2" = "Darkgrey")) +
  facet_wrap(~SAR, ncol = 1, labeller = label_both) +
  theme_bw()


