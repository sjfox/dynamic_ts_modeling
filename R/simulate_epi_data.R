## Script that actually simulates and saves the epi data
library(tidyverse)
library(cowplot)
source("R/epi_sim_models.R")



times <- seq(0,100,by=1/120)                     
params <- c(mu=1/50,beta0=1000,beta1=0.4,gamma=365/13)   #parameters
xstart <- c(S=0.06,I=0.001,R=0.939)              #initial conditions
out <- as.data.frame(lsoda(y = xstart,
                           times = times,
                           func = seasonal_sir,
                           parms = params))

out %>% gather(key, value, S:R) %>%
  ggplot(aes(time, value, color = key)) + geom_line()
