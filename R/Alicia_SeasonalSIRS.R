library(tidyverse)
library(cowplot)
library(deSolve)
source("R/epi_sim_models.R")

seasonal_sir <- function (t, x, parms) { 
  with(
    as.list(c(x, parms)),
    {
      beta <- beta0 * (1 + beta1*cos(6*t+0.05))     
      dS <- mu * (1-S) - beta*S*I+d*R                
      dI <- beta * S * I - (mu+gamma)*I
      dR <- gamma * I - mu*R-d*R
      dx <- c(dS, dI, dR)                       
      list(dx)                               
    }
  )
}

times <- seq(0,20,by=1/365)                     
params <- c(mu=0,beta0=6*365,beta1=.8,gamma=365/13, d=365/180)   #parameters 
xstart <- c(S=0.09,I=0.001,R=0.899)              #initial conditions
out <- as.data.frame(lsoda(y = xstart,
                           times = times,
                           func = seasonal_sir,
                           parms = params))

out %>% gather(key, value, S:R) %>%
  ggplot(aes(time, value, color = key)) + geom_line()


#curve(5/365 * (1 + .8*cos(6*x)), from=0, to=2)
