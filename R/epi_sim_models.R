## Script that holds the epidemiological models for simulating data
library(deSolve)



seasonal_sir <- function (t, x, parms) { 
  with(
    as.list(c(x, parms)),
    {
      beta <- beta0 * (1 + beta1*cos(2*pi*t))     
      dS <- mu * (1-S) - beta*S*I                
      dI <- beta * S * I - (mu+gamma)*I
      dR <- gamma * I - mu*R
      dx <- c(dS, dI, dR)                       
      list(dx)                               
    }
  )
}
