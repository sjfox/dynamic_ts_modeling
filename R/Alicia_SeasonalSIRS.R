library(tidyverse)
library(cowplot)
library(deSolve)
source("R/epi_sim_models.R")

seasonal_sir <- function (t, x, parms) { 
  with(
    as.list(c(x, parms)),
    {
      beta <- beta0 * (1 + beta1*(cos(6*t+0.05)-0.6))     
      dS <- mu * (1-S) - beta*S*I+d*R                
      dI <- beta * S * I - (mu+gamma)*I
      dR <- gamma * I - mu*R-d*R
      dx <- c(dS, dI, dR)                       
      list(dx)                               
    }
  )
}

times <- seq(0,100,by=1/365)                     
params <- c(mu=0,beta0=1*365,beta1=.5,gamma=365/13, d=365/180)   #parameters 
xstart <- c(S=0.09,I=0.001,R=0.899)              #initial conditions
out <- as.data.frame(lsoda(y = xstart,
                           times = times,
                           func = seasonal_sir,
                           parms = params))

out %>% gather(key, value, S:R) %>%
  ggplot(aes(time, value, color = key)) + geom_line()

####Adding Noise#####
plot(out$I[1:3650], type='l')
seasonal<-out$I
#Option 1: Gaussian noise (need to rescale values below 0)
seasonal.noisegaussian<-rnorm(length(seasonal), seasonal, sd=sd(seasonal))
#Option 2: Poisson noise (have to convert the values to the numeric scale before adding the noise)
seasonal2<-seasonal*100
seasonal.noisepoisson<-rpois(length(seasonal2), seasonal2)
#Option 3: transform data based on the logit scale, add noise and then re-scale
#This method leads to underdispersion and does not look reasonable.  
logit=function(x){log(x/(1-x))}
noise2<-1/(1+exp(-rnorm(length(seasonal),mean=logit(seasonal),sd=sd(seasonal))))

#Comparing the methods
par(mfrow=c(1,3))
plot(seasonal[1:3650], type='l')
plot(seasonal.noisepoisson[1:3650], type='l')
plot(seasonal.noisegaussian[1:3650], type='l')

#Reset negative values to 0
par(mfrow=c(1,1))
noise.gaussian2<-seasonal.noisegaussian; noise.gaussian2[which(noise.gaussian2<0)]<-0
plot(noise.gaussian2[1:3650], type='l')