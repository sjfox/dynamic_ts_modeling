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
seasonal.noisegaussian<-rnorm(length(seasonal), seasonal, sd=.01)
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

#Adding Noise at each model time step
tMax<-100; dt<-(1/365)
t<-seq(from=0, to=tMax, by=dt)
x.raw<-matrix(data=rep(0, length(t)), length(t), 3)
x.noise<-matrix(data=rep(0, length(t)), length(t), 3)
S0<-0.09;I0<-0.001;R0<-0.899
x.raw[1,]<-c(S0, I0, R0) #Raw data will be stored here
x.noise[1,]=c(S0, I0, R0) #Data with noise will be stored here
for (i in 1:length(t)){
  params <- c(mu=0,beta0=1*365,beta1=.5,gamma=365/13, d=365/180)   #parameters 
  xstart <- c(S=x.noise[i,1], I=x.noise[i, 2], R=x.noise[i,3])     #initial conditions based on noise-added output
  out <- as.data.frame(lsoda(y = xstart,
                             times = c(t(i), t(i+1)),
                             func = seasonal_sir,
                             parms = params))
  x.raw[i+1,1:3]<-as.numeric(out[2,c(2:4)])
  out.vec<-as.numeric(out[2, c(2:4)])
  out.noise<-rnorm(length(out.vec), out.vec, sd(seasonal)) #Using gaussian noise
  out.noise[which(out.noise>1)]<-1; out.noise[which(out.noise<0)]<-0 #Need to fix boundaries or the model won't run
  x.noise[i+1, 1:3]<-out.noise
}

#Predicting the data
require(rEDM); require(Rcpp); require(plyr)

#Try full time series with noise added
lib<-c(1, 365*10)
pred<-c(365*10+1, 36500)
ts<-seasonal.noisegaussian
simplex_output <- simplex(ts, lib, pred)

par(mar = c(4, 4, 1, 1), mgp = c(2.5, 1, 0))
plot(simplex_output$E, simplex_output$rho, type = "l", xlab = "Embedding Dimension (E)", 
     ylab = "Forecast Skill (rho)")
simplex_output <- simplex(ts, lib, pred, E = 8, tp = 1:10)
par(mar = c(4, 4, 1, 1))
plot(simplex_output$tp, simplex_output$rho, type = "l", xlab = "Time to Prediction (tp)", 
     ylab = "Forecast Skill (rho)")

smap_output <- s_map(ts, lib, pred, E = 8)
par(mar = c(4, 4, 1, 1), mgp = c(2.5, 1, 0))
plot(smap_output$theta, smap_output$rho, type = "l", xlab = "Nonlinearity (theta)", 
     ylab = "Forecast Skill (rho)")

data(block_3sp)
head(block_3sp)

lib <- c(1, NROW(block_3sp))
pred <- c(1, NROW(block_3sp))

block_lnlp_output <- block_lnlp(block_3sp, lib = lib, pred = pred, 
                                columns = c(1, 2, 3), target_column = 1, stats_only = FALSE, first_column_time = TRUE)
block_lnlp_output <- block_lnlp(block_3sp, lib = lib, pred = pred, columns = c("x_t", 
                                                                               "x_t-1", "y_t"), target_column = "x_t", stats_only = FALSE, first_column_time = TRUE)

observed <- block_lnlp_output[[1]]$model_output$obs
predicted <- block_lnlp_output[[1]]$model_output$pred

par(mar = c(4, 4, 1, 1), pty = "s")
plot_range <- range(c(observed, predicted), na.rm = TRUE)
plot(observed, predicted, xlim = plot_range, ylim = plot_range, xlab = "Observed", 
     ylab = "Predicted")
abline(a = 0, b = 1, lty = 2, col = "blue")