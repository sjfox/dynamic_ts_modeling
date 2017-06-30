## Script that holds the epidemiological models for simulating data
library(deSolve)



seasonal_sir <- function (t, x, parms) { 
  ## Seasonal SIR model with closed system (same births and deaths)
  ## Parameters are as follows:
        ## mu - birth rate/death rate
        ## beta0 - baseline transmission rate
        ## beta1 - cosine changing transmission rate
        ## gamma - recovery rate
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

bd_sir <- function (t, x, parms) { 
  ## SIR model with closed system (same births and deaths)
  ## Parameters are as follows:
    ## mu - birth rate/death rate
    ## beta - transmission rate
    ## gamma - recovery rate
  with(
    as.list(c(x, parms)),
    {
      dS <- mu * (1-S) - beta*S*I                
      dI <- beta * S * I - (mu+gamma)*I
      dR <- gamma * I - mu*R
      dx <- c(dS, dI, dR)                       
      list(dx)                               
    }
  )
}

bd_sir_gill_step <- function (x, parms) { 

  with(
    as.list(parms), 
    {
      N <- S + I + R
      total_rate <- mu*N + beta*S*I/N + mu*S + mu*I + gamma * I + mu*R # Calculates total rate of all possible events
      tau <- rexp(n=1, rate=total_rate)                     # How much time passes until next event
      
      U <- runif(1)       # uniform random draw
      
      new_states <- c(S,I,R-1) # death of recovered (Default state)
      
      if (U <= (mu*N + beta*S*I/N + mu*S + gamma*I + mu*I)/total_rate) new_states<-c(S,I-1,R) #death of infected
      if (U <= (mu*N + beta*S*I/N + mu*S + gamma*I)/total_rate) new_states<-c(S,I-1,R+1)  #recovery of infected
      if (U <= (mu*N + beta*S*I/N + mu*S)/total_rate) new_states<-c(S-1,I,R)    #death of a susceptible
      if (U <= (mu*N + beta*S*I/N)/total_rate) new_states<-c(S-1,I+1,R)       #transmission event
      if (U <= (mu*N/total_rate)) new_states<-c(S+1, I, R)                  #birth of susceptible
      c(tau, new_states) #return resulting states and time step
    }
  )
}

bd_sir_gillespie <- function (t, x, parms) {  
  ## SIR stochastic model with closed system (same births and deaths)
  ## Parameters are as follows:
    ## mu - birth rate/death rate
    ## beta - transmission rate
    ## gamma - recovery rate
  
    output <- array(dim = c(n_steps + 1, (length(x) + 1)))         #set up array to store results
    colnames(output) <- c("time","S","I","R") #name variables
    output[1,] <- x                           #first record of output is initial condition
    day <- 0
    for (k in 0:(t+1)) {                      #iterate for nstep steps
      states <- bd_sir_gill_step(x, parms) 
      if(as.integer(states[1]) > day) {
        output[k + 1,] <- states
        day <- as.integer(states[1])
      }
      
      x <- output[k+1, -1] # Next states are new states without the time column
    }
    
    output                                    
}
  
