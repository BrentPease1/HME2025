#04_multiscale
library(nimble)
library(here)
library(coda)
library(ggplot2)
library(MCMCvis)
library(AHMbook)

# All covariate effects, but no random variability (model 3)
data <- sim3Occ(nunit = 100, 
                nsubunit = 5, 
                nrep = 3,
                mean.psi = 0.8,
                beta.Xpsi = 1, 
                sd.logit.psi = 0,
                mean.theta = 0.6, 
                theta.time.range = c(-1, 1), 
                beta.Xtheta = 1, 
                sd.logit.theta = 0, 
                mean.p = 0.4, 
                p.time.range = c(-2,2), 
                beta.Xp = -1, 
                sd.logit.p = 0)
str(data)

# Look at data
str(data$z)           # True quadrat (pond) occurrence state
str(data$a)           # True subquadrat (water sample) occurrence state
str(data$y)           # Observed data


# specify model in nimble
multiscale1 <- nimbleCode({
  
  # Priors and model for parameters
  int.psi ~ dunif(0,1)  # Intercept of occupancy probability
  for(t in 1:n.samples){
    int.theta[t] ~ dunif(0,1)  # Intercepts availability probability
  }
  for(t in 1:n.surveys){
    int.p[t] ~ dunif(0,1)  # Intercepts detection probability 
  }
  
  # regression coefficients / effects of covariates
  beta.lpsi ~ dnorm(0, sd = 10)  # Slopes of three covariates
  beta.ltheta ~ dnorm(0, sd = 10)
  beta.lp ~ dnorm(0, sd = 10)
  
  # Likelihood (basic model structure)
  for (i in 1:n.pond){
    # Occurrence in pond i
    z[i] ~ dbern(psi[i])
    logit(psi[i]) <- logit(int.psi) + beta.lpsi * covA[i]
    
    for (j in 1:n.samples){
      # Occurrence in sample j
      a[i,j] ~ dbern(mu.a[i,j])
      mu.a[i,j] <- z[i] * theta[i,j]
      logit(theta[i,j]) <- logit(int.theta[j]) + beta.ltheta * covB[i,j]
      
      for (k in 1:n.surveys){
        # PCR detection error process in sample k
        y[i,j,k] ~ dbern(mu.y[i,j,k])
        mu.y[i,j,k] <- a[i,j] * p[i,j,k]
        logit(p[i,j,k]) <- logit(int.p[k]) + beta.lp * covC[i,j,k]
      }
    }
    tmp[i] <- step(sum(a[i,1:n.samples]) - 0.1)
  }
  
  # Derived quantities
  
  sum.z <- sum(z[1:n.pond])   # Total number of occupied ponds in sample
  sum.a <- sum(tmp[1:n.pond]) # Total number of ponds with presence in <=1 of the 5 samples
  mean.int.theta <- mean(int.theta[1:n.samples]) # average prob of occu in a given water sample
})


# Bundle data and constants
y <- data$y
nimData <- list(y = y)
nimConsts <- list(n.pond = dim(y)[1],
                  n.samples = dim(y)[2],
                  n.surveys = dim(y)[3], 
                  covA = data$covA, 
                  covB = data$covB, 
                  covC = data$covC) 

zst <- apply(y, 1, max)       # Avoid data/model/inits conflict
ast <- apply(y, c(1,2), max)
inits <- list(z = zst,
              a = ast, 
              int.psi = runif(1),
              int.theta = runif(n = nimConsts$n.samples),
              int.p = runif(n = nimConsts$n.surveys),
              beta.lpsi = rnorm(1, mean = 0, sd = 10),
              beta.ltheta = rnorm(1, mean = 0, sd = 10),
              beta.lp = rnorm(1, mean = 0, sd = 10))



# Parameters monitored
keepers <- c("int.p", "int.theta", "int.psi", "beta.lp",
             "beta.ltheta", "beta.lpsi", "sum.z", "sum.a", "mean.int.theta")

mscale1 <- nimbleMCMC(code = multiscale1,
                  data = nimData,
                  constants = nimConsts,
                  monitors = keepers,
                  inits = inits,
                  niter = 25000,
                  nburnin = 5000,
                  thin = 20,
                  nchains = 3)


# look at output
MCMCtrace(object = mscale1,
          pdf = FALSE, # no export to PDF
          ind = TRUE, # separate density lines per chain
          params = keepers)

MCMCsummary(object = mscale1, round = 2, params = keepers)


MCMCplot(object = mscale1, 
         params = keepers)