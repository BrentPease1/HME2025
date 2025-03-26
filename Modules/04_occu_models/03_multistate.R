# mutlistate occupancy models
library(unmarked)
library(nimble)
library(here)
library(coda)
library(ggplot2)
library(MCMCvis)


# Simulating the simplest dataset for multi-state occu models
# we'll assume 3 states w/ breeding example

# Parameters for conditions (Omega)
psi <- 0.8                # Expected proportion of occupied sites (probability of occupancy by focal species)
r <- 0.4                  # Exp. proportion of sites (among occupied) with breeding evidence


# Parameters for observation matrix (Theta)
p2 <- 0.5                 # Detection probability of site with single bird
p32 <- 0.2                # Classification probability of site with pair as single bird
p33 <- 0.6                # Classification probability of site with pair as pair

# Assemble initial state vector and the two matrices
# Populate initial state probability vector (Omega)
Omega <- c(1-psi, psi*(1-r), psi*r)

# Populate observation probability matrix (Theta)
Theta <- matrix(
  c(1, 0, 0,
    1-p2, p2, 0,
    1-p32- p33, p32, p33), ncol = 3, byrow = TRUE)


# Pick sample sizes (note use of names instead of letters)
nsites <- 100                 # denoted "M" above
nsurveys <- 4                 # ... "J" ...

# Function to get the category index
# rmulti will generate a vector of states, and which need to extract with slot had the 1
get1 <- function(x) which(x == 1)

# Set seed for reproducibility
set.seed(1)

# Draw initial states using Omega
z <- rmultinom(nsites, 1, Omega) # look at this to see what the get1 will do
print(z)
z <- apply(z, 2, get1)

# Draw observations using Theta
for(i in 1:nsites){
  y[i, ] <- apply(rmultinom(nsurveys, 1, Theta[z[i], ]), 2, get1)
}


# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --  
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --  

# OKAY, we have observations (y) 
# and we know truth z (and associated probabilities - let's try to recover those)

# if you can simulate the data, you can write the BUGS model
# Define the NIMBLE model
ms1 <- nimbleCode({
  
  # Priors
  psi ~ dunif(0, 1)
  r ~ dunif(0, 1)
  p2 ~ dunif(0, 1)
  
  # Multinomial logit link for observation model for state 3 (= pair)
  lp32 ~ dnorm(0, 0.001)
  lp33 ~ dnorm(0, 0.001)
  p32 <- exp(lp32) / (1 + exp(lp32) + exp(lp33))
  p33 <- exp(lp33) / (1 + exp(lp32) + exp(lp33))
  p31 <- 1-p32-p33                     # Nondetection prob for pairs by difference
  
  # Initial state probabilities (Omega)
  Omega[1] <- 1 - psi                 # Non-occupied
  Omega[2] <- psi * (1 - r)           # Occupied (single)
  Omega[3] <- psi * r                 # Occupied (pair)
  

  
  # Observation probabilities
  Theta[1,1] <- 1      # True state 1 always observed as 1
  Theta[1,2] <- 0
  Theta[1,3] <- 0
  Theta[2,1] <- 1 - p2 # True state 2 observed as 1 (missed) or 2 (detected)
  Theta[2,2] <- p2
  Theta[2,3] <- 0
  Theta[3,1] <- p31    # True state 3 (pair) observed as 1, 2, or 3
  Theta[3,2] <- p32
  Theta[3,3] <- p33
  
  # Latent state (true occupancy state)
  for (i in 1:nsites) {
    z[i] ~ dcat(Omega[1:3]) # True state distribution
  }
  
  # Observation model
  for (i in 1:nsites) {
    for (j in 1:nsurveys) {
      y[i, j] ~ dcat(Theta[z[i], 1:3]) # Observation model
    }
  }
  
  # Derived quantities (number of occupied sites)
  for (i in 1:nsites) {
    occ1[i] <- equals(z[i], 1)
    occ2[i] <- equals(z[i], 2)
    occ3[i] <- equals(z[i], 3)
  }
  n.occ[1] <- sum(occ1[1:nsites]) # Sites in state 1
  n.occ[2] <- sum(occ2[1:nsites]) # Sites in state 2
  n.occ[3] <- sum(occ3[1:nsites]) # Sites in state 3
})


# Initial values
zst <- apply(y, 1, max)       # Avoid data/model/inits conflict
inits <- list(z = zst,
              psi = runif(1),
              r = runif(1),
              p2 = runif(1),
              lp32 = rnorm(1, mean = 0, sd = 10),
              lp33 = rnorm(1, mean = 0, sd = 10)
              )

nimData <- list(y = y)
nimConsts <- list(nsites = nsites,
                  nsurveys = nsurveys) 

# Parameters monitored
keepers <- c("psi", "r", "p2", "p31", "p32", "p33", "Omega", "Theta", "n.occ")

ms1 <- nimbleMCMC(code = ms1,
                 data = nimData,
                 constants = nimConsts,
                 monitors = keepers,
                 inits = inits,
                 niter = 25000,
                 nburnin = 5000,
                 thin = 20,
                 nchains = 3)


# look at output
MCMCtrace(object = ms1,
          pdf = FALSE, # no export to PDF
          ind = TRUE, # separate density lines per chain
          params = keepers)

MCMCsummary(object = ms1, round = 2, params = keepers)


MCMCplot(object = ms1, 
         params = keepers)

