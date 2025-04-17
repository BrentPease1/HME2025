library(unmarked)
library(nimble)
library(here)
library(coda)
library(MCMCvis)


# AHM VOL 1: 6.4 A slightly more complex N-mixture model with covariates
# ------------------------------------------------------------------------



# Choose sample sizes and prepare obs. data array y
set.seed(1)                   # So we all get same data set
M <- 100                      # Number of sites
J <- 3                        # Number of repeated abundance measurements
C <- matrix(NA, nrow = M, ncol = J) # to contain the observed data

# Create a covariate called vegHt
vegHt <- sort(runif(M, -1, 1)) # sort for graphical convenience

# Choose parameter values for abundance model and compute lambda
beta0 <- 0                    # Log-scale intercept
beta1 <- 2                    # Log-scale slope for vegHt
beta2 <- 0.69
lambda <- exp(beta0 + beta1 * vegHt + beta2 * vegHt^2) # Expected abundance
plot(vegHt, lambda, type = "l", lwd = 3)  # Expected abundance

# Draw local abundance and look at data so far
N <- rpois(M, lambda)
points(vegHt, N)              # Add realized abundance to plot
table(N)

# Plot the true system state
par(mfrow = c(1, 2), mar = c(5,5,2,2), cex.axis = 1.5, cex.lab = 1.5)
plot(vegHt, N, xlab="Vegetation height", ylab="True abundance (N)", frame = F, cex = 1.5)
lines(seq(-1,1,,100), exp(beta0 + beta1* seq(-1,1,,100)), lwd=3, col = "red")


# Create a covariate called wind
wind <- array(runif(M * J, -1, 1), dim = c(M, J))

# Choose parameter values for measurement error model and compute detectability
alpha0 <- -2                        # Logit-scale intercept
alpha1 <- -3                        # Logit-scale slope for wind
p <- plogis(alpha0 + alpha1 * wind) # Detection probability
#plot(p ~ wind, ylim = c(0,1))       # Look at relationship

# Take J = 3 abundance measurements at each site
for(j in 1:J) {
  C[,j] <- rbinom(M, N, p[,j])
}

# Plot observed data and effect of wind on det. probability (Fig. 6???2, middle)
plot(wind, C/max(C), xlab="Wind", ylab="Scaled counts: C/max(C)", frame = F, cex = 1.5)
lines(seq(-1,1,,100), plogis(alpha0 + alpha1*seq(-1,1,,100)), lwd=3, col="red")


# Expected (lambda) and realized abundance (N) and measurements (C)
cbind(lambda=round(lambda,2), N=N, C1=C[,1], C2=C[,2], C3=C[,3])


# Create factors
time <- matrix(rep(as.character(1:J), M), ncol = J, byrow = TRUE)
hab <- c(rep("A", 33), rep("B", 33), rep("C", 34))  # assumes M = 100


# Load unmarked, format data in unmarked data frame and summarize
umf <- unmarkedFramePCount(
  y=C,                                            # Counts matrix
  siteCovs= data.frame(vegHt = vegHt), # Site covariates
  obsCovs = list(wind = wind))       # Observation covs
summary(umf)


# Fit model and extract estimates
# linear model for p follows first tilde, then comes linear model for lambda
# K is the upper summation limit for the summation over the random effects in the integrated
# likelihood (Royle, 2004b, p. 110). In unmarked, the default choice of K is 
# the maximum observed count plus 100.
summary(fm.Nmix1 <- pcount(~wind ~vegHt, data=umf, control=list(trace=T, REPORT=1)))


# Specify model in BUGS language
nmix2 <- nimbleCode({
  # Priors
    alpha0 ~ dunif(-10, 10) # Detection intercept
    alpha1 ~ dunif(-10, 10) # Detection slope
    beta0 ~ dunif(-10, 10)  # Abundance intercept
    beta1 ~ dunif(-10, 10)  # Abundance slope
    
  # Likelihood
  # Ecological model for true abundance
  for (i in 1:M){
    N[i] ~ dpois(lambda[i])
    log(lambda[i]) <- beta0 + beta1 * vegHt[i]

    # Some intermediate derived quantities
    critical[i] <- step(2-N[i])# yields 1 whenever N is 2 or less
    z[i] <- step(N[i]-0.5)     # Indicator for occupied site
    
    # Observation model for replicated counts
    for (j in 1:J){
      C[i,j] ~ dbin(p[i,j], N[i])
      logit(p[i,j]) <- alpha0 + alpha1 * wind[i,j]
    }
  }
  
  # Derived quantities
  Nocc <- sum(z[1:M])         # Number of occupied sites among sample of M
  Ntotal <- sum(N[1:M])       # Total population size at M sites combined

  N.critical <- sum(critical[1:M]) # Number of populations with critical size
  meta.pop.risk <- N.critical > 74 # proportion which N.critical >= 75
  
})


# Initial values
Nst <- apply(C,1,max)

nimInits <- list(N = Nst,
                 alpha0 = rnorm(1), #runif(1, -10, 10) #runif(1, -1, 1)
                 alpha1 = rnorm(1), 
                 beta0 = rnorm(1), 
                 beta1 = rnorm(1))


# Parameters monitored
keepers <- c("alpha0", "alpha1", "beta0",
            "beta1", "Nocc", "Ntotal",
            "N.critical", "meta.pop.risk") 


nimData <- list(C = C)
nimConsts <- list(M = nrow(C),
                  J = ncol(C),
                  wind = wind,
                  vegHt = vegHt) 



nmix2nim <- nimbleMCMC(code = nmix2,
                       data = nimData,
                       constants = nimConsts,
                       monitors = keepers,
                       inits = nimInits,
                       niter = 12000,
                       nburnin = 2000,
                       thin = 1,
                       nchains = 3) 

# look at output
MCMCtrace(object = nmix2nim,
          pdf = FALSE, # no export to PDF
          ind = TRUE, # separate density lines per chain
          params = keepers)

MCMCsummary(object = nmix2nim, round = 2, params = keepers)

dev.off()
MCMCplot(object = nmix2nim, 
         params = keepers)

# Example of a crazy derived variable in a Bayesian analysis:
# the posterior distribution of the number of populations
# that meet some hypothetical extinction threshold (here, two or fewer individuals), 
# and the imaginary critical number (red line, 75) of such populations 
# at which the metapopulation is going extinct. The metapopulation extinction risk 
# is the relative mass to the right of the red line and amounts to about 82%.

ncrit <- MCMCvis::MCMCchains(nmix2nim, params = 'N.critical')
plot(table(ncrit), xlab="Number of populations with critical size", ylab="Frequency", frame = F)
abline(v = 74.5, col = "red", lwd = 3)
MCMCvis::MCMCpstr(nmix2nim, params = 'meta.pop.risk')
