# nimble test run
library(nimble)
# Please run the code below

m <- nimbleModel(nimbleCode({x ~ dnorm(0, 1)}), inits = list(x = 0))
cm <- compileNimble(m)
MCMC <- buildMCMC(m)
CMCMC <- compileNimble(MCMC, project = m)
CMCMC$run(100)