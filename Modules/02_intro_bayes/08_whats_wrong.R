library(nimble)
peregrines<- c(616, 653, 658, 608, 575, 621, 583, 602, 581, 604, 584, 604)



model <- nimbleCode({

  ## Priors ##
  pop.mean ~ dnorm(0, sd = 100) # still flat 'uninformative', but draws from a random normal
  prec ~ dgamma(0.1, 0.1)
  pop.sd <- sqrt(1/prec)
  

  # likelihood
  for(i in 1:nObs){
    peregrines[i] ~ dnorm(pop.mean, sd = pop.sd) 
  }
  
 
})


inits <- list(pop.mean = rnorm(n = 1, 100, 10), 
          prec = rgamma(n = 1, 0.1,0.1))


bird_data <- list(bird = peregrines)

bird_constants <- list(nObs = length(peregrines))

keepers <- c('pop.mean', 'pop.sd')


# MCMC settings
nc = 3
nb = 1000
ni = nb + 2000
nt = 1


bird_samples <- nimbleMCMC(
  code = bird_model, 
  constants = bird_constants,
  data = bird_data,
  inits = inits,
  monitors = keepers,
  niter = ni,
  nburnin = nb,
  thin = nt,
  nchains = nc,
  summary = T)


bird_samples$summary$all.chains