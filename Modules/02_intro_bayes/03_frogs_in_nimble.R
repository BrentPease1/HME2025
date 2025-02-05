library(nimble)
frog_code01 <- nimble::nimbleCode({
  
  ## ## ## ## ## ## ## ## 
  #  Prior Probability  # 
  ## ## ## ## ## ## ## ##
  
  # prior probability of presence given what we know about frog
  prior.knowledge <- 0.5        
  
  # probability of detection is relatively high but depends on presence/absence 
  # incorporating prior knowledge of species dectection here is fine
  detect.prob <- 0.8 * presence 
  
  ## ## ## ## ## ## ## ## 
  #     Likelihood      # 
  ## ## ## ## ## ## ## ##
  
  # actual presence drawn from a Bernoulli distribution
  presence ~ dbern(prior.knowledge) 
  
  # actual detection occurs with random variation that follows a Bernoulli distribution
  detected ~ dbern(detect.prob)

  
}
)

## frog_code01 <- nimbleCode({
## 
##   ## ## ## ## ## ## ## ##
##   #  Prior Probability  #
##   ## ## ## ## ## ## ## ##
## 
##   # prior probability of presence given what we know about frog
##   prior.knowledge <- 0.5
## 
##   # probability of detection is relatively high but depends on presence/absence
##   detect.prob <- 0.8
## 
##   ## ## ## ## ## ## ## ##
##   #     Likelihood      #
##   ## ## ## ## ## ## ## ##
## 
##   # actual presence drawn from a Bernoulli distribution
##   presence ~ dbern(prior.knowledge)
## 
##   # actual detection occurs with random variation that follows a Bernoulli distribution
##   detected ~ dbern(detect.prob*presence) #conditional on presence of species
## 
## 
##   }
## )

# specify data
# must match the name in the model file
frog_data <- list(detected = 0)

# specify initial values
initsList <- list(presence = 1) # you can explicitly state the value
initsList <- list(presence = rbinom(1,1,0.5)) # but this is best practice
                                              # with the idea of not influencing 
                                              # the result
# tell nimble what we would like to monitor
# that is, what parameters do we want a posterior distribution for?
# in this example, we really only have 1 thing we care about: presence of frog
# but we can get in the routine of doing it
keepers <- c('presence')

# package it all up
frog_samples01 <- nimbleMCMC(code = frog_code01,
                             data = frog_data,
                             inits = initsList,
                             niter = 10000,
                             nchain = 1,
                             monitors = keepers,
                             summary = T)

# what did we just create?
str(frog_samples01) 
# TWO THINGS
# 10,000 numbers in a matrix with a column header of "presence"
# A summary of our posterior for everything in keepers
frog_samples01$summary


# with just one chain and a quite simple model, 
# our "model diagnostics" are a bit limited, 
# but we'll do a full inspection once we have a bigger model

# Say it is a really great pond

frog_code02 <- nimbleCode({
  
  ## ## ## ## ## ## ## ## 
  #  Prior Probability  # 
  ## ## ## ## ## ## ## ##
  
  # prior probability of presence given what we know about the pond
  prior.knowledge <- 0.75        
  
  # probability of detection is relatively high but depends on presence/absence 
  # incorporating prior knowledge of species detection
  detect.prob <- 0.8 * presence 
  
  ## ## ## ## ## ## ## ## 
  #     Likelihood      # 
  ## ## ## ## ## ## ## ##
  
  # actual presence drawn from a Bernoulli distribution
  presence ~ dbern(prior.knowledge) 
  
  # actual detection occurs with random variation that follows a Bernoulli distribution
  detected ~ dbern(detect.prob)

  }
)

frog_samples02 <- nimbleMCMC(code = frog_code02,
                            data = frog_data,
                            niter = 10000)

summary(frog_samples02) # point estimate nearly identical to solution found analytically 
