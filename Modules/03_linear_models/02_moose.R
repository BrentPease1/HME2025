# Logistic Regression
library(nimble)
library(abd)
library(ggplot2)
library(MCMCvis)
library(cowplot)
library(WVPlots)
library(here)

# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
# make up a linear predictor
b0 = -2
b1 = 1
x <- runif(10,-1,1)

lp <- b0 + b1*x # these are our log odds

odds <- exp(lp) #odds

p <- odds / (1 + odds)



out <- data.frame(log_odds = lp,
                  odds = odds,
                  p = p,
                  back_to_log_odds = qlogis(p),# probability to log odds
                  back_to_prob = plogis(lp),# log odds to probability
                  also_probs = exp(lp) / (1+exp(lp)),  # long handed inverse-logit transformation
                  x = x)
print(out)
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
# MOOSE EXAMPLE
moose <- read.table(here('Data/moose.txt'))

str(moose)
head(moose)

# visualize relationship
ggplot(moose, aes(voc,observed))+
  geom_point(position = position_jitter(w = 2, h = 0.05), size=3) +
  geom_smooth(colour="red") + xlab("Visual Obstruction") +
  ylab("Detection = 1") + 
  theme_bw()



m2<- nimbleCode({
  
  # priors
  B0 ~ dnorm(0, sd = 10)
  B1 ~ dnorm(0, sd = 10)
  
  
  # likelihood
  for(i in 1:nObs){
    y[i] ~ dbern(p[i])
    logit(p[i]) <- B0 + B1*voc[i]
    
    # Fit assessments
    # Computation of fit statistic, Pearson residuals (for Bayesian p-value)
    # pearson residuals are good here because in addition to detecting lack of fit
    # they can signal overdisperson (variance larger than expected), suggesting alternative model
    pearson[i] <- (y[i]-p[i]) / sqrt(p[i]*(1-p[i]))
    y.new[i]~ dbern(p[i])
    pearson.new[i] <- (y.new[i]-p[i]) / sqrt(p[i]*(1-p[i]))
    
    # squared residuals
    D[i] <- pow(pearson[i],2)
    D.new[i]<- pow(pearson.new[i],2)
  }
  
  # Add up discrepancy measures
  fit <- sum(D[1:nObs])
  fit.new <- sum(D.new[1:nObs])
}
)

nimData <- list(y = moose$observed)
nimConsts <- list(nObs = nrow(moose),
                  voc = moose$voc)

nimInits <- list(B0 = rnorm(1,0,10),
                 B1 = rnorm(1,0,10),
                 y.new = rep(0, nimConsts$nObs))
# monitors
keepers <- c('B0', 'B1', 'fit', 'fit.new')

# MCMC Settings
nc = 3
nb = 2000
ni = 10000 + nb
nt = 1

# get posteriors
m1mcmc <- nimbleMCMC(code = m2,
                        constants = nimConsts,
                        data = nimData,
                        inits = nimInits,
                        monitors = keepers,
                        niter = ni,
                        nburnin = nb,
                        thin = nt,
                        nchains = nc,
                        summary = F,
                        WAIC =T)


# look at output
MCMCtrace(object = m1mcmc$samples,
          pdf = FALSE, # no export to PDF
          ind = TRUE, # separate density lines per chain
          params = c('B0', 'B1'))
MCMCsummary(object = m1mcmc$samples, round = 2, params = c('B0', 'B1'))


MCMCplot(object = m1mcmc$samples, 
         params = c('B1'))


# look at fit statistics
fit.stats <- MCMCpstr(m1mcmc$samples, params = c("fit", "fit.new"), type = "chains")

T.extreme <- fit.stats$fit.new >= fit.stats$fit
(p.val <- mean(T.extreme)) #checks out

# 1 categorical ####
moose$year <- as.factor(moose$year)
table(moose$year)
levels(moose$year)
mod2 <- glm(observed ~ year, data = moose, family = binomial())
summary(mod2)

# reference coding (effects parameterization)
model.matrix(mod2)[c(115:117-36),]
moose[c(115:117-36), ]

ggeffects::ggeffect(mod2, 'year')
plot(ggeffects::ggeffect(mod2, 'year'))

# means parameterization
mod3 <- glm(observed ~ -1 + year, data = moose, family = binomial())
summary(mod3)
plot(ggeffects::ggeffect(mod3, 'year'))

# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
# Nimble effects parameterization ####

mod2_nim <- nimbleCode({
  
  # priors
  B0 ~ dnorm(mean = 0, sd = 10) # baseline group mean
  B1[1] <- 0 # 'corner constraints' for parameter identifiability
  B1[2] ~ dnorm(mean = 0, sd = 10) # effect for group 2 (year 2006)
  B1[3] ~ dnorm(mean = 0, sd = 10) # effect for group 3 (year 2007)
  
  # instead of writing out a bunch of these,
  # for(bb in 2:3){
  #   B1[bb] ~ dnorm(mean = 0, sd = 10)
  # }
  # B1[1] <- 0
  
  # likelihood
  for(i in 1:nObs){
    y[i] ~ dbern(p[i])
    logit(p[i]) <- B0 + B1[year[i]]
  }
  
})


nimData <- list(y = moose$observed)
nimConsts <- list(nObs = nrow(moose),
                  year = as.numeric(moose$year)) # consecutive integers!!

nimInits <- list(B0 = rnorm(1,0,10),
                 B1 = c(NA, rep(rnorm(1,0,10),2))) # important
# monitors
keepers <- c('B0', 'B1')

# MCMC Settings
nc = 3
nb = 2000
ni = 10000 + nb
nt = 1

# get posteriors
mod2mcmc <- nimbleMCMC(code = mod2_nim,
                     constants = nimConsts,
                     data = nimData,
                     inits = nimInits,
                     monitors = keepers,
                     niter = ni,
                     nburnin = nb,
                     thin = nt,
                     nchains = nc,
                     summary = F,
                     WAIC =F)

# look at output
MCMCtrace(object = mod2mcmc,
          pdf = FALSE, # no export to PDF
          ind = TRUE, # separate density lines per chain
          params = keepers)
MCMCsummary(object = mod2mcmc, round = 2, params = keepers)


MCMCplot(object = mod2mcmc, 
         params = keepers)

mod2_nim <- nimbleCode({
  
  # priors
  B0 ~ dnorm(mean = 0, sd = 10) # baseline group mean
  B1[1] <- 0 # 'corner constraints' for parameter identifiability
  B1[2] ~ dnorm(mean = 0, sd = 10) # effect for group 2 (year 2006)
  B1[3] ~ dnorm(mean = 0, sd = 10) # effect for group 3 (year 2007)
  
  
  # likelihood
  for(i in 1:nObs){
    y[i] ~ dbern(p[i])
    logit(p[i]) <- B0 + B1[year[i]]
  }
  
  p1 <- ilogit(B0)             # Probability for reference group
  p2 <- ilogit(B0 + B1[2])  # Probability for group 2
  p3 <- ilogit(B0 + B1[3])  # Probability for group 3
  
})

keepers <- c('p1', 'p2', 'p3')

mod2mcmc <- nimbleMCMC(code = mod2_nim,
                       constants = nimConsts,
                       data = nimData,
                       inits = nimInits,
                       monitors = keepers,
                       niter = ni,
                       nburnin = nb,
                       thin = nt,
                       nchains = nc,
                       summary = F,
                       WAIC =F)

MCMCplot(object = mod2mcmc, 
         params = keepers, 
         horiz = F, 
         labels = c('2005', '2006', '2007'), 
         ylab = 'Prob. of Detection')


# Nimble MEANS parameterization ####
mod3_nim <- nimbleCode({
  
  # priors
  for(bb in 1:3){
    B0[bb] ~ dnorm(mean = 0, sd = 10)
  }

  
  # likelihood
  for(i in 1:nObs){
    y[i] ~ dbern(p[i])
    logit(p[i]) <- B0[year[i]]
  }
  
  p1 <- ilogit(B0[1])             # Probability for reference group
  p2 <- ilogit(B0[2])  # Probability for group 2
  p3 <- ilogit(B0[3])  # Probability for group 3
  
})

keepers <- c('B0', 'p1', 'p2', 'p3')
nimInits <- list(B0 = rnorm(3,0,10)) # important
mod3mcmc <- nimbleMCMC(code = mod3_nim,
                       constants = nimConsts,
                       data = nimData,
                       inits = nimInits,
                       monitors = keepers,
                       niter = ni,
                       nburnin = nb,
                       thin = nt,
                       nchains = nc,
                       summary = F,
                       WAIC =F)
MCMCsummary(object = mod3mcmc, round = 2, params = keepers)

