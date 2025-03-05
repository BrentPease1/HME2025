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
                 y.new = rbinom(n = nimConsts$nObs, 1, 0.5))
# monitors
keepers <- c('B0', 'B1', 'fit', 'fit.new')

# MCMC Settings
nc = 3
nb = 200
ni = 1000 + nb
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
         params = c('B0', 'B1'))
