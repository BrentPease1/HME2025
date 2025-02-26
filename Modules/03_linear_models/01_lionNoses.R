# Linear Regression Review
library(nimble)
library(abd)
library(ggplot2)
library(MCMCvis)
library(cowplot)
library(WVPlots)
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
# SIMPLE LINEAR REGRESSION - Lion Noses                                     ####
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data("LionNoses") #from abd library
#These data come from a paper by Whitman, Starfield, Quadling, & Packer (2004), 
# in which the authors address the impact of trophy hunting on lion population dynamics. 
# The authors note that removing male lions can lead to increases in infanticide, 
# but the authors’ simulation models suggest that removal of only older males 
# (e.g., greater than 6 years of age) could minimize these impacts.3

# How could a researcher (or hunter/guide) tell the age of a lion from afar, though?
# It turns out that it is possible to get a rough idea of how old a male lion is 
# from the amount of black pigmentation on its nose
str(LionNoses)
head(LionNoses)

# plot the data
ggplot(LionNoses, aes(proportion.black, age)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x)+ 
  xlab("Proportion Black") + ylab("Age") +
  theme_classic() + 
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))

## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
## FREQ
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
# fit model 
lm.noses <- lm(age ~ proportion.black, data = LionNoses)

# look at model output
summary(lm.noses)

# CHECK: WHAT DO REGRESSION COEFFICIENTS MEAN?



# Maybe easier to interpret unit change with percentages?
LionNoses$percentage.black <- LionNoses$proportion.black*100
lm.noses2<-lm(age ~ percentage.black, data=LionNoses)
summary(lm.noses2)


## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
## BAYES
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

m1 <- nimbleCode({
  
  # priors
  B0 ~ dnorm(mean = 0, sd = 10)
  B1 ~ dnorm(mean = 0, sd = 10)
  tau ~ dgamma(1,1)
  sig <- sqrt(1/tau)
  
  # likelihood
  for(i in 1:nObs){
    y[i] ~ dnorm(mean = mu[i], sd = sig)
    mu[i] <- B0 + B1*percentage.black[i]
  }
  
})

nimData <- list(y = LionNoses$age)
nimConsts <- list(nObs = nrow(LionNoses),
                  percentage.black = LionNoses$percentage.black)

nimInits <- list(B0 = rnorm(1,0,10),
                 B1 = rnorm(1,0,10),
                 tau = rgamma(1,1,1))
# monitors
keepers <- c('B0', 'B1', 'sig')

# MCMC Settings
nc = 3
nb = 200
ni = 2000 + nb
nt = 1

# get posteriors
nim.noses <- nimbleMCMC(code = m1,
                        constants = nimConsts,
                        data = nimData,
                        inits = nimInits,
                        monitors = keepers,
                        niter = ni,
                        nburnin = nb,
                        thin = nt,
                        nchains = nc,
                        summary = T)
# Don't be bayesic, check your traceplots
samples_mcmc <- coda::as.mcmc.list(lapply(nim.noses$samples, coda::mcmc))
par(mfrow=c(1,length(samples_mcmc)))
coda::traceplot(samples_mcmc)

# Check Rhat
coda::gelman.diag(samples_mcmc)

# could also use MCMCvis to get rhat, n.effective, and posterior
MCMCvis::MCMCsummary(samples_mcmc)

# MCMCPLOTS STYLE DENSITY PLOTS
# it is important to remember that we have many samples from the posterior for each parameter
str(samples_mcmc)
plot_mcmc_density(samples_mcmc)


## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
# HOW DO WE DO MODEL CHECKING?                                              ####
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

# In Freq, we were taught to review residual diagnostic plots for linear regression
# models. Let's do that now:

# model assessment
# plot(lm.noses) works but less gg-y
ggResidpanel::resid_panel(lm.noses2, plots = c("resid", "qq", "ls"), nrow = 1, smoother = TRUE)

# we can create the same plots from nimble, but it will take more work and a deeper
# understanding of the underlying calculations in the residual panel
# (go to chaulk board and show calculations)


m1 <- nimbleCode({
  
  # priors
  B0 ~ dnorm(mean = 0, sd = 10)
  B1 ~ dnorm(mean = 0, sd = 10)
  tau ~ dgamma(1,1)
  sig <- sqrt(1/tau)
  
  # likelihood
  for(i in 1:nObs){
    y[i] ~ dnorm(mean = mu[i], sd = sig)
    mu[i] <- B0 + B1*percentage.black[i]
  }
  
  # SSE
  for(j in 1:nObs){
    residual[j] <- y[j] - mu[j] #residuals for observed data
    sq.res[j] <- pow(residual[j], 2) #squared residuals for observed data
  }
  # MSE
  mse <- sum(sq.res[1:nObs]) / (nObs - 2) #formula for mse
  
  # manual calculation of residual standard error
  manual.sig <- sqrt(mse)
  
})

# monitors
keepers <- c('B0', 'B1', 'sig', "residual", "mu", "manual.sig")

# get posteriors
nim.noses <- nimbleMCMC(code = m1,
                        constants = nimConsts,
                        data = nimData,
                        inits = nimInits,
                        monitors = keepers,
                        niter = ni,
                        nburnin = nb,
                        thin = nt,
                        nchains = nc)

# MCMCvis is nice
MCMCtrace(object = nim.noses,
          pdf = FALSE, # no export to PDF
          ind = TRUE, # separate density lines per chain
          params = c('B0', 'B1', 'sig'))
MCMCsummary(object = nim.noses, round = 2, params = c('B0', 'B1', 'sig'))
MCMCplot(object = nim.noses, 
         params = c('B0', 'B1', 'sig'))


# take a look at freq output again
summary(lm.noses2) # "residual standard error: 1.669 on 30 df
MCMCsummary(object = nim.noses, round = 2, params = c('manual.sig'))

## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
# Extract some values from the posteriors for diagnostic plots
nim.resids <- MCMCpstr(nim.noses, params = 'residual', func = mean)
nim.fitted <- MCMCpstr(nim.noses, params = 'mu', func = mean)

nim.fit <- data.frame(resid = nim.resids$residual, fitted = nim.fitted$mu)
nim.fit$std.abs.resid <- sqrt(abs(nim.fit$resid/sd(nim.fit$resid)))

# residual plot
p1 <- ggplot(nim.fit, aes(fitted, resid)) + 
  geom_point() +
  geom_hline(yintercept = 0, col = 'blue') + 
  geom_smooth(se = FALSE,col = 'red') +
  theme_bw() + 
  labs(title = 'Residual Plot', y = 'Residuals', x = 'Predicted Values')
# q-q plot
p2 <- ggplot(nim.fit, aes(sample = resid)) + 
  stat_qq() + 
  stat_qq_line(col = 'blue') + 
  theme_bw() + 
  labs(x = 'Theoretical Quantiles', y = 'Sample Quantiles', title = 'Q-Q Plot')
# location scale plot
p3 <- ggplot(nim.fit, aes(fitted, std.abs.resid)) + 
  geom_point() +
  geom_smooth(se = FALSE,col = 'red') + 
  theme_bw() + 
  labs(title = 'Location-Scale Plot', x = 'fitted', y = "sqrt(|Standardized Residuals|)")
# bring it all together
cowplot::plot_grid(p1, p2, p3, nrow = 1)


# OKAY, but let's do another model check
# in addition to standard residual plots, a formal goodness-of-fit test can be helpful
# for evaluating the model.

# We were on our way to assessing model fit using a sum-of-sqaures type discrepancy
# A really helpful resource for understanding the following approach is in Kery 2010

# we have already calculated the squared residuals for our observed data
# we just need to compare the squared residuals for data generated from the model *given* mu and tau
# then compare the fit of our observed data to the generated/simulated data

# Bayes P-value
# proportion of simulated datasets that are as or more
# extreme than the observed data

m1 <- nimbleCode({
  
  # priors
  B0 ~ dnorm(mean = 0, sd = 10)
  B1 ~ dnorm(mean = 0, sd = 10)
  tau ~ dgamma(1,1)
  sig <- sqrt(1/tau)
  
  # likelihood
  for(i in 1:nObs){
    y[i] ~ dnorm(mean = mu[i], sd = sig)
    mu[i] <- B0 + B1*percentage.black[i]
  }
  
  # SSE
  for(j in 1:nObs){
    residual[j] <- y[j] - mu[j] #residuals for observed data
    sq.res[j] <- pow(residual[j], 2) #squared residuals for observed data
    
    # GENERATE replicate data from the model
    y.new[j] ~ dnorm(mu[j], sd = sig)
    sq.new[j] <- pow(y.new[j] - mu[j], 2)
  }
  
  
  fit.obs <- sum(sq.res[1:nObs]) # Sum of squared residuals for actual data set
  fit.new <- sum(sq.new[1:nObs]) # Sum of squared residuals for new data set
  
  
  # MSE
  mse <- sum(sq.res[1:nObs]) / (nObs - 2) #formula for mse
  
  # manual calculation of residual standard error
  manual.sig <- sqrt(mse)
  
})

# monitors
keepers <- c('B0', 'B1', 'sig', "residual", "mu", "manual.sig", "fit.obs",
             'fit.new')

# get posteriors
nim.noses <- nimbleMCMC(code = m1,
                        constants = nimConsts,
                        data = nimData,
                        inits = nimInits,
                        monitors = keepers,
                        niter = ni,
                        nburnin = nb,
                        thin = nt,
                        nchains = nc)


# get the fit statistics out of the model
fit.stats <- MCMCpstr(nim.noses, params = c("fit.obs", "fit.new"), type = "chains")

T.extreme <- fit.stats$fit.new >= fit.stats$fit.obs
(p.val <- mean(T.extreme))

# this is a large p-value, suggesting we do not have strong evidence that our data are 
# inconsistent with the assumed model. See Conn et al., 2018 for comments on whether
# bayesian p-values are reasonable for model fit assessment (spoiler: they are conservative)

# can also plot to visualize
fit.stats <- MCMCchains(nim.noses, params = c("fit.obs", "fit.new"))
fit.stats <- as.data.frame(fit.stats)
fit.stats$postdiff <- fit.stats$fit.new - fit.stats$fit.obs
WVPlots::ShadedDensity(frame = fit.stats,
                       xvar = "postdiff",
                       threshold = 0,
                       title = "Posterior distribution: SSE(sim data)-SSE(obs data)",
                       tail = "right")+
  annotate("text", x=85, y = 0.005,
           label="Better fit to observed data")+
  annotate("text", x=-40, y = 0.005,
           label="Better fit to simulated data")

## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
# MODEL PREDICTIONS                                                         ####
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

# lastly, we can easily get predictions from nimble (or any software, really) with
# a few additional lines of code
# WE just need to pass in the values that we want to make predictions
# this can be a sequence of numbers or specific values. 
# we use the regression coefficients for the predictions 

# make a sequence of numbers for the observed values in the dataset
pred.black <- seq(min(LionNoses$percentage.black), max(LionNoses$percentage.black), length.out = 100)

m1 <- nimbleCode({
  
  # priors
  B0 ~ dnorm(mean = 0, sd = 10)
  B1 ~ dnorm(mean = 0, sd = 10)
  tau ~ dgamma(1,1)
  sig <- sqrt(1/tau)
  
  # likelihood
  for(i in 1:nObs){
    y[i] ~ dnorm(mean = mu[i], sd = sig)
    mu[i] <- B0 + B1*percentage.black[i]
  }
  
  # SSE
  for(j in 1:nObs){
    residual[j] <- y[j] - mu[j] #residuals for observed data
    sq.res[j] <- pow(residual[j], 2) #squared residuals for observed data
    
    # GENERATE replicate data from the model
    y.new[j] ~ dnorm(mu[j], sd = sig)
    sq.new[j] <- pow(y.new[j] - mu[j], 2)
  }
  
  
  fit.obs <- sum(sq.res[1:nObs]) # Sum of squared residuals for actual data set
  fit.new <- sum(sq.new[1:nObs]) # Sum of squared residuals for new data set
  
  
  # MSE
  mse <- sum(sq.res[1:nObs]) / (nObs - 2) #formula for mse
  
  # manual calculation of residual standard error
  manual.sig <- sqrt(mse)
  
  # MAKE PREDICTIONS OF y
  for(k in 1:nPreds){
    y.pred[k] <- B0 + B1*pred.black[k]
  }
  
})

# monitors
keepers <- c('y.pred') # simplified for illustration

# data
nimData <- list(y = LionNoses$age)

# constants
nimConsts <- list(nObs = nrow(LionNoses),
                  nPreds = length(pred.black), #THIS IS NEW
                  percentage.black = LionNoses$percentage.black,
                  pred.black = pred.black) #THIS IS NEW
# inits
nimInits <- list(B0 = rnorm(1,0,10),
                 B1 = rnorm(1,0,10),
                 tau = rgamma(1,1,1),
                 y.new = rnorm(n = nimConsts$nObs, mean = mean(LionNoses$age), sd = sd(LionNoses$age)))

# MCMC Settings
nc = 3
nb = 200
ni = 2000 + nb
nt = 1
# get posteriors
nim.noses <- nimbleMCMC(code = m1,
                        constants = nimConsts,
                        data = nimData,
                        inits = nimInits,
                        monitors = keepers,
                        niter = ni,
                        nburnin = nb,
                        thin = nt,
                        nchains = nc)



# MCMCvis is nice
mcmc_summary <- MCMCsummary(object = nim.noses, round = 2, params = c('y.pred'))
MCMCplot(object = nim.noses, 
         params = c('y.pred'), horiz = FALSE, ylab = "Predicted Age")

# plot relationship

# pull out some values from mcmc_summary
mcmc_plot <- data.frame(mean.pred = mcmc_summary$mean, 
                        lci = mcmc_summary$`2.5%`, 
                        uci = mcmc_summary$`97.5%`, 
                        pred.black)

# pass to the gg
ggplot(mcmc_plot, aes(x = pred.black, y = mean.pred)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), fill = "grey70", alpha = 2) +
  geom_line() +ylab("Age") +
  geom_point(data = LionNoses, aes(percentage.black, age)) +
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))+
  labs(x = "Percent Black Pigmentation in Nose", y = "Predicted Age")

## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

# Stone age robustness analysis
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
# too long of a name
lion <- LionNoses
rm(LionNoses)

# Set up our priors for the robustness analysis
priors <- list(
  list(beta_prior_mean = 0, beta_prior_sd = 10),  # Weak prior
  list(beta_prior_mean = 0, beta_prior_sd = 1),   # Moderate prior
  list(beta_prior_mean = 1, beta_prior_sd = 0.5)  # Strong, positive prior
)

# specify response variable
nimData <- list(y = LionNoses$age)

# monitors
keepers <- c('B0', 'B1', 'sig')

# MCMC Settings
nc = 3
nb = 200
ni = 2000 + nb
nt = 1


# specify our basic number
# but generalize the priors on the regression coefficients
# we'll pass those in as constants
m1 <- nimbleCode({
  
  # priors
  B0 ~ dnorm(mean = prior_mean, sd = prior_sd)
  B1 ~ dnorm(mean = prior_mean, sd = prior_sd)
  tau ~ dgamma(1,1)
  sig <- sqrt(1/tau)
  
  # likelihood
  for(i in 1:nObs){
    y[i] ~ dnorm(mean = mu[i], sd = sig)
    mu[i] <- B0 + B1*percentage.black[i]
  }
  
})

#
holder <- list()

for(i in 1:length(priors)){
  nimConsts <- list(nObs = nrow(lion),
                    percentage.black = lion$percentage.black,
                    prior_mean = priors[[i]]$beta_prior_mean,
                    prior_sd = priors[[i]]$beta_prior_sd)
  
  nimInits <- list(B0 = rnorm(1,nimConsts$prior_mean,nimConsts$prior_sd),
                   B1 = rnorm(1,nimConsts$prior_mean,nimConsts$prior_sd),
                   tau = rgamma(1,1,1))
  
  # get posteriors
  holder[[i]] <- nimbleMCMC(code = m1,
                            constants = nimConsts,
                            data = nimData,
                            inits = nimInits,
                            monitors = keepers,
                            niter = ni,
                            nburnin = nb,
                            thin = nt,
                            nchains = nc)
}

mcmc_summary1 <- MCMCsummary(object = holder[[1]], round = 2, params = c('B0', 'B1', 'sig'))
mcmc_summary2 <- MCMCsummary(object =  holder[[2]], round = 2, params = c('B0', 'B1', 'sig'))
mcmc_summary3 <- MCMCsummary(object =  holder[[3]], round = 2, params = c('B0', 'B1', 'sig'))

# plot relationship

# pull out some values from mcmc_summary
mcmc_plot <- data.frame(B1_mean = c(mcmc_summary1['B1','mean'], 
                                    mcmc_summary2['B1','mean'],
                                    mcmc_summary3['B1','mean']),
                        B1_lci = c(mcmc_summary1['B1','2.5%'], 
                                   mcmc_summary2['B1','2.5%'],
                                   mcmc_summary3['B1','2.5%']),
                        B1_uci = c(mcmc_summary1['B1','97.5%'], 
                                   mcmc_summary2['B1','97.5%'],
                                   mcmc_summary3['B1','97.5%']),
                        Priors = c('Weak', 'Moderate', 'Highly'))

ggplot(mcmc_plot, aes(x = Priors, y = B1_mean)) +
  geom_point(size = 3) +  
  geom_errorbar(aes(ymin = B1_lci, ymax = B1_uci), width = 0.2) +
  theme_classic() +
  labs(x = "Priors", y = "Estimate (95% CI)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

