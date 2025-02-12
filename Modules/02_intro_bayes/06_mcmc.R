## ----setup, include=FALSE------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)


## ----echo = TRUE, fig.height=2, fig.width=4, warning = FALSE, comment=FALSE----------------------------------
library(ggplot2)
norm_df <- data.frame(samples = rnorm(500))    # Generate samples from distribution

suppressWarnings(ggplot(norm_df, aes(samples)) + 
  geom_histogram(fill = "#660000") + 
  annotate("text", x = 0, y = 750, label = paste("Sample mean = ", round(mean(norm_df$samples),2))) + annotate("text", x = 0, y = 675, label = paste("Sample variance = ", round(sd(norm_df$samples),2))))



## ----echo = TRUE, fig.height=2, fig.width=4, warning = F-----------------------------------------------------
beta_df <- data.frame(samples = rbeta(n = 5000, shape1 = 2, shape2 = 5))

suppressWarnings(ggplot(beta_df, aes(samples)) + geom_histogram(fill = "#660000"))


## ----setup2, out.width='50%', fig.align='center', fig.cap='', warning = F, message = F-----------------------
knitr::include_graphics(here::here('Modules/02_Intro_Bayes/files_for_slides/coyote.jpg'))


## ----echo = T------------------------------------------------------------------------------------------------
set.seed(277)
y <- rbinom(n = 20,size = 1, prob = 0.7)  # note truth here is 0.7 - probability of detecting, observing, surviving, etc.



## ------------------------------------------------------------------------------------------------------------
beta_df <- data.frame(p = seq(0, 1, 0.01),
                      value = dbeta(seq(0, 1, 0.01), 8, 4),
                      dist = rep(c("Prior"), 101))

(p <- ggplot() + geom_path(data = beta_df, aes(x = p, y = value, group = dist, linetype = dist)) +
  scale_y_continuous("Density") +
  scale_linetype_manual(values = c("solid", "dotted")))


## ----echo = T, eval= F---------------------------------------------------------------------------------------
## 
## dbinom(x = 0:5, size = 5, prob = 0.2)

## ----echo =F, eval= T----------------------------------------------------------------------------------------

round(dbinom(x = 0:5, size = 5, prob = 0.2),2)


## ----echo = T, eval = F--------------------------------------------------------------------------------------
## posterior <- function(p, data){
##   prior <- dbeta(x = p, shape1 = 8, shape2 = 4)
##   like <- prod(dbinom(x = data, size=1, p=p)) # prod because more than one observation
##   return(like*prior)}


## ----echo = T, eval = F--------------------------------------------------------------------------------------
## # Initial Value (Starting Point) for MCMC
## p <- runif(1)
## (p)


## ----echo = T, eval = F--------------------------------------------------------------------------------------
## proposal <- function(p, sigma2){
##   alpha <- p * ((p * (1 - p) / sigma2) - 1)
##   beta <- (1 - p) * ((p * (1 - p) / sigma2) - 1)
## 
##   proposal <- rbeta(n = 1, shape1 = alpha, shape2 = beta)
##   return(proposal)
## }


## ----echo = T, eval = F--------------------------------------------------------------------------------------
## # MCMC set-up and 'tuning' parameter
## # tuning perturbs current value of parameter by some random noise
## n.iters <- 10000
## tune  <- 0.05 # you can try different tuning values to see its impact on acceptance rate
## 


## ----echo = T, eval = F--------------------------------------------------------------------------------------
## keep_p <- rep(0,n.iters) #holder for output
## 
## for(i in 1:n.iters){
## 
##   # Draw a candidate and compute acceptance ratio**:
## 
##   can <- rnorm(1,p,tune)
##   if(can < 0 | can > 1){next}
##   p1  <- posterior(can,y)
##   p2  <- posterior(p,y)
##   R   <- p1/p2
##   R   <- ifelse(R>1,1,R)
## 
##   # Make a decision:
##   keep <- rbinom(1,1,R)==1
##   # keep <- runif(1) < R # alternative approach
##   if(keep){
##     p <- can  #here is where we overwrite our initial p with our candidate value if it passes test
##   }
##   keep_p[i] <- p #store values of p - this is our *chain*!
## 
## }
## 


## ----echo = F, eval = T--------------------------------------------------------------------------------------
# Code for writing a Metropolis sampler for a 1D posterior

## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

# Function to compute the posterior distribution:
# Posterior is proportional to likelihood * prior
# Likelihood: Y[i] ~ Bern(p)            # binomial with a single trial
# Prior:      p ~ Beta(a, b)            # Beta(1,1) ~~ uniform(0,1)
posterior <- function(p, data){
  prior <- dbeta(x = p, shape1 = 8, shape2 = 4)
  like <- prod(dbinom(data, size=1, p=p)) # prod because more than one observation
  return(like*prior)}

sampleStats <- function(x){
  mean <- mean(x)
  sd <- sd(x)
  quants <- quantile(x, probs = c(0.025, 0.5, 0.975))
  out <- matrix(data = c(mean, sd, quants), nrow = 1, dimnames = list(1,c('mean',
                                                                          'sd',
                                                                          'lower95',
                                                                          'median',
                                                                          'upper95')))
  return(out)
}


# Simulate data
set.seed(277)
y <- rbinom(20,1,0.7)  # note truth here is 0.7 - probability of detecting, observing, surviving, etc.


# Compute the posterior on a grid for plotting below
p_grid <- seq(0,1,length=100)
dense <- rep(0,100)
for(i in 1:100){
  dense[i] <- posterior(p = p_grid[i], data = y)
}


# MCMC set-up and 'tuning' parameter
# tuning perturbs current value of parameter by some random noise
n.iters <- 10000
tune  <- 0.05 # you can try different tuning values to see its impact on acceptance rate


#initial (starting) value of mcmc
p <- runif(1)
keep_p <- rep(0,n.iters)


# **If the product of likelihood and prior is greater for the 
# proposed value of p than for the initial/current value,
# the proposed value is accepted, though if the initial/current value is smaller than
# the proposed value, initial/current may still be accepted, but is done so only with a probability equal to the ratio R.

# Begin the MCMC loop
for(i in 1:n.iters){
  
  # Draw a candidate and compute acceptance ratio**:

  can <- rnorm(1,p,tune)
  if(can < 0 | can > 1){next} 
  p1  <- posterior(can,y) 
  p2  <- posterior(p,y)
  R   <- p1/p2
  R   <- ifelse(R>1,1,R)

  
  # Make a decision: 
  keep <- rbinom(1,1,R)==1
  # keep <- runif(1) < R # alternative approach
  if(keep){
    p <- can  #here is where we overwrite our initial p with our candidate value if it passes test
  }
  keep_p[i] <- p
  
}


## ----echo = T, eval=T----------------------------------------------------------------------------------------

sampleStats <- function(x){
  mean <- mean(x)
  sd <- sd(x)
  quants <- quantile(x, probs = c(0.025, 0.5, 0.975))
  out <- matrix(data = c(mean, sd, quants), nrow = 1, dimnames = list(1,c('mean',
                                                                          'sd',
                                                                          'lower95',
                                                                          'median',
                                                                          'upper95')))
  return(out)
}

# summarize MCMC
sampleStats(keep_p)

# plot posterior density
par(ask=F,mfrow=c(1,2))
plot(density(keep_p), frame = FALSE, col = "#660000",main = "", lwd = 2,
     xlab=expression(p),ylab=expression(paste("f(",p,"|Y)")))

# plot MCMC chain trace plot

post_grid <- seq(0,1,length=length(keep_p))
plot(post_grid,keep_p,type="l",lwd=2,
     xlab=expression(p),ylab=expression(paste("f(",p,"|Y)")))



## ----echo = F------------------------------------------------------------------------------------------------
chains <- data.frame(iteration = rep(seq(1:n.iters), 3),
                     chain = rep(c("1", "2", "3"), each = n.iters),
                     p = sample(keep_p[10:n.iters], n.iters * 3, replace = TRUE))

ggplot(chains, aes(x = iteration, y = p, color = chain)) + geom_path() +
  scale_color_manual(values = c("#660000", "#00CCCC", "#66CC00")) +
  ggthemes::theme_clean()


## ----fig.height=4, fig.width=8, echo = F---------------------------------------------------------------------
poster <- data.frame(iteration = 1:n.iters, p = keep_p)

a1 <- ggplot(poster[1:100,], aes(x = iteration, y = p)) + 
  geom_path(color = "#660000") +
  scale_y_continuous(expression(p))

a2 <- ggplot(poster[1:100,], aes(x = p)) + geom_density() +
  scale_x_continuous(expression(p))

b1 <- ggplot(poster[1:500,], aes(x = iteration, y = p)) + 
  geom_path(color = "#660000") +
  scale_y_continuous(expression(p))

b2 <- ggplot(poster[1:500,], aes(x = p)) + geom_density() +
  scale_x_continuous(expression(p))

c1 <- ggplot(poster[1:5000,], aes(x = iteration, y = p)) + 
  geom_path(color = "#660000") +
  scale_y_continuous(expression(p))

c2 <- ggplot(poster[1:5000,], aes(x = p)) + geom_density() +
  scale_x_continuous(expression(p))

d1 <- ggplot(poster, aes(x = iteration, y = p)) + 
  geom_path(color = "#660000") +
  scale_y_continuous(expression(p))

d2 <- ggplot(poster, aes(x = p)) + geom_density() +
  scale_x_continuous(expression(p))

cowplot::plot_grid(a1, b1, c1, d1, a2, b2, c2, d2, 
                   labels = c("1A)", "2A)","3A)", "4A)",
                              "1B)", "2B)", "3B)", "4B)"),
                   nrow = 2)



## ----fig.height=3, fig.width=5-------------------------------------------------------------------------------
ggplot(chains, aes(x = iteration, y = p, color = chain)) + geom_path() +
  scale_color_manual(values = c("#660000", "#00CCCC", "#66CC00")) +
  ggthemes::theme_clean()


## ----out.width='50%', fig.align='center', fig.cap='', warning = F, message = F-------------------------------
knitr::include_graphics(here::here('Modules/02_Intro_Bayes/files_for_slides/converge1.jpg'))
knitr::include_graphics(here::here('Modules/02_Intro_Bayes/files_for_slides/converge2.jpg'))
knitr::include_graphics(here::here('Modules/02_Intro_Bayes/files_for_slides/converge3.jpg'))
knitr::include_graphics(here::here('Modules/02_Intro_Bayes/files_for_slides/converge4.jpg'))


## ----fig.height=3, fig.width=5-------------------------------------------------------------------------------
ggplot(poster[1:100,], aes(x = iteration, y = p)) + 
  geom_path(color = "#660000") +
  scale_y_continuous(expression(p))


## ----fig.height=3, fig.width=5-------------------------------------------------------------------------------
ggplot(poster, aes(x = iteration, y = p)) + 
  geom_path(color = "#660000") +
  scale_y_continuous(expression(p))

