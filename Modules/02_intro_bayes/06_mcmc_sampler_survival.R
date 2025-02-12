# Code for writing a Metropolis sampler for a 1D posterior

## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

# Function to compute the posterior distribution:
# Posterior is proportional to likelihood * prior
# Likelihood: Y[i] ~ binomial(J, p)            # binomial with a J trials
# Prior:      p ~ Beta(a, b)            # Beta(1,1) ~~ uniform(0,1)
posterior <- function(p, data, captured, a, b){
  prior <- dbeta(x = p, shape1 = a, shape2 = b)
  like <- prod(dbinom(data, size=captured, p=p)) # prod because more than one observation
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
y <- rbinom(n = 5, size = 20, p = 0.4)  # note truth here is 0.7 - probability of detecting, observing, surviving, etc.


# Compute the posterior on a grid for plotting below
p_grid <- seq(0,1,length=100)
dense <- rep(0,100)
for(i in 1:100){
  dense[i] <- posterior(p = p_grid[i], data = y, captured = 20, a = 2, b = 4)
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
plot_please = F
par(ask=TRUE,mfrow=c(1,1))
for(i in 1:n.iters){
  
  # Draw a candidate and compute acceptance ratio**:
  
  can <- rnorm(1,p,tune)
  if(can < 0 | can > 1){next} 
  p1  <- posterior(can,y,captured = 20, a = 2, b = 4) 
  p2  <- posterior(p,y,captured = 20, a = 2, b = 4)
  R   <- p1/p2
  R   <- ifelse(R>1,1,R)
  
  # Plot the candidate:
  if(plot_please){
    if(i<=n.iters * 0.001 | i >= n.iters * 0.001){
      
      plot(p_grid,dense,type="l",lwd=2,
           xlab=expression(p),ylab=expression(paste("f(",p,"|Y)")))
      lines(c(p,p),c(0,p2),col=2,lwd=2)
      lines(rep(can,2),c(0,p1),col=3,lwd=2)
      
      leg    <- NULL
      leg[1] <- paste("Iteration =", i)
      leg[2] <- paste("R = ",round(R,2))
      leg[3] <- paste("Initial/Old value = ",round(p,2))
      leg[4] <- paste("Candidate = ",round(can,2))
      
      legend("topleft",leg,lty=1,col=c(NA, NA, 2, 3),inset=0.05)
      if(i == 9985){
        legend("topright","We're at the end\n of the chain",inset=0.05)
      }
    }
  }
  
  
  # Make a decision: 
  keep <- rbinom(1,1,R)==1
  # keep <- runif(1) < R # alternative approach
  if(keep){
    p <- can  #here is where we overwrite our initial p with our candidate value if it passes test
  }
  keep_p[i] <- p
  
}


# summarize MCMC
sampleStats(keep_p)

# plot posterior
par(ask=F,mfrow=c(1,1))
post_grid <- seq(0,1,length=length(keep_p))
plot(post_grid,keep_p,type="l",lwd=2,
     xlab=expression(p),ylab=expression(paste("f(",p,"|Y)")))

# What if we try different prior?
