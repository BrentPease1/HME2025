
# Student guesses for what proportion of the focal species (pink paper) the box contained
thetas <- c(0.5, 0.33, 0)

# The number of samples
n <- 10
# how many were pink
k <- 3

# our prior belief in the corresponding thetas
priors <- c(0.1, 0.7, 0.2)

# probability associated with each theta based on our observed data
likelihoods <- dbinom(k, n, thetas)

# set up the scaling constant for the denominator of our bayes rule equation
unnormal_pos <- priors * likelihoods
normalize_pos <- unnormal_pos / sum(unnormal_pos)

# package it all up
data.frame(theta = thetas,
           likelihood = likelihoods,
           priors = priors,
           posterior = normalize_pos)

## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

## ROUND 2 - repeat and update our knowledge about the contents of the box
thetas <- c(0.5, 0.33, 0)
priors <- normalize_pos

# NEW KNOWLEDGE
n <- 10
k <- 6

likelihoods <- dbinom(k, n, thetas)

unnormal_pos <- priors * likelihoods
normalize_pos <- unnormal_pos / sum(unnormal_pos)

data.frame(theta = thetas,
           likelihood = likelihoods,
           priors = priors,
           posterior = normalize_pos)
