# Roll our own dbinom
binomial_pmf <- function(k, n, p) {
  if (k > n || k < 0) {
    ('k is greater than n or k is less than 0')
    return(0)  # PMF is 0 for impossible cases
  }
  # Compute the binomial coefficient
  binom_coeff <- factorial(n) / (factorial(k) * factorial(n - k))
  
  # same thing (choose is a shortcut to writing out all the factorials)
  # binom_coeff <- choose(n, k)
  
  # Compute the PMF
  pmf <- binom_coeff * (p^k) * ((1 - p)^(n - k))
  
  return(pmf)
}

# Set up parameters
n <- 10  # number of trials
p <- 0.5 # probability of success
k <- 3   # number of successes

# Calculate PMF
pmf <- binomial_pmf(k, n, p)

# did we do it correctly??
pmf == dbinom(x = k, size = n, prob = p)

# Compute PMF for all k from 0 to n
pmf_values <- sapply(0:n, binomial_pmf, n = n, p = p)
data.frame(Successes = 0:n, PMF = pmf_values)

# equivalent to pmf_values
dbinom(0:n, size = n, prob = p)



