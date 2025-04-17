library(here)

sp <- read.csv(here('Data/sparrows.csv'), header = T)

summary(lm(wingcrd ~ wt, data = sp))

head(sp)


b <- 43.17
m <- .734
x <- sp$wt
e <- rnorm(1295, mean = 0, sd = 1.784)
y = m*x + b + e

b0 <- -1
b1 <- 2
x <-  runif(n = 100, -1, 1)

y <- rnorm(n = 100, mean = b0 + b1*x, sd = 1)

summary(lm(y ~ x))
