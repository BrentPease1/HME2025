# Probably functions in R (d, p, q, and r)
# Code adapted from Jack Weiss's courses at UNC

# What is your favorite continuous number between -4 and 4? 
fav_value <- -2.3 # let's all start with 2, then play around after we discuss

# (after you set your fav_value, just Source the script)

## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
# set up plotting parameters
par(xpd=F)

# generate normal distribution plot that we will build on
curve(dnorm,-4,4)
mtext(side=3,line=.3,'X ~ Normal(0,1)',cex=.9,font=2)


# set up x variable that we will get probabilities for 
# get a sequence of numbers from -4 to 2
xvar <- seq(-4,fav_value,length=100)

# get probability of observing each value in xvar under a normal(0,1)
# NOTE: default values of dnorm is mean = 0, and sd = 1 (standard normal)
#       so we don't have to explicitly state those arguments
dvals<-dnorm(xvar)

## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
#### PNORM ####
polygon(c(xvar, rev(xvar)), c(rep(0, 100), rev(dvals)), col = 'grey70')

# annotate plot with info
arrows(fav_value, 0.15, fav_value, 0.25, code = 1, angle = 45, length = 0.1, lwd = 2)
text(fav_value, 0.25, paste('pnorm(', fav_value, ') = ', round(pnorm(fav_value), 3)), 
     pos = 3, cex = 0.85)

## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
#### DNORM ####
# annotate plot with dnorm
segments(fav_value, 0, fav_value, dnorm(fav_value), lwd = 2, col = 'tomato')
arrows(fav_value, dnorm(fav_value), fav_value + 0.35, 0.13, code = 1, angle = 30, 
       length = 0.1, lwd = 2, col = 'tomato')
text(fav_value + 0.35, 0.13, paste('dnorm(', fav_value, ') =', round(dnorm(fav_value), 3)), 
     cex = 0.85, pos = 3, col = 'tomato')

## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
#### QNORM ####

points(fav_value, 0, col = 'dodgerblue', pch = 16, cex = 1.1)
arrows(fav_value, 0, fav_value + 1.1, 0.05, code = 1, angle = 30, length = 0.1, lwd = 2, 
       col = 'dodgerblue')
text(fav_value + 0.25, 0.05, paste('qnorm(', round(pnorm(fav_value), 3), ') =', fav_value), 
     cex = 0.85, pos = 3, col = 'dodgerblue')

## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
#### RNORM ####
par(xpd=T)

points(rnorm(50), jitter(rep(0, 50)), pch = 16, cex = 0.6, col = 'seagreen')
arrows(0, 0.01, 0, 0.15, code = 1, angle = 30, length = 0.1, 
       lwd = 2, col = 'seagreen')
text(0, 0.15, 'rnorm(50)', col = 'darkgreen', cex = 0.85, pos = 3)

# END
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --