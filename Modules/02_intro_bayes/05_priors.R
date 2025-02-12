## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)


## ----fig.height=5, fig.width=8, message=F, warning=F--------------------------
library(ggplot2)
library(dplyr)
library(colorspace)

unf_df <- data.frame(x = seq(0,1,0.01),
                      y = dunif(seq(0,1,0.01), 0, 1))

ggplot(unf_df, aes(x, y)) + geom_area(fill = "#660000",  
                                       color = "#660000") + 
  scale_x_continuous(expression(theta)) +
  scale_y_continuous(expression(paste("Probability density of ", theta)), limits = c(0, 1.5)) +
  labs(subtitle = "Uniform(0,1)") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        plot.subtitle = element_text(size = 18))




## ----fig.height=5, fig.width=8------------------------------------------------
beta_df <- data.frame(x = seq(0,1,0.01),
                      y = dbeta(seq(0,1,0.01), 1, 1))

ggplot(beta_df, aes(x, y)) + geom_area(fill = "#660000",  
                                       color = "#660000") + 
  scale_x_continuous(expression(theta)) +
  scale_y_continuous(expression(paste("Probability density of ", theta)), limits = c(0, 1.5)) +
  labs(subtitle = "Beta(1,1)") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        plot.subtitle = element_text(size = 18))



## ----fig.width=8, fig.height=5------------------------------------------------
norm_df <- data.frame(x = seq(-1000,1000,1),
                      y = dnorm(seq(-1000,1000,1), 0, 100))

ggplot(norm_df, aes(x, y)) + geom_area(fill = "#660000",  
                                       color = "#660000") + 
  scale_x_continuous(expression(theta)) +
  scale_y_continuous(expression(paste("Probability density of ", theta))) +
  labs(subtitle = "Normal(0, 100)")+
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        plot.subtitle = element_text(size = 18))


## ----fig.width=8, fig.height=5------------------------------------------------
norm_df <- data.frame(x = seq(-10,10,1),
                      y = dnorm(seq(-10,10,1), 0, 100))

ggplot(norm_df, aes(x, y)) + geom_area(fill = "#660000",  
                                       color = "#660000") + 
  scale_x_continuous(expression(theta)) +
  scale_y_continuous(expression(paste("Probability density of ", theta))) +
  labs(subtitle = "Normal(0, 100)") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        plot.subtitle = element_text(size = 18))


## ----fig.width=8, fig.height=5------------------------------------------------
library(cowplot)
library(ggplot2)
norm_df <- data.frame(x = seq(-1000,1000,1),
                      y = dnorm(seq(-1000,1000,1), 0, 100))

a <- ggplot(norm_df, aes(x, y)) + geom_area(fill = "#660000",  
                                       color = "#660000") + 
  scale_x_continuous(expression(theta)) +
  scale_y_continuous(expression(paste("Probability density of ", theta))) +
  labs(subtitle = "Normal(0, 100)")+
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        plot.subtitle = element_text(size = 18))


b <- ggplot(norm_df, aes(x, y)) + geom_area(fill = "#660000",  
                                       color = "#660000") + 
  scale_x_continuous(expression(theta)) +
  scale_y_continuous(expression(paste("Probability density of ", theta)), limits = c(0,1)) + 
  labs(subtitle = "Normal(0, 100)") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        plot.subtitle = element_text(size = 18))

plot_grid(plotlist = list(a,b), nrow = 1)


## ----fig.width=8, fig.height=5------------------------------------------------
unif_df <- data.frame(x = seq(0.1, 50, 0.1),
                      y = dunif(seq(0.1, 50, 0.1), min = 0.1, max = 50))
a <- ggplot(unif_df, aes(x, y)) + geom_area(fill = "#660000",  
                                       color = "#660000") + 
  scale_x_continuous(expression(theta)) +
  scale_y_continuous(expression(paste("Probability density of ", theta))) +
  labs(subtitle = "Unif(0.01, 50)") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        plot.subtitle = element_text(size = 18))

gamma_df <- data.frame(x = seq(0.1,50,0.1),
                      y = dgamma(seq(0.1,50,0.1), shape = 0.001, rate = 0.001))

b <- ggplot(gamma_df, aes(x, y)) + geom_area(fill = "#660000",  
                                       color = "#660000") + 
  scale_x_continuous(expression(theta)) +
  scale_y_continuous(expression(paste("Probability density of ", theta))) +
  labs(subtitle = "Gamma(0.01, 0.01)") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        plot.subtitle = element_text(size = 18))

plot_grid(plotlist = list(a,b), nrow = 1)


## ----warning=FALSE------------------------------------------------------------
par(mfrow = c(1,4))
aa <- c(1,2,5,20)
bb <- c(1,5,2,1)
for(i in 1:length(aa)){
  a <- aa[i]
  b <- bb[i]
  N <- 10
  successes <- 2
  failures <- N - successes
  theta = seq(0.005, 0.995, length = 101)
  prior <- dbeta(x = theta, shape1 = a, shape2 = b)
  likelihood <- dbeta(theta, shape1 = successes + 1, shape2 = N-successes + 1) #rule of succession
  posterior <- dbeta(theta, a + successes, b + N - successes)
  
  # for plotting different densities on same figure
  m.orig = apply( cbind(prior, likelihood, posterior), 2, max)
  m = max(c(prior, likelihood, posterior))
  
  plot(theta, posterior, type = "l", ylab = "Density", 
       lty = 2, lwd = 3, ylim = c(0, m), xlim = c(min(theta), max(theta)),col = 1,
       main = paste0("Prior: beta(", a,", ", b, ")"))
  lines(theta, likelihood, lty = 1, lwd = 3, col = 2)
  lines(theta, prior, lty = 3, lwd = 3, col = 3)
  legend("topleft",y=m, c("Prior", "Likelihood", "Posterior"), lty = c(3, 1, 2),
         lwd = c(3, 3, 3), col = c(3, 2, 1))
}




## ----warning=F----------------------------------------------------------------
par(mfrow = c(1,1))
library(ggplot2)
set.seed(2756)
psi_samples <- runif(10000, 0, 1)
logit_psi <- qlogis(psi_samples)
transform_df <- data.frame(psi = psi_samples, logit_psi = logit_psi)
a <- ggplot(data = transform_df, aes(x = psi_samples)) +
  geom_histogram(bins = 50, fill = "#660000",, color = "#660000") +
  scale_y_continuous(lim = c(0,1000)) +
  labs(x = "p", y = "Frequency") +
  theme(axis.text = element_text(angle = 45, hjust = 1, size = 18),
        axis.title = element_text(size = 18),
        plot.subtitle = element_text(size = 18))
b <- ggplot(data = transform_df, aes(x = logit_psi)) +
  geom_histogram(bins = 50, fill = "#660000",, color = "#660000") +
  labs(x = "logit(p)", y = "Frequency") +
  scale_y_continuous(lim = c(0,1000)) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        plot.subtitle = element_text(size = 18))

plot_grid(plotlist = list(a,b), nrow = 1)


## -----------------------------------------------------------------------------
conjugates <- data.frame(Likelihood = c("$y_i \\sim binomial(n, p)$",
                                        "$y_i \\sim Bernoulli(p)$",
                                        "$y_i \\sim Poisson(\\lambda)$"),
                         Prior = c(c("$p \\sim beta(\\alpha, \\beta)$",
                                        "$p \\sim beta(\\alpha, \\beta)$",
                                        "$\\lambda \\sim gamma(\\alpha, \\beta)$")),
                         Posterior = c("$p \\sim beta(\\sum y_i + \\alpha, n -\\sum y_i + \\beta)$",
                                        "$p \\sim beta(\\sum_{i=1}^n y_i + \\alpha, \\sum_{i=1}^n (1-y_i) + \\beta)$",
                                        "$\\lambda \\sim gamma(\\alpha \\sum_{i=1}^n y_i, \\beta + n)$"))

kableExtra::kable(conjugates, "latex", align="c", booktabs=TRUE, escape = F, caption = 'A few conjugate distributions', format = "html")


## ----fig.width=5, fig.height=3------------------------------------------------
p <- seq(0,1,0.01)
df <- data.frame(p = p, 
                 Density = c(dbeta(p, 80, 80), dbeta(p, 3,2)),
                 parameter = rep(c("Pr(heads)", "Pr(survive)"), each = length(p)))

ggplot(df, aes(x = p, y = Density, color = parameter)) + geom_path() +
  scale_color_manual(values = c("#660000", "#00CCCC")) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  annotate("text", x = 0.28, y = 9, label = "beta(80,80)", size = 5, color = "#00CCCC") +
  annotate("text", x = 0.75, y = 2.5, label = "beta(3,2)", size = 5, color = "#660000")


## ----message=F,echo = F-------------------------------------------------------
knitr::purl(input = here::here('Modules/02_intro_bayes/05_priors.Rmd'),
            output = here::here('Modules/02_intro_bayes/05_priors.R'))

