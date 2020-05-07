library(rstan)
library(bayesplot)

# LOCAL SETTINGS #
setwd("/home/emaballarin/DSSC/statmeth/HOMEWORK/giacomo-coltello/src/")

# STAN PRELIMINARIES #

n <- 10  # sample size


# Priors:
sigma2 <- 2  # likelihood variance
tau2 <- 2    # prior variance
mu <- 7      # prior mean


# Data (simulation):
theta_sample <- 2  # true mean

set.seed(123)
y <- rnorm(n,theta_sample, sqrt(sigma2))


# MCMC run:
data <- list(N = n, y = y, sigma = sqrt(sigma2), mu = mu, tau = sqrt(tau2))
fit <- rstan::stan(file="normal.stan", data = data, chains = 4, iter=2000)

# From exercise text:
posterior <- as.array(fit)



# ACTUAL EXERCISE #

bayesplot::color_scheme_set(scheme = "brightblue")
bayesplot_theme_update(panel.background = element_rect(fill = "black"))

bayesplot::mcmc_intervals(posterior,
                          pars = c("theta"))

mcmc_areas(posterior,
           pars = c("theta"))

mcmc_dens_overlay(posterior, pars = c("theta"),
                  facet_args = list(nrow = 2)) +
                facet_text(size = 13)
