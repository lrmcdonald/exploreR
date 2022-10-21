#' Discrete Variables
#' Binomial
#' Number of successes in n trials
#' @export
pbinomd = function(x = 1:10, # number of successes (0 above n)
                   n = 5, # the number of Bernoulli trials run
                   p = 0.5 # probability that each Bernoulli trial is a success
){
  dens = dbinom(x = x, size = n, prob = p)
  df = data.frame(Value = x, Density = dens)
  with(df, barplot(Density~Value,
                   main = "Binomial Probability Distribution",
                   xlab = paste0("Number of successes in ", n,
                                 " trials with a ", p*100,
                                 "% probability of success.")))
}

#' Bernoulli
#' Binomial with N = 1. One run of an experiment with a binary outcome (e.g., toss a coin; can get either heads or tails)
#' @export
pbernd = function(x = 0:1, # fail/success
                  p = 0.8 # probability that the Bernoulli trial is a success
){
  dens = dbinom(x = x, size = 1, prob = p)
  df = data.frame(Value = x, Density = dens)
  with(df, barplot(Density~Value,
                   main = "Bernoulli Probability Distribution",
                   xlab = paste0("Fails (0) and successes (1) in a trial with a ",
                                 p*100, "% probability of success")))
}

#' Poisson
#' Probability of a fixed number of events in a given interval (often of time or space), given that events occur at a constant rate
#' @export
ppoisd = function(x = 1:10, # number of events
                  lambda = 5 # the rate at which events occur
){
  dens = dpois(x = x, lambda = lambda, log = F)
  df = data.frame(Value = x, Density = dens)
  with(df, barplot(Density~Value,
                   main = "Poisson Probability Distribution",
                   xlab = paste0("Number of events occurring at a rate of ", lambda)))
}

#' Log-Poisson
#' @export
plpoisd = function(x = 1:10, # number of events
                   lambda = 5 # the rate at which events occur
){
  dens = dpois(x = x, lambda = lambda, log = T)
  df = data.frame(Value = x, Density = dens)
  with(df, barplot(Density~Value, main = "Poisson Log-Probability Distribution",
                   xlab = paste0("Number of events occurring at a rate of ", lambda)))
}

#' Negative Binomial
#' In a sequence of independent and identically distributed Bernoulli trials with success probability p, if X = number of trials until the rth success, then X ~ NBin(r, p).
#' @export
pnbinomd = function(x = 1:100, # number of trials until rth success
                    r = 50, # number successes required
                    p = 0.5 # success probability
){
  dens = dnbinom(x = x, size = r, prob = p)
  df = data.frame(Value = x, Density = dens)
  with(df, barplot(Density~Value,
                   main = "Negative Binomial Probability Distribution",
                   xlab = paste0("Number of trials until the ", r,
                                 "th success with a probability of ", p*100,
                                 "% per trial")))
}

#' Geometric
#' In a sequence of independent and identically distributed Bernoulli trials with success probability p, if X = number of trials until the 1st success, then X ~ Geo(p).
#' @export
pgeomd = function(x = 1:10, # number of trials until first success
                  p = 0.5 # success probability
){
  dens = dgeom(x = x, prob = p, log = F)
  df = data.frame(Value = x, Density = dens)
  with(df, barplot(Density~Value, main = "Geometric Probability Distribution",
                   xlab = paste0("Number of trials until the 1st success with
                               a probability of ", p*100,
                               "% per trial")))
}
#' Log-Geometric
#' @export
plgeomd = function(x = 1:10, # number of trials until first success
                   p = 0.5 # success probability
){
  dens = dgeom(x = x, prob = p, log = T)
  df = data.frame(Value = x, Density = dens)
  with(df, barplot(Density~Value, main = "Geometric Log-Probability Distribution",
                   xlab = paste0("Number of trials until the 1st success with
                               a probability of ", p*100,
                               "% per trial")))
}

#' Hypergeometric
#' In a population of total size N with two types of individuals, Type 1 and Type 2 (M type 1's and N - M type 2's), X = number of Type 1's in a sample of size n drawn without replacement
#' @export
phyperd = function(draws = 1:10, # number of type 1's drawn in sample os size = trials
                   type1 = 5, # number of individuals of type 1
                   type2 = 5, # number of individuals of type 2
                   trials = 8 # number of trials
){
  dens = dhyper(x = draws, m = type1, n = type2, k = trials, log = F)
  df = data.frame(Value = draws, Density = dens)
  with(df, barplot(Density~Value, main = "Hypergeometric Probability Distribution",
                   xlab = paste0("Number of As in a sample drawn without replacement in ", trials, " trials from a pop with ", type1, " As and ",
                                 type2, " Bs")))
}

#' Continuous Variables
#' Uniform
#' Within some bounded interval, all outcomes have an equal probability of occurrence.
#' @export
punifd = function(x = seq(0, 1, by = 0.01), # outcome
                  a = 0.1, # minumum value
                  b = 0.9 # maxumum value
){
  dens = dunif(x = x, min = a, max = b)
  df = data.frame(Value = x, Density = dens)
  ggplot2::ggplot(df) +
    ggplot2::geom_line(ggplot2::aes(x = Value, y = Density)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Uniform Probability Distribution",
                  x = paste0("Outcomes within minimum, ", a, ", and maximum, ", b))
}

#' Normal
#' Among other process bases, according to the Central Limit Theorem, the averages of samples of observations of random variables independently drawn from independent distributions converge in distribution to the normal
#' @export
pnormd = function(x = seq(0, 100, by = 0.01), # outcome
                  mu = 25, # mean value
                  sigma = 2 # standard deviation
){
  dens = dnorm(x =x, mean = mu, sd = sigma, log = F)
  df = data.frame(Value = x, Density = dens)
  ggplot2::ggplot(df) +
    ggplot2::geom_line(ggplot2::aes(x = Value, y = Density)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Normal Probability Distribution",
                  x = paste0("Outcomes with a mean of ", mu, " and a standard deviation of ", sigma))
}
#' Log-Normal
#' @export
plnormd = function(x = seq(0, 5, by = 0.01), # outcome
                   mu = 5, # mean value
                   sigma = 2, # standard deviation
                   logpars = F # whether input is log scale or not - if false - take log
){
  mu = ifelse(logpars, mu, log(mu))
  sigma = ifelse(logpars, sigma, log(sigma))
  dens = dlnorm(x = x, meanlog = mu, sdlog = sigma)
  df = data.frame(Value = x, Density = dens)
  ggplot2::ggplot(df) +
    ggplot2::geom_line(ggplot2::aes(x = Value, y = Density)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Log-Normal Probability Distribution",
                  x = paste0("Outcomes with a mean of ", round(exp(mu), 2), " and a standard deviation of ",
                             round(exp(sigma), 2), " (meanlog = ", round(mu, 2), ", sdlog = ", round(sigma, 2), ")"))
}

#' Exponential
#' Describes the time between events in a Poisson point process
#' @export
pexpd = function(x = seq(0, 1, by = 0.01), # outcome
                 rate = 5 # rate at which events occur
){
  dens = dexp(x = x, rate = rate)
  df = data.frame(Value = x, Density = dens)
  ggplot2::ggplot(df) +
    ggplot2::geom_line(ggplot2::aes(x = Value, y = Density)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Exponential Probability Distribution",
                  x = paste0("Outcomes occurring with a rate of ", rate))
}

#' Gamma
#' A stochastic process with independent, non-negative increments having a gamma distribution with an identical scale parameter and a time-dependent shape parameter.
#' @export
pgammad = function(x = seq(0, 1, by = 0.01), # outcome
                   alpha = 5, # shape parameter
                   beta = 2 # rate paramter
){
  mu = round(alpha / beta, digits = 1)
  sigma = round(alpha / beta^2, digits = 1)
  dens = dgamma(x = x, shape = alpha, rate = beta)
  df = data.frame(Value = x, Density = dens)
  ggplot2::ggplot(df) +
    ggplot2::geom_line(ggplot2::aes(x = Value, y = Density)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Gamma Probability Distribution",
         x = paste0("Outcomes occurring with a mean of ", mu, " and a standard deviation of ", sigma,
                    " (alpha = ", alpha, " and beta = ", beta, ")"))
}

#' Beta
#' Continuous distribution bounded to lie between 0 and 1. Commonly used as a distribution describing p, the per-trial success probability for Bernoulli trials.
#' @export
pbetad = function(x = seq(0, 1, by = 0.01), # outcome
                  alpha = 5, # shape parameter 1
                  beta = 2 # shape paramter 2
){
  mu = alpha / (alpha + beta)
  mu = round(mu, digits = 1)
  sigma = (alpha * beta) / ((alpha + beta)^2 * (alpha + beta + 1))
  sigma = round(sigma, digits = 1)
  dens = dbeta(x = x, shape1 = alpha, shape2 = beta)
  df = data.frame(Value = x, Density = dens)
  ggplot2::ggplot(df) +
    ggplot2::geom_line(ggplot2::aes(x = Value, y = Density)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Beta Probability Distribution",
         x = paste0("Outcomes occurring with a mean of ", mu, " and a standard deviation of ", sigma,
                    " (alpha = ", alpha, " and beta = ", beta,")"))
}

#' Von Mises
#' The maximum entropy distribution for circular data when the real and imaginary parts of the first circular moment are specified
#' @export
pvonmisesd = function(x = seq(-pi, pi, by = 0.01), # outcome
                      mu = 1, # mean direction
                      kappa = 0.1 # concentrtion around mean
){
  dens = circular::dvonmises(x = x, mu = mu, kappa = kappa)
  df = data.frame(Value = x, Density = dens)
  ggplot2::ggplot(df) +
    ggplot2::geom_line(ggplot2::aes(x = Value, y = Density)) +
    ggplot2::coord_polar() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Von Mises Probability Distribution",
                  x = paste0("Angles occuring with a mean of ", mu, " radians and a concentration of ", kappa))
}
