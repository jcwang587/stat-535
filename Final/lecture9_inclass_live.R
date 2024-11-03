# Agenda
# ===

# - Sampling from distributions that are established in R
# - Sampling from discrete distributions
# - Inverse Transform Sampling
# - Acceptance-Rejection Sampling
# - Box-Muller Method


# Introduction
# ===
# The term "sampling" has two distinct meanings in statistical science.

# 1. The process of collecting samples from some real world population, typically trying to ensure
#    that the data have some "good" properties. E.g. conducting randomized trials in an experiment 
#    in order to guarantee, e.g., zero bias.

# In this class, we will only discuss:

# 2. Sampling is also the process of generating draws from a theoretical probability distribution. 
#    Such samples may be used to explore the properties of the distribution, for model validation,
#    and are the basis for many statistical and computational methods that involve random numbers.

# Even though there are good methods to generate draws from distributions like the uniform, the normal
# and some others, it is sometimes hard (or very hard) to generate draws from many other random variables,
# even if their density and their probabilistic properties are known. 

# In this lecture we will survey (uniform) random number generation - the fundamental ingredient in just 
# about every sampling method, and build upon this to generate draws from more and more complex
# distributions.


# Random number generation - Linear Congruential Generator
# ===

# - Typically computers DO NOT generate `true` random numbers.
# - Instead, they use pseudorandom numbers that we hope are indistinguishable
#   from `true` random numbers.

# - **A linear congruential generator (LCG)** is a simple algorithm that yields a sequence of
# pseudo-randomized numbers calculated with a discontinuous piecewise linear equation.

{
# \[
#   \begin{cases}
#   X_1 = seed\\
#   X_{i}=\left(aX_{i-1}+c\right)~~{\bmod {~}}~m&& \forall i  >1
#   \end{cases}\\
#   \hspace{6pt}\\
#   \text{Where}\\
#   m \hspace{6pt}\text{  s.t.  }\hspace{6pt} 0 < m  \hspace{6pt}\text { is the "modulus"}\\
#   a \hspace{6pt}\text{  s.t.  }\hspace{6pt} 0 < a < m \hspace{6pt}\text{is the "multiplier"}\\
#   c \hspace{6pt}\text{  s.t.  }\hspace{6pt} 0 \leq c < m \hspace{6pt}\text{is the "increment"}\\
# \]
}

par(mar = c(0, 0, 0, 0))
par(mfrow = c(1, 1))
plot(imager::load.image("Lecture9_img_LCG.png"), axes = FALSE) 

# Where `a`, `c` and `m` are constants. and `X1, X2, ...` a sequence of pseudorandom numbers.
# `X1` is called the `seed`.

# Example:

seed <- 10

rand_seq <- function(n, seed, a, c, m) {
  X <- numeric(n)
  X[1] <- seed
  for (i in 2:n) {
    X[i] <- (a * X[i - 1] + c) %% m
  }
  return(X)
}


hist(runif(2000))

library(tidyverse)

(example <- rand_seq(n = 20, seed = 10, a = 5, c = 12, m = 16))
ggplot(transform(data.frame(X = example), X = (X / 16))) +
  geom_histogram(aes(x = X, y = ..density..))

# This sequence has period 4.

# Try other numbers for a, c and m:
rand_seq(n = 20, seed = 10, a = 131, c = 7, m = 16) # Period 8
example <- rand_seq(n = 1000, seed = 10, a = 131, c = 7, m = 16)
ggplot(transform(data.frame(X = example), X = (X / 16))) +
  geom_histogram(aes(x = X, y = ..density..))


(example <- rand_seq(n = 15, seed = 10, a = 1664545, c = 1013904223, m = 2^32))
(example <- rand_seq(n = 15, seed = 10, a = 1664545, c = 1013904223, m = 2^32))

(example <- rand_seq(n = 15, seed = 11, a = 1664545, c = 1013904223, m = 2^32))

example <- rand_seq(n = 1000, seed = 10, a = 1664545, c = 1013904223, m = 2^32)
example <- rand_seq(n = 100000, seed = 11, a = 1664545, c = 1013904223, m = 2^32)

ggplot(transform(data.frame(X = example), X = (X / 2^32))) +
  geom_histogram(aes(x = X, y = ..density..), breaks = seq(0, 1, 0.025), color = "white") +
  stat_function(fun = dunif, color = "red")

n <- 1000
plot(example[1:(n-1)], example[2:n], pch = ".")

library(plotly)
plot_ly() %>%
  add_markers(x = example[1:(n-2)], y = example[2:(n-1)], z = example[3:(n)])



# How to distinguish non-randomness?
# ===

# - Look at period.
# - Missing some values.
# - Proper distribution in the limit.
# - Autocorrelation.
# - Gets harder for higher dimensions.


# Random number generation in R
# === 

# - The default method used by R is the Mersenne Twister
#   (https://en.wikipedia.org/wiki/Mersenne_Twister).
# - See `?Random` for more details about random number generation in R.
# - Like many of the basic functions in R, Mersenne Twister is written in C.
# - The function name is `MT_genrand`, which is called from the C function
#   `unif_rand`. Both generate uniformly distributed 
#   values in the interval [0,1). The source code for both can be found at:
#   https://github.com/wch/r-source/blob/master/src/main/RNG.c
#   (https://github.com/wch/r-source is a repository for the source code for R)

# - `unif_rand` is at the heart of the C function `runif` which generates a single random draw 
#   between a minimal value of `a` and a maximal value of `b`. This function can be found at:
#   https://github.com/wch/r-source/blob/f8d4d7d48051860cc695b99db9be9cf439aee743/src/nmath/runif.c

# - The interface between R and C generates an R function called `runif` which allows us to generate `n` 
#   samples from a uniform random variable between the values of `min` and `max`.

#   (this interface is defined in the file
#   https://github.com/wch/r-source/blob/trunk/src/library/stats/src/random.c, 
#   using the macro `DEFRAND2_REAL` and helper function `random2`)

# ````
#   runif(n, min = 0, max = 1)
# ````

# - So, the R function `runif()` generates almost directly the most fundamental random numbers in R.

runif(10)
runif(10)
runif(10)
runif(10)

set.seed(2) 
runif(10)
runif(10)

set.seed(2) 
runif(10)
runif(10)

# Other distributions in R
# ===

# Most distribution that R handles have four functions.
# There is a root name, for example, the root name for the normal distribution is `norm`.
# This root is prefixed by one of the letters:

# - `p` for "probability", the cumulative distribution function (cdf).
# - `q` for "quantile", the inverse cdf.
# - `d` for "density", the density function pdf.
# - `r` for "random", a random number generator from specified distribution.

# Some built-in distributions
# ===

# - Beta, Binomial, Cauchy, Chi-Squared, Exponential, F, Gamma, Geometric, Hypergeometric,
# Logistic, Log Normal, Negative Binomial, Normal, Poisson, Student t, Studentized Range,
# Uniform, Weibull, Wilcoxon Rank Sum Statistic, Wilcoxon Signed Rank Statistic.

# Whose respective `root` names are:
# - `beta`, `binom`, `cauchy`, `chisq`, `exp`, `f`, `gamma`, `geom`, `hyper`,
# `logis`, `lnorm`, `nbinom`, `norm`, `pois`, `t`, `tukey`,
# `unif`, `weibull`, `wilcox`, `signrank`.

# - For example, the exponential distribution has `pexp`, `qexp`, `dexp`, and `rexp`:

# https://github.com/wch/r-source/blob/5a156a0865362bb8381dcd69ac335f5174a4f60c/src/nmath/sexp.c

# ```
# dexp(x, rate = 1, log = FALSE)
# pexp(q, rate = 1, lower.tail = TRUE, log.p = FALSE)
# qexp(p, rate = 1, lower.tail = TRUE, log.p = FALSE)
# rexp(n, rate = 1)
# ```

ggplot(data.frame(s = rexp(5000))) +
  geom_histogram(aes(x = s, y = ..density..), color = "white", breaks = seq(0, 10, 0.25)) +
  stat_function(fun = dexp, col = "red")

ggplot(data.frame(s = pexp(rexp(5000), lower.tail = TRUE))) +
  geom_histogram(aes(x = s, y = ..density..), color = "white", breaks = seq(0, 1, 0.05)) +
  stat_function(fun = dunif, col = "red")


# Sampling (With Replacement) from Discrete Distributions
# ===

# 1. Finite Discrete Distributions

# Example: Single 0/1 Coin Toss (Bernoulli)
# P(Coin = 1) = p = 1 - P(Coin = 0)

p <- 1/3


as.integer(runif(1) < p)
as.integer(runif(1) < p)

# Exercise:
# Write your own Binomial(n, p) sampler (sum of successes in n coin tosses)

my_binom <- function(n, p = 0.5) {
  u <- runif(n)
  # u[1] < p, u[2] < p, ..., u[n] < p
  return(sum(u < p))
}

my_binom(1000)
my_binom(1000, 1/3)

set.seed(6)
rbinom(1, 3, 0.5)
runif(3)

set.seed(6)
runif(6)


# This method can be extended to any finite discrete distribution with a vector of probabilities `p`.

# Suppose that X takes values in S = {1, 2, 3, ..., n} with probability mass function defined by
# the following table:

#     p_1   p_2   p_3 ... p_n
# x    1     2     3  ...  n

# To generate draws from this distribution we partition [0,1) into `n` sub-intervals:
# [0, p_1), [p_1 , p_1 + p_2), [p_1 + p_2, p_1 + p_2 + p_3), ...,
#    [p_1 + p_2 + ..., p_{n-1}, p_1 + ... + p_n),
# generate a Uniform(0, 1), and check which interval the variable falls into.
# If it falls in the first, return X = 1,
# if second, return X = 2, etc.

# Exercise:

# Write a function that generates samples from an arbitrary finite discrete distribution
# whose PMF is given by a vector of probabilities. Hint: Use `cumsum` and either use a
# `for` loop, or play with `which()`, `min()` and `sapply()`

# Test your function with a fair die (`P = rep(1/6, 6)`), `geom_bar` and `mean()`

sample_discrete_for <- function(n, P) {
  P_cum <- 
  X <- numeric(n)
  u <- runif(n)
  
  for (i in 1:n) {
    for (j in 1:length(P)) {
      if ("") {
        X[i] <- ""
        break()
      }
    }
  }
  
  return( X )
}

P <- rep(1/6, 6)
s <- sample_discrete_for(10000, P)

ggplot(data.frame(s)) +
  geom_bar(aes(s))
mean(s)

P <- c(1/2, 1/4, 1/8, 1/16, 1/32, 1/32)
s <- sample_discrete_for(10000, P)

ggplot(data.frame(s)) +
  geom_bar(aes(s))

# A "vectorized" version:

sample_discrete <- function(n, P) {
  P_cum <- ""
  u <- ""
  return( sapply(u, function(u) {""} ))
}

P <- rep(1/6, 6)
s <- sample_discrete(1000, P)

ggplot(data.frame(s)) +
  geom_bar(aes(s))
mean(s)

# This kind of sampling in done efficiently in R with the function `sample.int` (which
# is based on C code)

microbenchmark::microbenchmark(
  sample_discrete_for(1000, P),
  sample_discrete(1000, P),
  sample.int(n = 6, size = 1000, prob = P, replace = TRUE)
)



# Sampling from countably infinite discrete distributions
# ===

# We can also sample from countably infinite discrete distributions whose PMF is given in
# a functional form. 

# - For numerical stability, better to work with CDF function.

# Start by writing a function that generates a single random draw:

sample_discrete_fn_1 <- function(f, ...) {
  u <- runif(1)

  j <- 0
  cum_p <- ""
  
  repeat {
    if ("") {
      X <- ""
      break()
    } else {
      ""
      ""
    }
  }

  return( X )
}

# Try it with, e.g., Poisson:

{
# $\!f(k;\lambda )=\Pr(X=k)={\frac {\lambda ^{k}e^{-\lambda }}{k!}}$
}

# f(k; lambda) = P(X = k) = lambda^k * e^{-lambda} / k!

pois_cdf <- function(k, lambda) {
  k_seq <- seq(0, k, by = 1)
  return( sum( exp(-lambda) * (lambda ^ k_seq) / factorial(k_seq) ) )
}


sample_discrete_fn_1(pois_cdf, lambda = 5)

#  And generalize the function to generate `n` Poisson draws:

sample_discrete_fn <- function(n, f, ...) {
  ""
}

sample_discrete_fn(1000, pois_cdf, lambda = 5)

n <- 100000
lambda <- 5

s <- sample_discrete_fn(n, pois_cdf, lambda)
ggplot(data.frame(s = s), aes(s)) +
  geom_bar(color = "white") +
  geom_line(aes(y = n * dpois(s, lambda)), color = "red") +
  geom_point(aes(y = n * dpois(s, lambda)), color = "red")
  
# That was a bit slow...
microbenchmark::microbenchmark(
  sample_discrete_fn(1000, pois_cdf, lambda = 5),
  rpois(1000, 5)
)

# https://github.com/wch/r-source/blob/f8d4d7d48051860cc695b99db9be9cf439aee743/src/nmath/rpois.c


# Inverse Transform Sampling
# ===

# Inverse transform sampling (also known as inversion sampling, the inverse probability
# integral transform, the inverse transformation method, Smirnov transform, universality
# of the uniform, quantile transform method or the golden rule) is a basic method for
# pseudo-random number sampling, i.e. for generating sample numbers at random from
# any probability distribution given its cumulative distribution function.

{
# Given $U \sim \textrm{Uniform}(0,1)$ and cdf $F$ from a continuous distribution.
# Then $X = F^{-1}(U)$ is a random variable with cdf $F$.

# Proof: Consider a monotone function T:$[0,1]\rightarrow R$ such that $T(U)=X$.
# \[
# F(x)=P(X\le x) = P (T(u) \le x) = P (u \le T^{-1}(x)) = T^{-1}(x)=u
# \]
# Therefore, $x=F^{-1}(u).$
}
  
# - $F^{-1}$ is the quantile function. (Quantile function associated with a probability distribution 
#   of a random variable, specifies the value of the random variable such that the probability of the
#   variable being less than or equal to that value equals the given probability. It is also called the
#   percent-point function or inverse cumulative distribution function.)
# - If we can generate uniforms and calculate quantiles, we can generate non-uniforms.

# Example: Exponential
# ===

{
  # Suppose $X \sim \text{Exp}(\lambda)$.  Then we have density:
  #   \[
  #     f(x) = \beta e^{-\lambda x}
  #     \]
  # and cdf
  # \[
  #   F(x) = 1 - e^{-\lambda x}
  #   \]
  # 
  # The quantile function isfor Exponential($\lambda$) is derived by finding the value of $Q$ for which $1-e^{-\lambda Q}=p$:
  # 
  #   $Q(p) = -\frac{\log(1-p)}\lambda$
  # 
  #   So if $U \sim \text{Uniform}(0,1)$, then $F^{-1} (U) = -\frac{\log(1-U)}\lambda \sim \text{Exp}(\lambda)$.
}

par(mar = c(0, 0, 0, 0))
plot(imager::load.image("Lecture9_img_inv_exp.png"), axes = FALSE) 

lambda <- 3
U <- runif(10000)
y <- ""

par(mar = c(4, 4, 1, 1))
hist(y, freq = FALSE, breaks = 50)
curve(dexp(x, rate = lambda), add = TRUE, col = "red", from = 0.01)

# Alternatively, we can use the built-in quantile function:
z <- qexp(U, lambda)
hist(z, freq = FALSE, add = TRUE, col = rgb(1, 0, 0, 0.2), breaks = 50)


# Once we start accumulating distributions we can sample from we can continue to sample
# from other based on probabilistic properties.

# Gamma(n, beta)
# ===

# For example, the gamma(alpha, beta) distribution when alpha is a positive integer
# can be expressed as a sum of iid exponential draws.

{
# - Remember that if $X_1, \dots, X_n$ are IID $\text{Exp}(\beta)$, then $\sum_{i=1}^n X_i \sim \Gamma(n, \beta)$.
# - Hence if we need a $\Gamma(\alpha, \beta)$ random variate and $\alpha \in \{ 1, 2, \dots \}$, then take $U_1, \dots, U_{\alpha}$ IID $\text{Uniform}(0,1)$ and set
# \[
# \sum_{i=1}^{\alpha} - \frac{\log(1-u_i)}{\beta} \sim  \Gamma(\alpha, \beta).
# \]
}
  

# So for one draw:
n <- 5
beta <- 5
U <- runif(n)
y <- sum(- log(1 - U) / beta)

# And for multiple draws we can, e.g.:
N <- 10000
U <- matrix(runif(n * N), ncol = N)
s1 <- apply(U, 2, function(U) { sum(- log(1 - U) / beta) })

hist(s1, freq = FALSE)
hist(rgamma(N, n, beta), add = TRUE, col = rgb(1, 0, 0, 0.2), freq = FALSE)

# Of course, the fact that something is easy does not necessarily make it optimal.

microbenchmark::microbenchmark(
  {
    U <- matrix(runif(n * N), ncol = N)
    s1 <- apply(U, 2, function(U) { sum(- log(1 - U) / beta) })
  }, {
    U <- matrix(runif(n * N), ncol = N)
    s1 <- apply(U, 2, qexp, rate = beta)
  }, {
    rgamma(N, n, beta)
  }
)


# Challenges in Inverse Transform Sampling
# ===
# - Quantile functions often don't have closed form solutions or even nice numerical solutions.
# - But we know the probability density function — can we use that?

# Acceptance-Rejection Sampling
# ===

# Acceptance-Rejection sampling is a basic technique used to generate observations from a distribution. 
# It is also commonly called the acceptance-rejection method or accept-reject algorithm, and is a type
# of exact simulation method. The method works for any distribution in $\mathbb {R} ^{m}$ with a density.

# See Acceptance-Rejection.pdf


# Illustration:

# 1. Sample target beta with truncated normal proposal

# Suppose that we wish to sample from a Beta(3, 6) distribution:
alpha <- 3
beta <- 6

make_f_beta <- function(alpha, beta) {
  return( 
    function(x) {
      return( dbeta(x, alpha, beta) )
    }
  )
}

f <- make_f_beta(3, 6)

par(mfrow = c(1, 1))
curve(f, ylab = "Density", ylim = c(0, 3.5), lwd = 2)

x_seq <- seq(0, 1, by = 0.01)
df <- data.frame(x = x_seq, Density = f(x_seq))
plt <- ggplot(df) +
    geom_line(aes(x = x, y = Density), lwd = 1.2)

# And use the truncated normal with a = 0, b = 1, mean = 0, sd = 1 distribution as a proposal g:
make_g_truncnorm <- function(a, b, mean, sd) {
  return( 
    function(x) {
      return( truncnorm::dtruncnorm(x, a, b, mean, sd) )
    }
  )
}

a <- 0; b <- 1; mean <- 0; sd <- 1
g <- make_g_truncnorm(a, b, mean, sd)

curve(g, add = TRUE, col = "blue", lwd = 2)
plt <- 
    plt +
    stat_function(fun = g, color = "blue", lwd = 1.2)

# Want: sup_x f(x) / g(x) < oo

f_g_ratio <- function(x) { return ( f(x) / g(x) ) }
curve(f_g_ratio, add = TRUE, lty = 2, lwd = 1, col = rgb(0.2, 0.2, 1, 0.8))

plt <- 
    plt +
    stat_function(fun = f_g_ratio, color = rgb(0.2, 0.2, 1, 0.8), lty = 2)

f_g_optimize <- optimize(f_g_ratio, interval = c(0, 1), maximum = TRUE)



# And so the maximum is at:
points(f_g_optimize$maximum, f_g_optimize$objective, pch = 20, col = rgb(0.2, 0.2, 1, 0.8))

plt <- 
    plt +
    geom_point(aes(x = f_g_optimize$maximum, y = f_g_optimize$objective), color = rgb(0.2, 0.2, 1, 0.8))

# Hence we'd choose M to be the maximal value of the ratio of densities
# (adding something small to make sure we are above the max, here adding 0.1 for ease
# of illustration):
M <- f_g_optimize$objective + 0.1

#  Hueristically, it is as if we "blow up" g(x) by M:
curve(M * g(x), col = rgb(0, 0, 1), add = TRUE, lwd = 2, lty = 2)

plt <- 
    plt +
    stat_function(fun = function(x) { M * g(x) }, color = rgb(0, 0, 1), lwd = 1.2, lty = 2)

# And then the following the algorithm:
# 1. sample u ~ Uniform(0,1)
# 2. Sample X ~ exp(1) 
# 3. if u <= f(X) / (M * g(X)), accept X

# Translates to a dart throwing game!

# Throw a dart by drawing u ~ U and X ~ g:
{
u <- runif(1)
X <- truncnorm::rtruncnorm(1, a, b, mean, sd)

# Plot it at (X, u * M * g(X)):
points(X, u * M * g(X), col = rgb(0, 0, 0, 0.5))
plt <- 
    plt +
    geom_point(aes(x = X, y = u * M * g(X)), color = rgb(0, 0, 0, 0.5), pch = 1)
}

# Accept it if it is below the density curve of f:
{
accept <- (u * M * g(X) <= f(X))
if (accept) {
  col = rgb(0, 0, 0, 0.4)
} else {
  col = rgb(1, 0, 0, 0.4)
}
points(X, u * M * g(X), pch = 20, col = col)
# plt <- 
#     plt +
#     geom_point(aes(x = X, y = u * M * g(X)), color = col)
}

n <- 10000
n_accept <- 0
S <- NULL
for (i in 1:n) {
  u <- runif(1)
  X <- truncnorm::rtruncnorm(1, a, b, mean, sd)
  
  accept <- (u * M * g(X) <= f(X))
  if (accept) {
    col = rgb(0, 0, 0, 0.4)
    n_accept = n_accept + 1
    S <- c(S, X)
  } else {
    col = rgb(1, 0, 0, 0.4)
  }
  points(X, u * M * g(X), pch = 20, col = col, cex = 0.5)
}


# Or vectorized:
{
n <- 10000
res <- data.frame(u = numeric(n), X = numeric(n), y = numeric(n), accept = logical(n))
res$u <- runif(n)
res$X <- truncnorm::rtruncnorm(n, a, b, mean, sd)
res$y <- res$u * M * g(res$X)
res$accept <- res$y <= f(res$X)
plt +
  geom_point(data = res, aes(x = X, y = y, color = accept)) +
  scale_colour_manual(values = c(rgb(1, 0, 0, 0.4), rgb(0, 0, 0, 0.4))) +
  theme(legend.position="none")
}


# Did we get the expected acceptance probability?
(acceptance_ratio <- n_accept / n)
1 / M

# Does our sample look like a beta distributed random variable?
hist(S, freq = FALSE, col = rgb(0, 0, 0, 0.2))
curve(f, add = TRUE, lwd = 2)


# 2. Sample t as target and normal as proposal.
df <- 30
curve(dt(x, df), from = -2, to = 2, ylim = c(0, 0.4))
curve(dnorm(x), from = -2, to = 2, col = "red", add = TRUE)

t_norm_ratio <- function(x) { return(dt(x, df) / dnorm(x)) }
t_norm_optimize <- optimize(t_norm_ratio, interval = c(-2, 2), maximum = TRUE)

# Check the ratio:
curve(t_norm_ratio, lty = 2, lwd = 1, col = rgb(0.2, 0.2, 1), from = -2, to = 2)

# optimize() doesn't check the boundaries!
t_norm_ratio(2) > t_norm_optimize$objective

# So can we take M <- t_norm_ratio(2) ?

# Answer:

# No!

# Even though the distributions look very similar:
curve(dt(x, df), from = -5, to = 5, ylim = c(0, 0.4))
curve(dnorm(x), from = -5, to = 5, col = "red", add = TRUE)

# Their ratio tells us that the tails behave radically different:
curve(t_norm_ratio, lty = 2, lwd = 1, col = rgb(0.2, 0.2, 1), from = -5, to = 5)

# Zoom in:
curve(dt(x, df), from = 4, to = 5)
curve(dnorm(x), from = 4, to = 5, col = "red", add = TRUE)

# As a matter of fact, the tail of the t-distribution decays polynomially.
# The tail of the normal decays exponentially. The ratio over the entire real
# line is unbounded, so we cannot use the normal as a proposal for the target t.


# Example: Gamma
# === 
# - Suppose we want to simulate $X \sim \Gamma(3/2, 1)$ with density:
#   $f_x(x)=\frac{2}{\pi}\sqrt{x}e^{-x}$
# 
# - Can use the accept-reject algorithm with a $\Gamma(n,1)$ and
  # $n \in \{ 1, 2, \dots \}$ since we know how to simulate this.
# 
# - Then we have
# \[
# \begin{aligned}
# M & = \sup_{x>0} \frac{f_x(x)}{f_y(x)}\\
#   & = \sup_{x>0} \frac{\frac{2}{\pi} \sqrt{x} e^{-x}}{\frac{1}{(n-1)!} x^{n-1} e^{-x}}\\
#   & = \frac{2 (n-1)!}{\pi} \sup_{x>0} x^{-n+3/2} = \infty
# \end{aligned}
# \]
# since
# \[
# n < 3/2 \text{ implies } x^{-n+3/2} \to \infty \text{ as } x \to \infty,
# \]
# and 
# \[
# n > 3/2 \text{ implies } x^{-n+3/2} \to \infty \text{ as } x \to 0.
# \]
# 
# - Hence, we need to be a little more creative with our proposal distribution.
# - We could consider a mixture distribution.  That is, if $f_1(z)$ and $f_2(z)$
#   are both densities and $p \in [0,1]$.  Then
# \[
# p f_1(z) + (1-p) f_2(z)
# \]
# is also a density.
# - Consider a proposal that is a mixture of a $\Gamma(1,1) = \text{Exp}(1)$ and a $\Gamma(2,1)$, i.e.
# \[
# f_y(y) = \left[ p e^{-y} + (1-p) y e^{-y} \right] I(0 < y < \infty). 
# \]
# 
# Now, we have
# \[
# \begin{aligned}
# M & = \sup_{x>0} \frac{f_x(x)}{f_y(x)}\\
#   & = \sup_{x>0} \frac{\frac{2}{\pi} \sqrt{x} e^{-x}}{p e^{-x} + (1-p) x e^{-x}}\\
#   & = \frac{2}{\pi} \sup_{x>0} \frac{ \sqrt{x}}{p + (1-p) x} \\
#   & = \frac{2}{\pi} \frac{1}{2 \sqrt{p(1-p)}}
# \end{aligned}
# \]
# 
# Prove the last line, i.e. maximize $h(x) = \frac{ \sqrt{x}}{p + (1-p) x}$ for $x>0$ or $\log h(x)$.
# 
# - Note that $M$ is minimized when $p=1/2$ so that $M_{1/2} = 2 / \sqrt{\pi} \approx 1.1283$.
# - Then the accept-reject algorithm to simulate $X \sim \Gamma(3/2, 1)$ is as follows:
# 1. Draw $Y \sim f_y$ with
# \[
# f_y(y) = \left[ p e^{-y} + (1-p) y e^{-y} \right] I(0 < y < \infty) 
# \]
# and and independently draw $U \sim \text{Uniform}(0,1).$
# 2. If 
# \[
# u < \frac{2}{\sqrt{\pi}} \frac{f_x(y)}{f_y(y)}=\frac{2 \sqrt{y}}{1+y}
# \]
# set $X=Y$; otherwise return to 1.
# 
# Simulating from Mixtures
# ===
# - Write $f(z) = p f_1(z) + (1-p) f_2(z)$ as the marginal of the joint given by
# \[
# f(z | w) = f_1(z) I(w=1) + f_2(z) I(w=0)
# \]
# where $W \sim \text{Binomial}(1,p)$.
# - Thus to simulate from $f(z)$.
# 1. Draw $U \sim \text{Uniform}(0,1)$.
# 2. If $u < p$ take $Z \sim f_1(z)$; otherwise take $Z \sim f_2(z)$.
# - Exercise: Show $Z \sim f(z)$.
 
# Example: Gamma
# ===

ar.gamma <- function(n = 100) {
  x <- numeric(n)
  i <- 1
  while(i < (n+1)) {
  	u <- runif(1)
  	if(u < .5) {
  		y <- -1 * log(1-runif(1))
  	} else {
  		y <- sum(-1 * log(1 - runif(2)))
  	}
  	u <- runif(1)
  	temp <- 2 * sqrt(y) / (1+y)
  	if(u < temp){
  		x[i] <- y
  		i <- i+1
  	}
  }
  return(x)
}

x <- ar.gamma(10000)

hist(x, freq = FALSE, breaks = 50, xlab = "x", ylab = "f(x)", main = "Histogram and Theoretical")
curve(dgamma(x, 3/2, 1), col = "red", lwd = 2, add = TRUE)

# Box-Muller
# ===

# - __Box-Muller transformation__ transform generates pairs of independent, standard normally distributed
#   random numbers, given a source of uniformly distributed random numbers.
# - Let $U \sim \text{Uniform}(0,1)$ and $V \sim \text{Uniform}(0,1)$ and set:
# $$
# R=\sqrt{-2\log U} \hspace{10mm} \textrm{ and } \hspace{10mm} \theta = 2\pi V
# $$
# - Then the following transformation yields two independent normal random variates:
# $$
# X=R\cos(\theta) \hspace{10mm} \textrm{ and } \hspace{10mm} Y=R\sin(\theta)
# $$



# Summary
# ===
# - We can transform uniform draws into other distributions when we can compute the distribution function:
#   + Sampling from discrete distributions by benchmarking a uniform draw vs. the PMF.
#   + Inverse Transform Sampling when we can invert the cdf.
#   + Rejection Sampling if all we have is the density.
#   + Box-Muller Sampling to generate two independent standard normal variables.  