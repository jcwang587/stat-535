# Introduction to model fitting =====

# The goal of a model is to provide a simple low-dimensional summary of a dataset. 
# E.g. instead of n data points (n-dimensional object) a vector of p parameters
# (p-dimensional, assuming that p<<n).

# In this introduction we will focus on models that fit a curve to data.

# There are two parts to such a model:
  
# - a family of models that express a precise pattern that you want to capture. 
#   For example, the pattern might be a straight line, or a square root curve. 

#  You will express the model family is expressed as an equation like 
#  y = a_1 + a_2 * x
#  or
#  y = a_1 + a_2 * sqrt(x)

#  Here, x and y are known variables from your data, and a1 and a2 are parameters that can
#  vary to capture different patterns.

# - a fitted model by finding the model from the family that is the "closest" to your data.
#  This takes the generic model family and makes it specific, like 
#  y = 3 * x + 7
#  or
#  y = 9 * sqrt(x) + 2


# Lets take a look at the simulated dataset `sim`. 
# It contains two continuous variables, x and y. Let’s plot them to see how they’re related:
sim <- readRDS("sim.rds")

library(tidyverse)
ggplot(sim, aes(x, y)) + 
  geom_point()
  
# You can see a strong pattern in the data. 
# Let’s use a model to capture that pattern and make it explicit.

# We need to supply the basic form of the model.

# In this case, the relationship looks concave. Let's try our luck with y = a1 + a2 * sqrt(x).
# Let’s start by getting a feel for what models from that family look like by randomly generating
# a few and overlaying them on the data.

# Random parameter values:
models <- data.frame(
  a1 = runif(250, -20, 40),
  a2 = runif(250, 0, 5)
)

# Add them to the plot:
plot(sim$x, sim$y, pch = 20, xlab = "x", ylab = "y")
for (i in 1:nrow(models)) {
  lines(0:100, "", col = rgb(0, 0, 0, 0.3))
}

# There are 250 models on this plot, but a lot are really bad!

# We need to find the good models by making precise our intuition that a good model
# is “close” to the data. We need a way to quantify the distance between the data
# and a model. Then we can fit the model by finding the value of a1 and a2 that
# generate the model with the smallest distance from this data.

# One easy place to start is to find the vertical distance between each point and the model.

# This distance is the difference between the y value given by the model (the prediction),
# and the actual y value in the data (the response).

# To compute this distance, we first turn our model family into an R function.
# This takes the model parameters and the data as inputs, and gives values predicted by the model as output:
  
model1 <- function(a1, a2, data) {
  return( "" )
}

# Try it:  
model1(7, 1.5, sim)

# Next, we need some way to compute an overall distance between the predicted and actual values. 
# In other words, the plot above shows 30 distances: how do we collapse that into a single number?
  
# One common way to do this in statistics to use the “root-mean-squared deviation”. We compute
# the difference between actual and predicted, square them, average them, and the take the square root.

measure_distance1 <- function(a1, a2, data) {
  diff <- ""
  return( "" )
}

measure_distance1(7, 1.5, sim)

# We can add a column that measures the distance for all the points using `mapply` (which is an `apply` family function that
# iterates over several parameters at a time):

""

# Or equivalently using `purrr::map2dbl()`
""
""

""


""


# Next, let’s overlay the 10 best models on to the data. Color the models by -dist:
n_top <- 10
best_models <- filter(models, rank(dist) <= n_top) 
scale_best_models <- 
  ( (best_models$dist - min(best_models$dist) ) / 
      ( max(best_models$dist) - min(best_models$dist) )  )

plot(sim$x, sim$y, pch = 20, xlab = "x", ylab = "y")
for (i in 1:n_top) {
  lines(0:100, best_models$a1[i] + best_models$a2[i] * sqrt(0:100),
        col = rgb(0, 0, 1 - scale_best_models[i], 0.9))
}

# We can also think about these models as observations, and visualizing with a scatterplot of a1 vs a2,
# again colored by -dist. We can no longer directly see how the model compares to the data, but we can
# see many models at once. Again, I’ve highlighted the 10 best models, this time by drawing red circles
# underneath them.

ggplot(models, aes(a1, a2)) +
  geom_point(data = filter(models, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist))

# Instead of trying lots of random models, we could be more systematic and generate an evenly spaced grid
# of points (this is called a grid search). 

# We can pick the parameters of the grid roughly by looking at where the best models were in the plot above.

grid <- expand.grid(
  a1 = seq(-5, 20, length = 25),
  a2 = seq(1, 3, length = 25)
) %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, .f = measure_distance1, data = sim))

grid %>% 
  ggplot(aes(a1, a2)) +
  geom_point(data = filter(grid, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist)) 

# When you overlay the best 10 models back on the original data, they all look pretty good:

n_top <- 10
best_models <- filter(grid, rank(dist) <= n_top) 
scale_best_models <-
  ( (best_models$dist - min(best_models$dist) ) /
      ( max(best_models$dist) - min(best_models$dist) )  )

plot(sim$x, sim$y, pch = 20, xlab = "x", ylab = "y")
for (i in 1:n_top) {
  lines(0:100, best_models$a1[i] + best_models$a2[i] * sqrt(0:100),
        col = rgb(0, 0, 1 - scale_best_models[i], 0.9))
}

# We could imagine iteratively making the grid finer and finer until you narrowed in on the best model.
# But there’s a better way to tackle that problem: a numerical minimization tool called Newton-Raphson
# search.

# The intuition of Newton-Raphson is pretty simple: you pick a starting point and look around for the
# steepest slope. You then ski down that slope a little way, and then repeat again and again, until you
# can’t go any lower. In R, we can do that with optim():

measure_distance1_helper <- function(a, data) {
  return( measure_distance1(a[1], a[2], data) )
}

best1 <- optim(c(0, 0), measure_distance1_helper, data = sim)
best1$par

ggplot(sim, aes(x, y)) +
  geom_point(size = 2, colour = "grey30") +
  geom_function(fun = function(x) "", color = "blue")

# If you have a function that defines the distance between a model and a dataset, an algorithm that can minimize
# that distance by modifying the parameters of the model, you can find the best model.

# This approach will work for any family of models that you can write an equation for.

# Exercise: 
# Repeat the optim step for the family of models a1 + a2 * (x ^ a3) and the same data sim.
# Compare it to the previous model, which is a better fit?

# Is the optimizer sensitive to initial values?

# Rewrite the `optim` steps with a different a distance measure for both methods: absolute difference.

# One of the two models we tried is actually ...
# Show it!