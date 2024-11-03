# Agenda
# ===
# - Basics of optimization 
# - Gradient descent method

# Examples of Optimization Problems 
# ===

# - Minimize mean-squared error of regression. (Gauss, c. 1800) 
# - Maximize likelihood of distribution. (Fisher, c. 1918) 

# Optimization Problems 
# ===

par(mar = c(0, 0, 0, 0))
plot(imager::load.image("Lecture8-2_img_optim.png"), axes = FALSE)
plot(imager::load.image("Lecture8-2_img_calc_grad.png"), axes = FALSE)

# Gradient Descent - Algorithm
# ===

# 1. Start with an initial guess for theta, call it theta0, step-size eta. 
# 2. Repeat:

# 1. Start with an initial guess for theta, call it theta(0), step-size eta. 
# 2. For every i >= 0, do:
  
  # - Find the gradient of f at theta(i) 
  # - If the gradient of f at theta(i) is "close enough" to the zero vector 
  #   stop and return theta(i) as approximate theta*.
  # - Else, set theta(i+1) = theta(i) - eta * ( gradient of f at theta(i) ) and repeat.


# Pros and Cons of Gradient Descent 
# ===

# + Pros: 
#   - Moves in direction of greatest immediate improvement. 
#   - If `eta` is small enough, gets to a local minimum eventually, and then stops. 
# 
# + Cons: 
#   - "small enough" `eta` can be really, really small. 
#   - Slowness or zig-zagging if elements of the gradient of f are of very different sizes. 


# Implementing the gradient descent method
# ===

# At the heart of the gradient descent method lies the computation of the gradient. 

# We will use here the `numDeriv::grad()` function with its default method.

# Exercise:

# Write a `gradient_descent` function that finds the minimum of an arbitrary function.

# 1. What should be the inputs for the function?

# Answer: 
# f - an arbitrary function
# theta0 - starting point (theta(0))
# step_scale - coefficient of gradient in the descent (eta)

# We define "small enough" to be when the absolute value of all the coordinates of the gradient
# is smaller than some `stopping_partial_deriv`.

# In order to avoid infinite loops, it is recommended to add a maximal number of iterations,
# call it `max_iter`

# In addition, it is advisable to allow passing more arguments to `f` via `...`

# So, the function will look like:

# gradient_descent <- function(f, theta0, step_scale, stopping_deriv, max_iter, ...) {
    ""
    # return("")
# }

# 2. Write pseudo-code.

# 3. Write the function in R
    
# 4. Test it with functions for which you know the minimum (try random values for theta0)


    
    
# - Works equally well whether `f` is mean squared error of a regression, $\psi$ error of
#   a regression, (negative log) likelihood, cost of a production plan, ...

# Exercise:

# Use the gradient descent method to find the estimators for simple linear regression by minimizing the MSE.

# The model is y_i = beta_0 + beta_1 * x_i + epsilon_i

# Try your work on the MASS::cats data set, regressing Heart Weight on Body Weight.
# However, add a fake new last observation that is an outlier:

cats <- readRDS("fewer_cats.rds")

# Tip: when you define the objective function (MSE), don't use `x` as an argument because it will
#      duplicate the `x` argument of `grad()`!

MSE_regression <- function("") {
  return( "" )
}

# Compare your estimates to the least squares estimates obtained by `lm()`


# Exercise:
# Use the `psi` function from Lecture 8-1 for the regression (that is, use 
# mean-psi-error instead of MSE as your objective function), set the additional
# parameter of psi to be `c = 1.2`.

# This is a type of regression that is more robust to outliers,
# because psi(x) <= x^2

psi <- function(x, c = 1) {
  return (
    ifelse(
      abs(x) > c,
      2 * c * abs(x) - c^2, 
      x^2
    )
  )
}

par(mar = c(4, 4, 1, 1))
curve(x^2, from = -5, to = 5)
curve(psi(x, c = 1.2), from = -5, to = 5, add = TRUE, col = "blue")

mean_psi_regression <- function("") {
  return(  )
}

b_psi_GD <-
  gradient_descent(
    f = mean_psi_regression,
    theta0 = c(0, 0),
    step_scale = 0.001,
    stopping_deriv = 1e-4,
    max_iter = 250000,
    Y = cats$Hwt,
    X = cats$Bwt,
    c = 1.2
  )

coef(lm(Hwt ~ Bwt, data = cats))
b_MSE_GD$argmin
b_psi_GD$argmin

par(mar = c(4, 4, 1, 1))
plot(cats$Bwt, cats$Hwt)
abline(a = "", b = "", col = "blue") # MSE fit
abline(a = "", b = "", col = "red") # psi fit

# Compare to the lm coefficients without the outlier:
lm_no_outlier <- lm(Hwt ~ Bwt, data = cats[-nrow(cats) , ])
abline(lm_no_outlier, col = "chartreuse4")

# And so you have written your own Robust Simple Linear Regression!