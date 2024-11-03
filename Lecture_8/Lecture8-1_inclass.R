## Previously we focused on...  
# - Writing our own functions.
# - Dividing labor with multiple functions.
# - Using `apply`, `sapply`, etc., to avoid iteration.


## Agenda
# - Functions are objects in R, just like everything else.
# - Functions can be arguments to other functions and also returned by other functions.


## R Functions That Take Functions as Arguments
# - `apply()`, `sapply()`, `ddply()` etc.: Takes a function and applies it on some objects (rows/columns of 
#    a matrix, elements of a list).
# - `optim()` takes a function and returns a value for its input that minimizes it.
#    I.e. finds argmin_x( f(x) )
# - `curve()`: Evaluate a function over a range, plots the results.


## Anonymous Functions

# - `function()` returns an object of class `function`.
foo <- function() {}
class(foo)

# - So far we've assigned that object to a name.
# - If we don't have an assignment, we get an **anonymous function**.
# - Usually part of some larger expression:

sapply( (-2):2, function(x) { exp(x) / (1 + exp(x)) } )

# - Often handy when connecting other pieces of code, especially in the functions such as
# `apply` and `sapply`.

# - Won't clutter the workspace.
# - Can't be examined or re-used later.

## Functions that take functions as input and return vectors

### Gradient of a Function

# - Problems in statistics sometimes boil down to optimization.  
# - Lots of optimization problems require the gradient of the **objective function**.
# - Gradient of $f: R^n -> R$ at $x$:

# Given 
# $f\colon \mathbf {R} ^{n}\to \mathbf {R}$ 
# its gradient 
# $\nabla f\colon \mathbf {R} ^{n}\to \mathbf {R} ^{n}$
# is given by
# $$
#   \nabla f(x) = \left[\frac{\partial f}{\partial x_1}(x) \,  \ldots \, \frac{\partial f}{\partial x_p}(x)\right]
# $$

par(mar = c(0,0,0,0))
plot(imager::load.image("Lecture8-1_img_gradient.png"), axes = FALSE)

# - We do the same thing to get the gradient of $f$ at $x$ no matter what $f$ is:

# ```
#   find the partial derivative of f with respect to each component of x
#   return the vector of partial derivatives.
# ```

# - It makes no sense to re-write this every time we change $f$.
# Therefore, we write code to calculate the gradient of an arbitrary function.
  
## Writing Our Own Gradient Function

# - Use the simplest possible method: change `x` by some amount, find the difference
#   in `f`, compute the slope.

# - Pseudo-code:

# gradient <- function(function f, numeric x, numeric deriv_steps) {
#   define empty gradient vector

#   coordinate 1:
#     new_x <- x
#     new_x[1] <- ""
#     gradient[1] <- ""

#   coordinate 2:
#     new_x <- x
#     new_x[2] <- ""
#     gradient[2] <- ""

#   similarly for all coordinates

#   return(gradient)
# }


# Exercise: 
# Write your own gradient function using a `for` loop

gradient <- function(f, x, deriv_steps, ...) {
  p <- length(x)
  stopifnot(length(deriv_steps) == p)
  gradient <- numeric(p)
  for (i in 1:p) {
    ""
    ""
    ""
  }
  return(gradient)
}


# Test our function!
# Let $f(x1, x2) = sin(x1) + cos(x2)$. Then $\nabla f = [ cos(x1),  -sin(x2) ]$. 

# First, define $f$:
foo <- function(x) {
  return( sin(x[1]) + cos(x[2]) )
}

# Then, select an $x$ value:
x <- runif(2)

#  Compute the gradient:
gradient(f = foo, x = x, deriv_steps = c(1e-6, 1e-6))

# And compare to the analytical gradient function evaluated at x:
c(""(x[1]), ""(x[2]))

all.equal(
  gradient(f = foo, x = x, deriv_steps = c(1e-6, 1e-6)),
  c(""(x[1]), ""(x[2])),
  tolerance = 1e-5
  )

# Exercise:
# Define $f$ to be some polynomial function. Check that our gradient function works for this function as well. 

""

# - We can potentially use matrix manipulation and `apply`:
gradient_matrix_based <- function(f, x, deriv_steps, ...) {
  p <- length(x)
  stopifnot(length(deriv_steps) == p)
  x_new <- matrix(rep(x, times = p), nrow = p) + diag(deriv_steps, nrow = p)
  f_new <- apply(x_new, 2, f, ...)
  gradient <- (f_new - f(x, ...)) / deriv_steps
  return(gradient)
}

gradient_matrix_based(goo, x, deriv_steps = rep(1e-6, 3))

## To Improve:
# - Our implementation will act badly if `f` is only defined on a limited domain
#   and we ask for the gradient somewhere near a boundary.
# - Forces the user to choose `deriv_steps`.
# - Uses the same `deriv_steps` everywhere, imagine $f(x) = x^2 / sin{x}$.

# As is often the case, there are packages in R that perform computations for us.

## `numDeriv::grad()`

# grad(func, x, ...)

# - Assumes `func` is a function which returns a single floating-point value.
# - Assumes `x` is a vector of arguments to `func`. If `x` is a vector and `func(x)`
#   is also a vector, then it's assumed `func` is vectorized and we get a vector of
#   derivatives.
# - Extra arguments in `...` get passed along to `func`.


# Exercise:
# What is the default method for `grad()`?
# One of the methods for `grad()` computes the gradient in exactly the same way that we did. 
# Which method is that? Show that we get the same results (you might need to use the `method.args`
# parameter).


""


# - Other functions in the package compute the Jacobian of a vector-valued function, and
#   the matrix of second partials (Hessian).

# - Smart and advanced R users know when to use external packages wisely, but also have the
#   gumption to write their own functions when needed (or extract/modify external code according
#   to need).


## Functions That Take Functions or Expressions as Inputs

# Example: `curve()`

# - A call to `curve` looks like this:

# curve(expr, from = a, to = b, n = 101, ...)

# Where `expr` is some expression involving a variable called `x` which is swept
# `from` the value `a` `to` the value `b` in `n` steps, `...` are other plot-control
# arguments.

# - The inputs for `curve` are an expression (which is the name of a function, or
#   a call or an expression written as a function of x) which will evaluate to an
#   object of length n.

# For example:

a <- curve(x^2 * sin(x), from = -10, to = 10)

## Using `curve()` with our own functions

# - If we have defined a function already, we can use it in `curve`:

psi <- function(x, c = 1) {
  return (
    ifelse(
    abs(x) > c,
    2 * c * abs(x) - c^2, 
    x^2
    )
  )
}
curve(psi(x, c = 10), from = -20, to = 20)

# Exercise:
# What is the following line doing?

curve(psi(x = 10, c = x), from = -20, to = 20)

# And what is the following line doing?

curve(psi(x = 10, c = c), from = -20, to = 20, xname = "c")

""


# - If our function doesn't take vectors to vectors, `curve` does not work:

gmp <- read.table("gmp.dat")
head(gmp)

# GMP stands for Gross Metropolitan Product
# PCGMP for Per Capita GMP

# So to compute the population size in a metropolitan area:
gmp$pop <- round(gmp$gmp / gmp$pcgmp)

# Suppose that we think that there is a power law that relates
# population size to PCGMP
# (see https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1852329/ for similar ideas),

# i.e. PCGMP = y0 * (population_size)^a

# Suppose next that we want to plot the curve 
# of the MSE for different `y0` and `a` values.
# The MSE for a general power law is given by:

# Exercise:
# Write an MSE function for a general power law, Y = y0 * N^a

mse_power_law <- function(y0, a, Y, N) {
   return( mean( ( Y - y0 * (N^a) )^2 ) )
}

# Try it:

mse_power_law(y0 = 6611, a = 0.1, Y = gmp$pcgmp, N = gmp$pop)

# Let us use `curve` to plot the MSE over a range of `a` values from 0.1 to 0.15:

curve(
  mse_power_law(a = x, y0 = 6611, Y = gmp$pcgmp, N = gmp$pop),
  from = 0.10,
  to = 0.15
  )

# We get an error message: even though the form is similar to our previous example, `curve` only works 
# where the `x` input is a vector. This isn't the case for our MSE function (it is a scalar function)

# How do we solve this?
  
# - Define a new, vectorized function, say with `sapply`:
a_seq <- seq(from = 0.10, to = 0.15, by = 0.01)
sapply(X = "", FUN = "", y0 = 6611, Y = gmp$pcgmp, N = gmp$pop)

# And then wrap it in a function:
mse_power_law_sapply <- function(a_seq, ...) {
  return( sapply(X = "", FUN = "", ...))
}
mse_power_law_sapply(seq(from = 0.10, to = 0.15, by = 0.01), y0 = 6611, Y = gmp$pcgmp, N = gmp$pop)

# Now we can use this with
curve(
  mse_power_law_sapply(a = x, y0 = 6611, Y = gmp$pcgmp, N = gmp$pop),
  from = 0.10,
  to = 0.20,
  xlab = "x",
  ylab = "MSE"
  )
curve(
  mse_power_law_sapply(a = x, y0 = 5100, Y = gmp$pcgmp, N = gmp$pop),
  add = TRUE,
  col = "blue"
  )

# - Alternate strategy: `Vectorize()` returns a new, vectorized function, in the same
#   fashion that we used above. However, it is a little more robust and allows multiple 
#   arguments to be vectorized easily:
mse_power_law_vec <- Vectorize(mse_power_law, vectorize.args = c("y0", "a"))
mse_power_law_vec(a = seq(from = 0.10, to = 0.15, by = 0.01), y0 = 6611, Y = gmp$pcgmp, N = gmp$pop)
mse_power_law_vec(a = 1/8, y0 = c(5000, 6000, 7000), Y = gmp$pcgmp, N = gmp$pop)


curve(
  mse_power_law_vec(a = x, y0 = 6611, Y = gmp$pcgmp, N = gmp$pop),
  from = 0.10,
  to = 0.20,
  xlab = "a",
  ylab = "MSE"
  )
curve(
  mse_power_law_vec(a = x, y0 = 5100, Y = gmp$pcgmp, N = gmp$pop),
  add = TRUE,
  col = "blue"
  )


## Example: surface function

# - `curve()` takes an expression and plots a 1-D curve by sweeping over `x`.

# - We may want to plot by sweeping over two variables, having `z` computed as a function of
#   `x` and `y` at an expression or function we provide.

# We can build on the built-in plotting function `contour()`:
# contour(x,y,z, [[other stuff]])

# where `x` and `y` are vectors of coordinates, `z` is a matrix
# of the corresponding shape. (see `help(contour)` for graphical options.)

# - Strategy: `contour_fn()` should make `x` and `y` sequences, evaluate the expression
#   at each combination to get `z`, and then call `contour`.

## First version: `contour_fn()`

# Ingredients:
(g <- expand.grid(x = 1:3, y = 5:8))
mapply(FUN = `*`, g$x, g$y)

# - Only works with vector-to-number $\mathbf{R}^2 -> \mathbf{R}$ functions:

# Exercise: Complete the following function:

contour_fn <- function(f, from_x = 0, to_x = 1, from_y = 0, to_y = 1, n_x = 101, n_y = 101, ...) {
  x_seq <- seq(from = "", to = "", length.out = "")
  y_seq <- seq(from = "", to = "", length.out = "")
  plot_grid <- expand.grid(x = "", y = "")
  z_values <- mapply("", "", "")
  z_matrix <- matrix(z_values, nrow = n_x) # What is going on in this line?
  contour(x = x_seq, y = y_seq, z = z_matrix, ...)
  return(invisible(list(x = x_seq, y = y_seq, z = z_matrix)))
}

# We can call our function with anonymous and named functions:
contour_fn(function(x, y) {x + abs(y^2)}, from_x = -1, from_y = -1)

xy_fn <- function(x, y) {x + abs(y^2)}
contour_fn(xy_fn, from_x =-1, from_y = -1, col = "red")


## Expressions and Evaluation

# - Recall our first example with `curve()`:
  # curve(x^2 * sin(x), from = -10, to = 10)

# -  I.e. `curve` doesn't require us to write a function every time --- what's its trick?

# -  **Expressions** are just another class of R objects, they can be created and manipulated.

# - One manipulation is **evaluation**.

# eval(expr, envir)

# Evaluates the expression `expr` in the environment `envir`, which can be a data frame
# or even just a list.

# -  When we type something like `x^2 + y^2` as an argument to `contour_fn`, R tries to evaluate it directly.

# - `substitute` returns the _unevaluted_ expression.

# - `curve` uses first `substitute(expr)` and then `eval(expr,envir)`, having made the right `envir`.


# Example:
unevaluated_expression <- substitute(x + y^2)
unevaluated_expression

eval(unevaluated_expression, envir = list(x = 5, y = 3))
eval(unevaluated_expression, envir = list(x = c(1, 2), y = c(1, 2)))

grid <- 
  expand.grid(
    x = seq(from = 0, to = 1, length.out = 101),
    y = seq(from = 0, to = 1, length.out = 101)
    )

eval(unevaluated_expression, envir = grid)

## Second version: `contour_expr()`

# Exercise:
# Use the above functions to modify our contour_fn function to a new function
# called contour_expr that takes expressions as inputs

contour_expr <- function(expr, from_x = 0, to_x = 1, from_y = 0, to_y = 1, n_x = 101, n_y = 101, ...) {
  x_seq <- seq(from = from_x, to = to_x, length.out = n_x)
  y_seq <- seq(from = from_y, to = to_y, length.out = n_y)
  plot_grid <- expand.grid(x = x_seq, y = y_seq)
  unevaluated_expression <- ""
  z_values <- eval("", envir = "")
  z_matrix <- matrix(z_values, nrow = n_x)
  contour(x = x_seq, y = y_seq, z = z_matrix, ...)
  invisible(list(x = x_seq, y = y_seq, z = z_matrix))
}

# Try it!
contour_expr(x + abs(y^2), from_x = -1, from_y = -1, col = "blue")

# But...
ggg <- function(x, y) { x + abs(y^2) }
contour_expr(ggg, from_x = -1, from_y = -1)

# Third version: Handle Both Functions and Expressions by Converting Expressions to Functions

# We can utilize the strong `rlang::new_function()` function:`

contour_fn_expr <- function(expr, from_x = 0, to_x = 1, from_y = 0, to_y = 1, n_x = 101, n_y = 101, ...) {
  if (is.function("")) {
    return(contour_fn(expr, from_x = "", to_x = "", from_y = "", to_y = "", n_x = "", n_y = "", ...))
  } else {
    expr <- substitute(expr)
    f <- rlang::new_function(rlang::exprs(x = , y = ), expr)
    return(contour_fn(f, from_x = "", to_x = "", from_y = "", to_y = "", n_x = "", n_y = "", ...))
  }
}

contour_fn_expr(x + abs(y^2), from_x = -1, from_y = -1, col = "red")
contour_fn_expr(function(x, y) { x + abs(y^2) }, from_x = -1, from_y = -1, col = "blue")
contour_fn_expr(ggg, from_x = -1, from_y = -1, col = "green")

## Functions of Functions: Mathematically

par(mar = c(0, 0, 0, 0))
plot(imager::load.image("Lecture8-1_img_fns_of_fns1.png"), axes = FALSE)
plot(imager::load.image("Lecture8-1_img_fns_of_fns2.png"), axes = FALSE)


## Returning Functions in R

# A function has three parts:

# - formals(), a list of arguments that control how you call the function.

# - body(), the code inside the function.

# - environment(), the data structure that determines how the function finds
#   the values associated with the names. 


# `formals`` and `body`` are specified explicitly when you create a function, the environment
# is specified implicitly, based on where you defined the function. A function takes values
# for all names which aren't its arguments from the environment where it was defined, not
# the one where it is called. The function environment always exists, but it is only printed
# when the function isn't defined in the global environment.


## Returning Functions: A trivial example

# Functions can be return values like anything else.

make_circumference_fn <- function(ratio_to_diameter = pi) {
  circumference <- function(d) {
    return(ratio_to_diameter * d)
  }
  return(circumference)
}


circ3 <- make_circumference_fn(3)
circ3(10)

formals(circ3)
body(circ3)
environment(circ3)
ls(environment(circ3))
environment(circ3)$circumference  
# (bytecode is a pointer to a compiled version of the body of the function)
environment(circ3)$ratio_to_diameter

circ4 <- make_circumference_fn(4)
environment(circ4)
ls(environment(circ4))
environment(circ4)$circumference
environment(circ4)$ratio_to_diameter

## Returning a function: a bad example
# To exemplify the scoping behavior in R, let's look at the following function:

bad_fn <- function(x) {
  return( x + y )
}

# We haven't defined `y` within `bad_fn`, but still there is no error message.
# We only get an error message when we call the function:

bad_fn(10)

# But, if y was defined in the global environment, R takes that value:
y <- 100
bad_fn(10)

y <- 1000
bad_fn(10)

# Although convenient for casual scripting, this behavior is typically not recommended
# for your functions, especially if those serve as an output from another function.

## Returning a function: a Less Trivial Example

# Create a linear predictor, based on values of two variables.

# Ingredients:
# to perform a simple linear regression:
#  Make some data:
x <- runif(30)
y <- 2 + 8 * x + rnorm(30)
par(mar = c(3, 3, 1, 1))
plot(x, y)
#  Fit a simple linear regression (y_i = beta_0 + beta_1 * x_i + epsilon_i):
lm(y ~ x)
lm_fit <- lm(y ~ x)

# Note
lm_fit$model

# stores the original data in the output. We can use `lm` with the option `model = FALSE` 
# to negate that:
lm_fit <- lm(y ~ x, model = FALSE)
lm_fit$model

beta0 <- coef(lm_fit)[1]
beta1 <- coef(lm_fit)[2]
abline(a = beta0, b = beta1)
# Predict points on curve given the same x values:
beta0 + beta1 * x
points(x, beta0 + beta1 * x, pch = 20)
segments(x, y0 = y, y1 = beta0 + beta1 * x, lty = 2, col = rgb(0, 0, 0, 0.6))

# Exercise:
# define a function that, based on these data, returns a predictor function
# (do not use the built-in `predict()`)

make_linear_predictor <- function(x, y) {
  linear_fit <- lm("", "")
  beta0 <- coef("")[""]
  beta1 <- coef("")[""]
  predictor <- function(new_data) {
    return( "" )
  }
  return("")
}

# The predictor function persists and works, even when the data we used to create it is gone:
library(MASS)
#  Bwt = Body weight
#  Hwt = Heart weight
vet_predictor <- make_linear_predictor(x = cats$Bwt, y = cats$Hwt)
vet_predictor

detach(package:MASS)
cats$Bwt
cats$Hwt

vet_predictor(3.5)  # My cat's body mass in kilograms

formals(vet_predictor)
body(vet_predictor)

# What happens if we define `linear_fit` in the global environment?
linear_fit <- lm(rep(1, 10) ~ seq(1:10))
linear_fit
vet_predictor(3.5)

# Nothing, because `linear_fit` was stored in the internal environment. R searches for `linear_fit`
# first in the internal environment, and only if it fails to find it there it searches for
# it in the global environment.

ls(environment(vet_predictor))

# But, as it turns out, the data is also saved here! That is because **the environment from which a
# function is called is saved with it**.

environment(vet_predictor)$x
environment(vet_predictor)$y

# Let us then make a slimmer version:

make_linear_predictor_slim <- function(x, y) {
  linear_fit <- lm(y ~ x, model = FALSE)
  beta0 <- coef(linear_fit)[1]
  beta1 <- coef(linear_fit)[2]
  rm("", "", "")
  predictor <- function(new_data) {
    return( unname( beta0 + beta1 * new_data ) )
  }
  return(predictor)
}


vet_predictor_slim <- make_linear_predictor_slim(MASS::cats$Bwt, MASS::cats$Hwt)

vet_predictor_slim(3.5)
all.equal(vet_predictor_slim(3.5), vet_predictor(3.5))

ls(environment(vet_predictor_slim))

# Compare the results based on an inflated data set, where both `x` and `y` are repeated 100000 times.

vet_predictor <- make_linear_predictor(x = rep(MASS::cats$Bwt, 100000), y = rep(MASS::cats$Hwt, 100000))
vet_predictor_slim <- make_linear_predictor_slim(x = rep(MASS::cats$Bwt, 100000), y = rep(MASS::cats$Hwt, 100000))

saveRDS(vet_predictor, "vet_predictor.rds")
file.info("vet_predictor.rds")$size

saveRDS(vet_predictor_slim, "vet_predictor_slim.rds")
file.info("vet_predictor_slim.rds")$size

# This can be very useful with a large training set.

## A more mathematical example

# - Instead of finding $\nabla f(x)$, find the function $\nabla f$:

nabla <- function(f, ...) {
  grad_fn <- function(x, ...) { 
    numDeriv::grad(func = f, x = x, ...)
  }
  return(grad_fn)
}

# Exercise: Write a test case to see if this works.


""


## Summary

# - In R, functions are objects, and can be arguments to other functions:
#     + Use this to do the same thing to many different functions.
#     + Separates writing the high-level operations and the first-order functions.
#     + Use `sapply` (etc.), wrappers, anonymous functions as adapters.
# - Functions can also be returned by other functions:
#     + Variables other than the arguments to the function are fixed by the environment of creation.
#     + Manipulating expressions lets us flexibly create functions.