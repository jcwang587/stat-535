# Agenda ====
# -  Defining functions: Tying related commands into bundles.
# -  Interfaces: Controlling what the function can see and do.
# -  Example: Parameter estimation code.


# Why Functions? ====
 
# Data structures tie related values into one object, and functions tie related commands into one object. 
# In both cases: easier to understand, easier to work with, easier to build into larger things.
 
# What is Function for? ====

# - Things you're going to re-run, especially if it will be re-run with different data or parameters.
# - Chunks of code you keep highlighting and hitting return on.
# - Chunks of code which are small parts of bigger analyses.
# - Chunks which are very similar to other chunks.

# Example ====

# "Robust" loss function, for outlier-resistant regression
# Inputs: vector of numbers (x)
# Outputs: vector with x^2 for small entries, 2|x|-1 for large ones
psi1 <- function(x) {
  if (!is.numeric(x)) { return(0) }
  psi <- 
    ifelse(
      x^2 > 1,
      2 * abs(x) - 1,
      x^2
      )
  return(psi)
}


# We can use the defined function just like the built-in functions:
z <- c(-0.5, -5, 0.9, 9)
zz <- psi1(z)
psi1("V")

# **Interfaces**:
# the **inputs** or **arguments**;
# the **outputs** or **return value**

# Calls other functions `ifelse()`, `abs()`, operators `^` and `>`.
# `return()` says what the output is.  

# Comments: one-line description of purpose; listing of arguments;
# listing of outputs. Not required but very helpful.

# Named and Default Arguments ====

# "Robust" loss function, for outlier-resistant regression
# Inputs: vector of numbers (x), scale for crossover (C), defaults to 1
# Outputs: vector with x^2 for small entries, 2*C*|x| - C^2 for large ones:

psi2 <- function(x, C = 1) {
  psi <- ifelse(x^2 > C^2, 2 * C * abs(x) - C^2, x^2)
  return(psi)
}

identical(psi1(z), psi2(z, 1))
Z <- z
identical(psi1(Z), psi2(C = 1, x = Z))

identical(psi1(Z), psi2(C = 1, Z))

# Default values get used if names are missing:
psi1(Z)

# Named arguments can go in any order when explicitly tagged:
psi2(C = 1, x = Z)


# Checking Arguments ====

# _Problem_: Odd behavior when arguments aren't as we expect.
psi2(x = z, C = c(1, 1, 1, 10))
psi2(x = z, C = -1)

# _Solution_: Arguments to `stopifnot()` are a series of expressions which should all
# be TRUE; execution halts, with error message, at _first_ FALSE.

# "Robust" loss function, for outlier-resistant regression
# Inputs: vector of numbers (x), scale for crossover (C) defaults to 1
# Outputs: vector with x^2 for small entries, 2*C|x|-C^2 for large ones
psi3 <- function(x, C = 1) {
  # Scale should be a single positive number
  stopifnot(length(C) == 1, C > 0)
  psi <- ifelse(x^2 > C^2, 2 * C * abs(x) - C^2, x^2)
  return(psi)
}


psi3(x = z, C = c(1, 1, 1, 10))
psi3(x = z, C = -1)


# Function Environment ====

# - Each function has its own environment.  
# - Names in the function's environment override names in the global environment. 
# - Internal environments start with the named arguments.  
# - Assignments inside the function only change the internal environment.  
#  (There _are_ ways around this, but they are difficult and best avoided.)
# - Names undefined in the function are looked for in the environment the function 
#   gets called from.  

# Example 1: Internal Environment ====

x <- 7
y <- c("A","C","G","T","U")
adder <- function(x, y) {
  x <- x + y
  return(x)
  }

adder(1, 2)


# R, when it doesn't find a variable in the internal environment, looks for it in the "parent" environment...
x <- "&"
y <- c("A","C","G","T","U")

adder <- function(y) {
  x <- x + y
  return(x)
}

adder(1)



# Example 2: Internal Environment ====

circle_area <- function(r) { return(pi * r^2) }
circle_area(c(1, 2, 3))
pi <- 3
circle_area(c(1, 2, 3))
rm(pi) # Restore sanity
circle_area(c(1, 2, 3))


# Ellipsis ====
# The ellipsis argument `...` captures any number of arguments that aren’t otherwise matched.
# You can use this to provide additional arguments for functions you call from within 
# your function.

# Example:
plot_x_squared <- function(x, ...) {
  return(plot(x, x^2, main = "Plot of x squared", ...))
}

plot_x_squared(seq(1:5))
plot_x_squared(x = seq(1:5))
plot_x_squared(seq(1:5), type = "l", col = "red", lwd = 3)
plot_x_squared(seq(1:5), type = "l", col = "red", lwd = 3, main = "Haha!")




# See more at:
# https://www.r-bloggers.com/r-three-dots-ellipsis/

# Exercise:
# Write a function that samples from the truncated normal distribution and plots a 
# histogram of the sample if the user chooses to do so.

# How do we think about this?

# 1. What is the core task?
# 2. What are the inputs? (then add defaults, checks)
# 3. What are the outputs? (the consider adding attributes)

# Answer for 1.: sample from the truncated normal distribution. We have seen how 
# to do that:
s <- rnorm(10000)
trunc_s <- s[s > 0.5]

# But this is partial. This is just the left-truncated normal. We can do this more
# generally with arbitrary mu, sigma, a and b.
a <- 0.5
b <- Inf
mu <- 0
sigma <- 1
''
''


# Continuation of the answer for 1.: plot a histogram if wanted.
#...


''

# Time to make this into a function.
# 2. What are the inputs?



# 3. what are the outputs? 



sample_truncated_normal <- function() {
  #...
  
  
  
  
  return()
}

tn_sample <- sample_truncated_normal()
hist()

# Think about default values, sanity checks, additional arguments:
sample_truncated_normal <- function() {

  
  
  return()
}

tn_sample <- sample_truncated_normal()
#...



''


# Add some attributes:
# First, check what are the original attributes of your output, so that you won't
# override attributes that were assigned by other functions and that you may want
# to preserve.
attributes(tn_sample)

# Here an empty list. So we can add at will. Let's add N and the hist() object if 
# called for.

sample_truncated_normal <- function() {

    

  return(trunc_s)
}

tn_sample <- sample_truncated_normal()
str(tn_sample)

tn_sample <- sample_truncated_normal(10000, plot_histogram = TRUE)
str(tn_sample)


# Using Multiple Functions ====

# - Multiple functions: Doing different things to the same object.
# - Sub-functions: Breaking up big jobs into small ones.

# _Meta-problems_: When/why you have to write more than one function?

# - You've got more than one problem.
# - Your problem is too hard to solve in one step.
# - You keep solving the same problem.

# _Meta-solutions_:

# - Write multiple functions, which rely on each other.
# - Split your problem, and write functions for the pieces.
# - Solve the recurring problems once, and re-use the solutions.

# We will see an example for this in HW3. 

# Summary ====

# -  **Functions** bundle related commands together into objects: easier to
#   re-run, easier to re-use, easier to combine, easier to modify, easier to think about, less risk of error.
# -  **Interfaces** control what the function can see (arguments,
#   environment) and change (its internals, its return value).
# -  **Calling** functions we define works just like calling built-in
#   functions: named arguments, defaults.
# - **Multiple functions** let us do multiple related jobs, either on the same object or on similar ones.
# - **Sub-functions** let us break big problems into smaller ones, and re-use the solutions to the smaller ones.