# Agenda ====
# - Conditionals: switching between different calculations.
# - Iteration: Doing something over and over.
# - Vectorizing: Avoiding explicit iteration.

# Conditionals ====
# Have the computer decide what to do next.

# Example 1:

#       {  x, if x>=0
# |x| = {
#       { -x, if x<0

#          { x^2,        if |x|<=1
# psi(x) = {
#          { 2*|x| - 1,  if |x|>1


# Exercise, plot psi(x) for -2.5 <= x <= 2.5

# x <- seq(-2.5, 2.5, 0.01)
# psi <- numeric(length(x))
# psi[x < - 1] <- 2 * -(x[x < -1]) - 1
# psi[x >= - 1 & x <= 1] <- x[x >= - 1 & x <= 1]^2
# psi[x > 1] <- 2 * (x[x > 1]) - 1

x <- seq(-2.5, 2.5, 0.01)
psi_x <- numeric(length(x))
psi_x[x < -1] <- 2 * abs(x[x < -1]) - 1
psi_x[x >= -1 & x <= 1] <- (x[x >= -1 & x <= 1]) ^ 2
psi_x[x > 1] <- 2 * abs(x[x > 1]) - 1

plot(x, psi_x, type = "l")
lines(x, x^2, col = "red")

# Example 2:
# if the country code is not "US", multiply prices by current exchange rate.

# if( ) ====
# Simplest conditional:

# if (condition) {
#   do_something
# }


love <- FALSE
if (love) {
  print("Yes!! They love me!")
}


# Slightly less simple conditional:
# if (condition) {
#   do_something
# } else {
#   do_something_else
# }

# Compute absolute value:
x <- -5
if (x >= 0) {
  abs_x <- x
} else {
  abs_x <- -x
}
abs_x

# Try the above with a vector, say c(-5, 3, -2, 1, 0), does it work?


# The exception is that a numeric value of length 1 is recast as FALSE if 0, 
# TRUE if anything else.
if (1) { print("Got here") }
if (0) { print("Got here") }
if (0 == FALSE) { print("Got here") }
if (NA) { print("Got here") }
if (NaN) { print("Got here") } 
if (-Inf) { print("Got here") } 
(0.5 - 0.3) - (0.3 - 0.1)
if ((0.5 - 0.3) - (0.3 - 0.1)) { print("Got here") } 
if (!all.equal((0.5 - 0.3) - (0.3 - 0.1), 0)) { print("Got here") } 

# One-line actions don't need curly braces (you will see this when you read code, 
# not recommended):
if (x >= 0) x else -x

# Nested if( ) ====
# `if` can *nest* arbitrarily deeply:
x <- seq(-2.5, 2.5, 0.01)
if (x^2 < 1) {
  x^2
} else {
  if (x >= 0) {
    2 * x - 1
  } else {
    -2 * x - 1
  }
}



# Combining Booleans: && and || ====
# `&` and `|` work like `+` or `*`, combining terms element-wise while 
# `&&` and `||` give _one_ Boolean, lazily:

c(1, 2, 3) < 2.5
(0 == 0) & (c(1, 2, 3) < 1.5)
(0 == 0) && (c(1, 2, 3) < 1.5)

(0 > 0) & (print("5") != "5")
(0 > 0) && c(print("5") != "5", print("6") != "5")
(0 == 0) && c(print("5") != "5", print("6") != "5")

(0 == 0) | (print("5") != "5")
(0 > 0) || c((print("5") != "5"), (print("6") != "5"))
(0 == 0) || c((print("5") != "5"), (print("6") != "5"))
  
# This expression on the right is only evaluated if needed.


# switch( ) ====
# The `switch()` function tests an expression against elements of a `list`. Simplify nested `if` with `switch()`: give a variable to select on, then a value for each option:
EXPR <- 2
switch(EXPR,
       "red",
       "green",
       "blue"
       )
switch(EXPR + 1,
       "red",
       "green",
       "blue"
       )

EXPR <- "color"
switch(EXPR,
       color = "red",
       shape = "square",
       length = 5
       )
EXPR <- "length"
switch(EXPR,
       color = "red",
       shape = "square",
       length = 5
       )

library(datasets)
states <-
  data.frame(
    state.x77,
    abb = state.abb,
    region = state.region,
    division = state.division
    )
type.of.summary <- "median"
switch(
  type.of.summary,
  mean = mean(states$Murder),
  median = median(states$Murder),
  histogram = hist(states$Murder),
  "What?!")

type.of.summary <- "something"
switch(
  type.of.summary,
  mean = mean(states$Murder),
  median = median(states$Murder),
  histogram = hist(states$Murder),
  "What?!"
  )

# Iteration Three ways ====
# 
#   + ```for(){}```
#   + ```while(){}```
#   + ```repeat{}```: `repeat` has no Boolean exit condition. You must use break.
# 
# for( ) ====
# `for` iterates over a **counter** (here `i`) along a vector
# (here `1:length(table.of.logarithms)`) and **loops through**
# the **body** until it runs through the vector:

for (i in 1:7) {
  print(i)
}

vector_of_logarithms <- numeric(7)
vector_of_indices <- 1:7
for (i in vector_of_indices) {
  cat(paste0("i = ", i, "\n"))
  vector_of_logarithms[i] <- log(i)
}
vector_of_logarithms


for (c in c("love", "hate")) {
  print(c)
}


# ```for``` construct works on any vector. You can loop over 
# a vector of names (or file names), for instance.

smurf_names <- 
  c("Smurfette",
    "Papa Smurf",
    "Clumsy Smurf",
    "Brainy Smurf",
    "Grouchy Smurf",
    "Hefty Smurf"
    )

for (smurf_name in smurf_names) {
  
  if (rbinom(1, 1, 0.5)) {
    cat(paste0("I love you ", smurf_name, "!\n"))
  } else {
    cat(paste0("I love you not, ", smurf_name, "!\n"))
  }
  
}

# Or you can loop over a list (= non-atomic vector)
list_to_iterate_over <- list(1, "character", FALSE)
for (l in list_to_iterate_over) {
  print(typeof(l))
}


for (i in 1:7) {
  print(vector_of_logarithms[i])
}

for (i in vector_of_logarithms) {
  print(which(i == vector_of_logarithms))
  print(i)
}


# Nested Iteration Example ====
A <- matrix(1:10, nrow = 2, ncol = 5)
B <- matrix(11:25, nrow = 5, ncol = 3)
C <- matrix(0, nrow = nrow(A), ncol = ncol(B))
if (ncol(A) == nrow(B)) {
  for (i in 1:nrow(C)) {
    for (j in 1:ncol(C)) {
      for (k in 1:ncol(A)) {
        C[i,j] <- C[i,j] + A[i,k] * B[k,j]
      }
    }
  }
} else {
  stop("matrices a and b non-conformable")
}

# What does this do?
C
# Compare it to the standard operator:

A %*% B



# `next` statement skips the remainder of the current iteration 
# of the loop and proceed directly to the next one,
# `break` statement exists the loop.
x <- 1:6

# val <- 3
for (val in x) {
  
  if (val == 3){
    next
  }
  if (val == 6) {
    break
  }

  print(val)
  
}

# while( ): Conditional Iteration ====

# Take the square root of `x0` until it is close enough to 1:
x0 <- 7
x <- x0
{
  i <- 1
  cat(paste0("x = ", x, "\n"))
  while (x - 1 > 1e-06) {
    x <- sqrt(x)
    cat(paste0(x0, "^(1/", 2 ^ i,  ") = ", x, "\n"))
    i = i + 1
  }
}

# Condition in the argument to `while()` must be a single Boolean value
# (like `if()`). The body is looped over until the condition is `FALSE`.
# The loop never begins unless the condition starts `TRUE`.


# for( ) vs. while( ) ====
# `for()` is better when the number of times to repeat (values to iterate over) is
# clear in advance, while `while()` is better when you can recognize when to stop
# once you're there, even if you can't guess it to begin with. Every `for()` could
# be replaced with a `while()`.  

# repeat ====

repeat {}

# Sample from the normal distribution and stop when an observation is larger in 
# absolute value than 2.5:

# Sample from the normal distribution and stop when an observation is larger in 
# absolute value than 2.5:
repeat {
  s <- rnorm(1)
  cat(paste0(round(s, 3), "; "))
  if (abs(s) > 2.5) {
    break
  }
}

# Vectorization ====
  
# As we have seen, many very basic operations in R actually involve a whole
# set of operations that run in the background. This may potentially cause R code
# to be slow, compared to compiled (C, C++, Java) code. Many operations can be
# *vectorized*. That is, some of the operations skipped, memory allocated in a 
# more efficient fashion, and loops carried out in C or C++. This, at times, can
# significantly accelerate your code.

# For a nice introduction to vectorization in R, see:
# https://www.r-bloggers.com/vectorization-in-r-why/  
    

# Example 1:

N <- 10
a <- rnorm(N)
b <- rnorm(N)

# An awful way to compute the sum of the vectors `a` and `b`:
a_plus_b_ver1 <- a[1] + b[1]
for (i in 2:N) {
  a_plus_b_ver1[i] <- a[i] + b[i]
}
a_plus_b_ver1

# A slightly less awful way to compute the sum of two vectors:
a_plus_b_ver2 <- numeric(N)
for (i in 1:N) {
  a_plus_b_ver2[i] <- a[i] + b[i]
}
a_plus_b_ver2

# We "vectorized" the memory allocation.

# A good way to compute the sum of two vectors:
a_plus_b_ver3 <- a + b

# Let's measure the relative speed using the `microbenchmark` function:
N <- 100000
a <- rnorm(N)
b <- rnorm(N)

res <- 
  microbenchmark::microbenchmark(
    {
      a_plus_b_ver1 <- a[1] + b[1]
      for (i in 2:N) {
        a_plus_b_ver1[i] <- a[i] + b[i]
      }
    },
    {
      a_plus_b_ver2 <- numeric(N)
      for (i in 1:N) {
        a_plus_b_ver2[i] <- a[i] + b[i]
      }
    },
    {
      a_plus_b_ver3 <- a + b
    },
    unit = "ns" # for "nanoseconds"
  )
sum_res <- summary(res)
sum_res
sum_res$median[1] / sum_res$median[2]
sum_res$median[2] / sum_res$median[3]

# Using the triple `for()` loop we wrote earlier vs. `a %*% b` will be even worse...


# Vectorized Calculations ====
# Many functions are set up to vectorize automatically:
abs(-3:3)
log(1:7)

# We saw the `apply()` family in the previous lecture.

# Vectorized Conditions: ifelse() applies an `if` statement to each element of a vector: ====
# The `ifelse` function:
# 1st argument is a logical vector, then pick from the 2nd or 3rd arguments, 
# first for `TRUE` and then for `FALSE`.


N <- 10
x <- runif(N, -5, 5)
ifelse(x > 5, "X is greater than 5!", "x is smaller than 5  :(")


# using a high level loop:
x_seq <- seq(-2.5, 2.5, 0.01)
res <- numeric(length(x_seq))
i <- 1
for (x in x_seq) {
  if (x^2 < 1) {
    res[i] <- x^2
  } else {
    if (x >= 0) {
      res[i] <- 2 * x - 1
    } else {
      res[i] <- -2 * x - 1
    }
  }
  i <- i + 1
}

# "vectorized":
x <- x_seq
res2 <- 
ifelse(
  x^2 < 1,
  x^2,
  ifelse(
    x >= 0,
    2 * x - 1,
    -2 * x - 1
  )
)
  
# Speed:
N <- 10000
x <- runif(N, -5, 5)
res <- 
  microbenchmark::microbenchmark(
    {
      x_seq <- seq(-2.5, 2.5, 0.01)
      res <- numeric(length(x_seq))
      i <- 1
      for (x in x_seq) {
        if (x^2 < 1) {
          res[i] <- x^2
        } else {
          if (x >= 0) {
            res[i] <- 2 * x - 1
          } else {
            res[i] <- -2 * x - 1
          }
        }
        i <- i + 1
      }
      
    }, 
    {
      x <- x_seq
      res2 <- 
        ifelse(
          x^2 < 1,
          x^2,
          ifelse(
            x >= 0,
            2 * x - 1,
            -2 * x - 1
          )
        )
    }
  )
sum_res <- summary(res)
sum_res$median[1] / sum_res$median[2]

# It is important to notice that the performance may also depend on the 
# input values. What do you expect will happen if we repeat the same computation
# with x <- runif(N, another_lower_boundary, another_upper_boundary)?

# Summary ====
# - `if`, nested `if`, `switch`.
# - Iteration: `for`, `while`, `repeat`.
# - Avoiding iteration with whole-object ("vectorized") operations.