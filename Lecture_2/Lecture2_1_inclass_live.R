# Lecture 2-1: basics of data

# Agenda ======
# - Built-in data types
# - Built-in functions and operators
# - First data structures: Vectors and arrays
# 
# Data Types =========
#   
# + **Logical**: TRUE, FALSE, NA
# + **Numeric**: can be one of two:
#     + *Double*: 13.8, -0.5, Inf, 1
#     + *Integers*: 2L, 34L, 0L, -1L
# + **Complex**: 3 + 2i
# + **Character**: 'abc', "good", "TRUE", '12.3'
# + **Raw**: "Hello" is stored as 48 65 6c 6c 6f

# Examples:
v1 <- TRUE
class(v1)
typeof(v1)

v2 <- 2.5
class(v2)
typeof(v2)

v3 <- 2L
class(v3)

v4 <- 2 + 5i
class(v4)

v5 <- "Good evening"
class(v5)

(v6 <- charToRaw("Hello"))
class(v6)

# Operators
# =========
  
7 + 5
7 - 5
7 * 5
7 ^ 5
7 %% 5 # remainder
7 %/% 5 # integer division

# Operators are, in essence, functions. In fact, there is an equivalent 
# notation which highlights this:
  
"+"(5, 7)


# Number comparisons
# ==================
# Comparisons are also binary operators; they take two objects, like numbers, 
# and give a Boolean:

7 > 5
7 < 5
7 >= 7
7 == 5
7 != 5


# Comparison operators are also functions:

"=="(5, 7)


# Boolean operators
# =================
# "and":
(5 > 7) & (6 * 7 == 42)


# "or":
(5 > 7) | (6 * 7 == 42)


# More types
# ==========
  
# `typeof()` function returns the type

# `is.`_foo_`()` functions return Booleans for whether the argument is of type _foo_.

# `as.`_foo_`()` (tries to) cast its argument to type _foo_ to translate it sensibly 
# into a _foo_-type value.

# Examples:
  
typeof(7)
is.numeric(7)
is.double(7)
is.integer(7)

typeof(7L)
is.numeric(7L)
is.double(7L)
is.integer(7L)

is.na(7)
is.na(NA) # NA is logical
typeof(NA)

is.logical(NA_integer_)
NA_character_
NA_complex_

# 0, 1 = 2^1 = 2
# 00, 01, 10, 11 = 2^2 = 4 > 3 = number of possible logical values
# 000, 001, ..., 111 = 2^3 

is.na(0/0)
is.nan(0/0)

is.character(7)
is.character("7")
is.character("seven")
is.na("seven")

options(digits = 10)
print(6.100000001)
6.100000001 == 6.1
# When we print double, they are represented only up to a default level of precision.

5 / 6
as.character(5 / 6)
5 / 6 == as.numeric(as.character(5 / 6))
as.character(as.numeric(as.character(5 / 6)))

# Breakout session:
# Carefully map the inputs and outputs of the various operators and return 
# in 6 minutes with interesting examples:

# Interesting examples: 
typeof(2L * 2L)
typeof(4L * 2)
typeof(2 * FALSE)
typeof(2L ^ 2L)
typeof(1 & 2)  # we know that FALSE is recast as double to 0, TRUE to 1. 
# In general, most recasting in R goes from simple to complex. This is a rare example 
# of recasting a complex value to a simpler one. 
as.logical(1)
as.logical(0)
as.logical(2)

"hi" > "hello" # alphabetical ordering!
"hi" < "hello"

2 ^ 200
as.integer(2 ^ 200)

is.double(2 ^ 2000)
as.integer(2 ^ 2000)

# log(a^b) = b * log(a)
log(2 ^ 2000)
exp(2000 * log(2) - 1998 * log(2))

2^2000 - 2^1998

2 ^ (-2000) == 0L
-2000 * log(2)

# Variables
# =========

# We've already seen that we can assign values to variables.
a = 5
a <- 5
# There are also some important built in variables in R:

T
F

# is a global variable. However, as tempting it may be to use it, see what happens:

T <- "Peace!"
T
T == TRUE
rm(T)  
T

TRUE <- 5

# Similarly, pi
pi
pi <- 3.1
rm(pi)
pi


# Variables can be arguments to functions or operators, just like constants:

cos(pi)
pi <- 0
cos(pi)

# Note: generally speaking R functions does not use pointers (Indeed, many functions
# and objects in R use C in the background and utilize C pointers, but pointers are not
# used at the high-level)

# Workspace
# ========= 

# What names have you defined values for?
ls()


# Getting rid of a defined variable:
rm("pi")
ls()

foo <- function() {}



# Getting rid of only the defined functions:
rm(list = lsf.str())

# Getting rid of all defined objects:
rm(list = ls())


# Getting rid of all objects except for functions:
rm(list = setdiff(ls(), lsf.str()))
# What is setdiff?

# First data structure: vectors
# =============================

# Group related data values into one object, a **data structure**.

# An atomic **vector** is a sequence of values, all of the same type:
x <- c(7, 8, 45, 10)
x
is.vector(x)
is.numeric(x)
is.double(x)
length(x)

# c() is a function that returns a vector containing all its arguments in order.
x[1] 
# is the first element, `x[4]` is the 4th element.

# It is important to note that the brackets are also a function:
"["(x, 2)



x[-4] 
# is a vector containing all but the fourth element.

vector(length = 6)
numeric(6)
# returns an empty vector of length 6; helpful for filling things up later.

x[2] <- 5 
x[2] <- 8


# Vector arithmetic
# =================

# Operators apply to vectors "element wise" ("element by element"):
y <- c(-7, -8, -45, -10)
x + y 
x * y


# Recycling
# ===
# **Recycling** repeats elements in shorter vector when combined with longer
x

x + c(-7, -8)
x ^ c(1, -1)


# Scalars are vectors of length 1 for purposes of recycling:

2 * x


# Can also do element by element comparisons:
x == y


# Boolean operators work element wise:
x > 9




# Vector comparisons
# ===

# To compare whole vectors, best to use `identical()` or `all.equal()`:
z <- c(1, 2, 1, 2)
w <- c(1, 2)
z == c(1, 2)
identical(z, w)
all.equal(z, w)
dplyr::near(x, -y)
all(dplyr::near(x, -y))


# But:
identical(x, y)
all.equal(x, y)



# Functions that take vectors as input:
# =====================================
  
# Many functions take vectors as arguments:

# Vector in - scalar out:  
mean(x)
median(x)

sd(x)

var(x)
sum(  ( x - mean(x) )^2  ) / length(x) # ML estimator of the variance
var(x) == sum(  ( x - mean(x) )^2  ) / ( length(x) - 1 ) # another estimator of the variance - unbiased!

n <- length(x)
((n - 1) / n) * var(x)

max(x)
min(x)
is.integer(length(x))
sum(x)

# Vector in - vector out (element wise):
sqrt(x)
is.na(x)

# Vector in - vector out:
sort(x)
order(x)

# Vector in - something completely different out:
h <- hist(x)
# takes a vector of numbers and produces a histogram, a highly structured object, 
# with the side-effect of making a plot.
foo <- ecdf(x)
foo(8)

# produces a the empirical cumulative-density-function object.
summary(x)
# gives a five-number summary of numerical vectors.


# Addressing vectors ==================

# Vector of indices:
x[c(2, 4)]

# Vector of negative indices
x[c(-1, -3)]

# Boolean vector:
x[x > 9]
y[x > 9]

# which() inputs a Boolean vector and returns a vector of the indices of the TRUE values:
which(x > 9)
places <- which(x > 9)
places
y[places]

which(x == pi)
y[which(x == pi)]
# Different NULL values

# You can give names to elements or components of vectors:
names(x) <- c("v1","v2","v3","fred")
names(x)
x
x[c("fred","v1")]


# `names(x)` is a character vector:
names(x)
sort(names(x))
which(names(x) == "fred")


# Attributes:
# Attributes are additional layers of information that can be added to any object.
attributes(x) <- c(attributes(x), list(whim = "3"))

x + 1

y

set.seed(1)



identical(x, -y)
all.equal(x, -y)
all(dplyr::near(x, -y))

names(y) <- names(x)
identical(x, -y)
all.equal(x, -y)
all(dplyr::near(x, -y))



# Peculiarities of floating-point numbers
# ======================================

# The more bits in the fraction part, the more precision.

# The R floating-point data type is a `double`.

# With finite precision, arithmetic on `doubles` is not the 
# same as arithmetic on real numbers.

0.45 == 3 * 0.15

0.45 - 3 * 0.15
0.00000000000000000000001 + 0.1 == 0.1


0.00000000000000000000001 + 0.000000000000000000000001 == 0.000000000000000000000011
0.1 + 0.000000000000000000000001 == 0.100000000000000000000001
0.100000000000000000000001 == 0.1


# Often ignorable, but not always:
#   - Rounding errors tend to accumulate in long calculations.
#   - When results should be approximately 0, errors can flip signs.
sign(1e-16)
sign(-1e-16)
sign(0)


all.equal(0, 1e-16)
all.equal(0, -1e-16)
all.equal(1e-16, -1e-16)

#   - When comparisons are needed withing the code.
#   - Usually better to use `all.equal()` or `dplyr::near()` than exact comparison.

-16 * log(10)
16 * log(10)




#    - The round() function returns a double, but:
round(0.5 - 0.3, digits = 6) == round(0.3 - 0.1, digits = 6)
0.5 - 0.3 == 0.3 - 0.1


6 * as.numeric(as.character(5 / 6))
5 / 6 == as.numeric(as.character(5 / 6))
5 / 6 - as.numeric(as.character(5 / 6))
dplyr::near(5 / 6, as.numeric(as.character(5 / 6)))


# A word on factors
# =================
# R uses factors to represent categorical data. Factors are integer
# vectors that have a `levels` attribute:
x <- factor(c("ab", "cd", "ab"), levels = c("ab", "cd", "ef"))
x
typeof(x)

attributes(x)
