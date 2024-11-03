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
#     + *Double*: 13.8, -0.5, Inf
#     + *Integers*: 2L, 34L, 0L, -1L
# + **Complex**: 3 + 2i
# + **Character**: 'abc', "good", "TRUE", '12.3'
# + **Raw**: "Hello" is stored as 48 65 6c 6c 6f

# Examples:
v1 <- ""
class(v1)

v2 <- ""
class(v2)

v3 <- ""
class(v3)

v4 <- ""
class(v4)

v5 <- ""
class(v5)

(v6 <- "")
class(v6)

# Operators
# =========
  
7 + 5
7 - 5
""
""
""
""

# Operators are, in essence, functions. In fact, there is an equivalent 
# notation which highlights this:
  





# Number comparisons
# ==================
# Comparisons are also binary operators; they take two objects, like numbers, 
# and give a Boolean:

7 > 5
7 < 5
7 "" 7
7 "" 5
7 "" 5


# Comparison operators are also functions:




# Boolean operators
# =================
# "and":



# "or":



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
is.na(0/0)
is.nan(0/0)
is.character(7)
is.character("7")
is.character("seven")
is.na("seven")
as.character(5 / 6)
as.numeric(as.character(5 / 6))


# Breakout session:
# Carefully map the inputs and outputs of the various operators and return 
# in 6 minutes with interesting examples:

# Interesting examples: 





















# Variables
# =========

# We've already seen that we can assign values to variables.
a = 5

# There are also some important built in variables in R:

T

# is a global variable. However, as tempting it may be to use it, see what happens:





   
# Similarly, pi

# Variables can be arguments to functions or operators, just like constants:




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










# c() is a function that returns a vector containing all its arguments in order.
x[1] 
# is the first element, `x[4]` is the 4th element.

# It is important to note that the brackets are also a function:




x[-4] 
# is a vector containing all but the fourth element.

vector(length = 6)
# returns an empty vector of length 6; helpful for filling things up later.







# Vector arithmetic
# =================

# Operators apply to vectors "element wise" ("element by element"):
y <- c(-7, -8, -45, -10)




# Recycling
# ===
# **Recycling** repeats elements in shorter vector when combined with longer
x




# Scalars are vectors of length 1 for purposes of recycling:




# Can also do element by element comparisons:




# Boolean operators work element wise:





# Vector comparisons
# ===

# To compare whole vectors, best to use `identical()` or `all.equal()`:








# But:








# Functions that take vectors as input:
# =====================================
  
# Many functions take vectors as arguments:

# Vector in - scalar out:  
mean(x)
median(x)
sd(x)
var(x)
max(x)
min(x)
length(x)
sum(x)

# Vector in - vector out (element wise):
sqrt(x)
is.na(x)

# Vector in - vector out:
sort(x)

# Vector in - something completely different out:
hist(x)
# takes a vector of numbers and produces a histogram, a highly structured object, 
# with the side-effect of making a plot.
ecdf(x)
# produces a the empirical cumulative-density-function object.
summary(x)
# gives a five-number summary of numerical vectors.


# Addressing vectors
# ==================
  
# Vector of indices:



# Vector of negative indices



# Or combinations:



# Boolean vector:




# which() inputs a Boolean vector and returns a vector of the indices of the TRUE values:








# Named components
# ================
  
# You can give names to elements or components of vectors:
names(x) <- c("v1","v2","v3","fred")
names(x)





# `names(x)` is a character vector:
names(x)




# Attributes:
# Attributes are additional layers of information that can be added to any object.
attributes(x)
y

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






# Often ignorable, but not always:
#   - Rounding errors tend to accumulate in long calculations.
#   - When results should be approximately 0, errors can flip signs.
#   - When comparisons are needed withing the code.
#   - Usually better to use `all.equal()` or `dplyr::near()` than exact comparison.







#    - The round() function returns a double, but:




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
