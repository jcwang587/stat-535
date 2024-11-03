# Agenda
# ===
# Atomic types:
# - Arrays
# - Matrices

# Non-atomic types:
# - Lists
# - Dataframes
# - Structures of structures

# Atomic structures, starting with arrays
# ===

# Many data structures in R are made by adding bells and whistles to vectors, 
# so "vector structures".

# **Arrays**
x <- c(7, 8, 10, 45)

x.arr <- array(x, dim = c(2, 2))


x.arr

is.matrix(x.arr)



# `dim` says how many rows and columns; filled by columns.

dim(x.arr)
is.vector(x.arr)
is.matrix(x.arr)
is.array(x.arr)

typeof(x.arr)
str(x.arr)


x.arr <- array(x, dim = c(2, "9"))
x.arr <- array(x, dim = c(2, 2.5))

x.arr <- array(x, dim = c(3, 9))

x.arr - c(3, 2, 27)

t(t(x.arr) - 1:9)


# Can have 3, 4, ..., n dimensional arrays; `dim` is a length-n vector:
x.arr <- array(x, dim = c(2, 7, 5))

x.arr[ , , 1]
x.arr[ , , 5]

x.arr[1, , ]
x.arr[3, , ]

x.arr[ , 3, ]

x.arr <- array(x, dim = c(2, 7, 5, 6, 7, 24))
x.arr[1, 4, 5, 2, 3, 5]
is.vector(x.arr)
is.array(x.arr)

# typeof() returns the type of the elements.
# str() gives the **structure**: here, a numeric array, with two dimensions, 
# both indexed 1--2, and then the actual numbers. 
# attributes() returns a named list, where each item in the list is an attribute.

typeof(x.arr)
str(x.arr)

# Or a 3D array:
z.arr <- array(1:(2 * 3 * 4), dim = c(2, 3, 4))
z.arr

dim(z.arr)

typeof(z.arr)
str(z.arr)

# Accessing and operating on arrays
# ===

# We can access a 2-D array either by pairs of indices or by the underlying vector:
x.arr
x.arr[1,2]
x.arr[3]

# Omitting an index means "all of it":
x.arr[1:2, 2]
x.arr[ , 2]

z.arr[2, 1, 3]
z.arr[ , 1, 3]
z.arr[ , , 3]


# Functions on arrays
# ===
# Using a vector-style function on a vector structure will go down to the underlying
# vector, unless the function is set up to handle arrays specially:
x.arr
x.arr > 9
c(x.arr)
which(x.arr > 9)

z.arr
z.arr > 9
c(z.arr)
which(z.arr > 9)

# Many functions do preserve array structure:
y.arr <- -x
y.arr + x.arr

x.arr + 8

w.arr <- -z.arr
w.arr + z.arr

z.arr + 8


# Running example: resource allocation ("mathematical programming")
# ====
# A factory makes cars and trucks, using labor and steel:

# - a car takes 40 hours of labor and 1 ton of steel.
# - a truck takes 60 hours and 3 tons of steel.
# - resources: 1600 hours of labor and 70 tons of steel each week.


# Matrices
# ===
# In R, a matrix is a special case of an array, a 2D one.

factory <- matrix(c(40, 1, 60, 3), nrow = 2)
factory
is.array(factory)
is.matrix(factory)

# The dimension could be specified by `ncol`, and/or `byrow=TRUE` to fill by rows.  

# Element-wise operations with the usual arithmetic and comparison operators
# (e.g., `factory / 3`).

# Compare whole matrices with `identical()` or `all.equal()`. `all.equal()` tests 
# for near equality, while `identical()` is more exact (e.g. it has no tolerance for 
# differences, and it compares storage type).

all.equal(8.0, 8.1, tolerance = 1.0)

# Matrix multiplication
# ===
# Gets a special operator

six.sevens <- matrix(rep(7, 6), ncol = 3)
six.sevens

factory %*% six.sevens # [2x2] * [2x3]


# What happens if you try `six.sevens %*% factory`?


# Multiplying matrices and vectors
# ===

# Numeric vectors can act like proper vectors:

output <- c(10, 20)
factory %*% output  # vector is recast to 2x1
output %*% factory  # vector is recast to 1x2

# R casts the vector as either a row or a column matrix.

# Matrix operators
# ===

# Transpose:
t(factory)

# Determinant:
det(factory)

# The diagonal
# ===
# The `diag()` function can extract the diagonal entries of a matrix:
diag(factory)

# Creating a diagonal or identity matrix
diag(c(3, 4))
diag(2)
# It can also _change_ the diagonal:
all_fives <- matrix(5, 2, 2)
diag(all_fives) <- c(35, 4)
all_fives


# Inverting a matrix
solve(factory)
factory %*% solve(factory)

# "Solve" linear system
# ===
# Solving the linear system Ax = b for x:

available <- c(1600, 70)
solve(factory, available)

#         car  truck
# labor [ 40[labor/car]    60[labor/truck] ] [ x1[cars]   ]   [1600[labor]]
# steel [  1[steel/car]     3[steel/truck] ] [ x2[trucks] ] = [  70[steel]]


#  Therefore, we can afford 10 cars and 20 trucks with the 
#  available resources (labor, steel)

# Names in matrices
# ===
# We can name either rows or columns or both, with `rownames()` and `colnames()`. 
#  These are character vectors, and we use the same function to get and to set 
# their values.Names help us understand what we're working with.

rownames(factory) <- c("labor", "steel")
colnames(factory) <- c("cars", "trucks")
factory

# Alternatively, you can use `dimanmes()` to assign names to the rows and cols:
factory2 <- factory
dimnames(factory2) = list(c("labor", "steel"),
                          c("cars", "trucks"))
factory2
identical(factory, factory2)

available <- c(1600, 70)
names(available) <- c("labor", "steel")

solution <- solve(factory, available)
solution

# Row or column operations
# ===
# Take the mean: `rowMeans()`, `colMeans()`: input is matrix,
# output is vector.  Also `rowSums()`, etc.

# `summary()`: vector-style summary of column.

colMeans(factory)
rowMeans(factory)
summary(factory) # applies to each column

# `apply()`, takes 3 arguments: the array or matrix, then 1 for rows and 2
# for columns, for the second argument, then name of the function to apply to each.
apply(factory, 1, mean)
apply(factory, 2, mean)

# What would `apply(factory, 1, sd)` do?

# Lists
# ====
# Sequence of values, _not_ necessarily all of the same type. I.e. non-atomic vectors.

my.distribution <- list("exponential", 7, FALSE)
my.distribution

# Most of what you can do with vectors you can also do with lists.

# Accessing elements of lists
# ===
# Can use `[ ]` or use `[[ ]]` for vectors and matrices, but only with a single index.
# `[[ ]]` drops names and structures, `[ ]` does not.

my.distribution[1]
is.character(my.distribution[1])
my.distribution[[1]]
is.character(my.distribution[[1]])
my.distribution[[2]]^2
("[["(my.distribution, 2))^2


#  What happens if you try `my.distribution[2] ^ 2`?
#  What happens if you try `[[ ]]` on a vector?

# Expanding and contracting lists
# ===
# Add to lists with `c()` (also works with vectors):
my.distribution <- c(my.distribution, 7)
my.distribution

# Chop off the end of a list by setting the length to something smaller
# (also works with vectors):
length(my.distribution)
length(my.distribution) <- 3
my.distribution

# Naming list elements
# ===
# We can name some or all of the elements of a list
names(my.distribution) <- c("family", "mean", "is.symmetric")
my.distribution
my.distribution[["family"]]
my.distribution["family"]


# Lists have a special short-cut way of using names, `$` (which 
# removes names and structures):
my.distribution[["family"]]
my.distribution$family

# Adding named elements:
my.distribution$was.estimated <- FALSE
my.distribution[["last.updated"]] <- "2011-08-30"
my.distribution


# Removing a named list element, by assigning it the value `NULL`:
my.distribution$was.estimated <- NULL
my.distribution

# Creating a list with names:
another.distribution <- list(family = "gaussian",
                             mean = 7,
                             sd = 1,
                             is.symmetric = TRUE,
                             last.updated = -5)
another.distribution

# Key-Value pairs
# Lists give us a way to store and look up data by _name_, rather than by _position_.

# A really useful programming concept with many names:
# **key-value pairs**, **dictionaries**, **associative arrays**, **hashes**

# If all our distributions have components named `family`, we can look it up by
# name without caring where it is in the list


# Hierarchical lists
#===
# So far, every list element has been same length.

# List elements can be other data structures, e.g., vectors and matrices.
# List elements can even be other lists which may contain other data structures, 
# including other lists, which may contain other data structures...

# This **recursion** lets us build arbitrarily complicated data structures from 
# the basic ones.

# Most complicated objects are (usually) lists of data structures.


all.distributions <- list(the_good = my.distribution,
                          the_bad = another.distribution,
                          the_ugly = another.distribution)
lapply(all.distributions, "[[", "family") # applies to lists
sapply(all.distributions, "[[", "family") # applies to vectors, tries to simplify output

lapply(all.distributions, "[[", "last.updated") # applies to lists
sapply(all.distributions, "[[", "last.updated") # applies to vectors, tries to simplify output

# Last note on lists:
# ===
# Atomic vectors are a special type of lists. So functions that apply to lists 
# (which are non-atomic vectors") also apply to vectors. E.g.
x
x[[4]]
lapply(x, "+", 1)
sapply(x, "+", 1)

# Dataframes
# ===
# + Dataframe is the classic data table, n rows for cases, p columns for variables.
# + It is not just a matrix because *columns can have different types*.   
# + It is not a list because all columns have the same number of rows.
# + Although many matrix functions work for dataframes (`rowSums()`,
# `summary()`, `apply()`), no matrix multiplying dataframes can be
# performed even if all columns are numeric.

a.matrix <- matrix(c(35, 8, 10, 4), nrow = 2)
colnames(a.matrix) <- c("v1","v2")
a.matrix
class(a.matrix)
a.matrix[ ,"v1"]  # Try a.matrix$v1 and see what happens

a.data.frame <- data.frame(a.matrix, logicals = c(TRUE, FALSE))
a.data.frame
class(a.data.frame)
a.data.frame$v1
a.data.frame[ ,"v1"]
a.data.frame[1, ]
colMeans(a.data.frame)
# What happened with the third value?

# Note: lists are by far the most flexible data structure in R. They 
# can be seen as a collection of elements without any restriction on 
# the class, length or structure of each element. Since each element of
# a list can also be a list, lists are the preferred data type for hierarchical data.
# The only thing you need to take care of, is that you don't give two elements the
# same # name. That might cause a lot of confusion as R doesn't give errors
# for that.

# Data frames are lists as well, but they have a few restrictions:  

# + you can't use the same name for two different variables.
# + all elements of a dataframe are atomic vectors.
# + all elements of a dataframe have an equal length.

# Adding or removing rows and columns
# ===
# We can add rows or columns to an array or data-frame with `rbind()` and `cbind()`,
# but be careful about forced type conversions:
a.data.frame <- rbind(a.data.frame, list(v1 = -3, v2 = -5, logicals = TRUE))
a.data.frame
a.data.frame <- cbind(a.data.frame, v3 = c(3,4,6))
a.data.frame

# Note: rbind() and cbind() are very slow operations. It is much preferred to specify 
# a large data frame in advance and fill in lines rather than adding them using 
# rbind() and cbind().

# To remove rows or columns:
a.data.frame[-c(3), -c(3, 4)]



# More on dataframe: Creating an dataframe
# ===
library(datasets)
states <- 
  data.frame(
    state.x77,
    abb = state.abb,
    region = state.region,
    division = state.division
  )

# `data.frame()` is combining here a pre-existing matrix (`state.x77`),
# a vector of characters (`state.abb`), and two vectors of qualitative
# categorical variables (**factors**; `state.region`, `state.division`)

# Column names are preserved or guessed if not explicitly set
colnames(states)
states[1, ]

# Dataframe access
# ===
# By row and column index:
states[49, 3]

# By row and column names:
states["Wisconsin", "Illiteracy"]

# All of a row:
states["Wisconsin", ]

# All of a column:
head(states[ , 3])
head(states[ , "Illiteracy"])
head(states$Illiteracy)

# Rows matching a condition:
# states[states$division=="New England", "Illiteracy"]
# states[states$region=="South", "Illiteracy"]

# Replacing values
# ===
# Parts or all of the dataframe can be assigned to:
summary(states$HS.Grad)
states$HS.Grad <- states$HS.Grad / 100
summary(states$HS.Grad)
states$HS.Grad <- 100 * states$HS.Grad

# with()
# ===
# What percentage of literate adults graduated HS (high school)?
head(100 * (states$HS.Grad / (100 - states$Illiteracy)))

# `with()` takes a data frame and evaluates an expression "inside" it:
with(states, head(100 * (HS.Grad / (100 - Illiteracy))))

# Data arguments
# ===
# Lots of functions take `data` arguments, and look variables up in
# that data frame:
plot(Illiteracy ~ Frost, data = states)

# You might see the following in examples, this is NOT recommended for code that
# is anything more than trivial:
attach(states)
plot(Illiteracy ~ Frost, col = "blue")


# Summary
# ===
# - Arrays add multi-dimensional structure to vectors.
# - Matrices act like you'd hope they would.
# - Lists let us combine different types of data.
# - Dataframes are hybrids of matrices and lists, for classic tabular data.
# - Recursion lets us build complicated data structures out of the simpler ones.