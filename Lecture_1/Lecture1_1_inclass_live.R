# Introduction to R -----

# R is a high-level programming language for statistical computing and graphics.
# R-Studio is a an integrated development environment (IDE) for R.

# Recommended: try first all in R, then repeat in RStudio.

# We have already met a R command in this script. The '#' command, which...

# To compare, let's run our second command:
print("Hello world!")

# 'print' is a *function* that takes inputs and generate output.
# *Functions* typically accept arguments between parentheses.

# Data that appear between quotation marks or apostrophes are treated as text.
# Functions also return something:
a <- print("Hello world!")

# What does the function 'print' return?
# it returns its input.

# What is the third command we just met?
# `<-` the assignment operator

# What changed in the IDE when we ran this command?
# we see the varialbe's name and value in the Environment tab.

a
# What happened when we ran the above line?


(a <- print("hi"))
# What happened in the above?


# Compare to:
a <- "hi"
a <- 5
(a <- 5)

# Few preliminaries:
# 1. R is case sensitive.
A <- 5

# 2. R is 1-based.

a <- c(3, 4, 5)
a[1]
a[0] # for zero based

# 3. R ignores indentation and spacing.

5+6
            5 + 6

5    +    6

5 + 
  6

# Try in console:
# 5       +  (enter) 6 (enter)
# 5   + (Esc)

# What is `+` ?
# It is an operator which is a type of function

a <- 5 + 6

`+` <- function(x, y) {
  return(12)
}

5 + 6
5 + "hi"
rm(`+`)

print <- 5
print

print("Hello world")

print <- function(x) { return("no way out?")}

print("hello world")

# Packages -----
# Note: a `-----` sequence at the end of the comment adds an entry to
# the document outline.

# Another crucial usage of R is to run packages:
iris_reg <- lm(Sepal.Length ~ Petal.Width, data = iris)
summary(iris_reg)

# Data structures: vector -----

# R operates on named data structures. A simple such structure is the numeric 
# vector, which is a single entity consisting of an ordered collection of numbers. 

# To set up a vector named `x`, say, consisting of six numbers, 
# namely 10.4, 5.6, 3.1, 6.4, 21.7 and 5.8 use the R command:
x <- c(10.4, 5.6, 3.1, 6.4, 21.7, 5.8)
# or 
x = c(10.4, 5.6, 3.1, 6.4, 21.7, 5.8)

# `x` is an atomic vector of size / dimension (what size?) 6
# `c` is a function

# *ordered* means that we can approach elements of a vector by index:
x[0]
x[0] <- 5
x[0]

x[i]



# As we've said, R is 1-based
x[1]

# What about the following?
# (don't just rush through this - think and provide an educated
# guess before you run each command. Which of the results conformed 
# with your guesses? Which surprised you? Why were you surprised?)
x[-2]
x[1000]

rep(5, 100)

5
# Moral: scalars in R are vectors of length 1!!!!
a <- 5

x[-5]

`[`(x, 5)
"["(x, 5)


x[-(0-5)]
x[+(0-5)]

x[-Inf]


x[2.48]
x[2.52]

x[1:2]

":"(2, 4)

x[c(1, 1, 5, 2, 1)]

x[TRUE]
x[FALSE]

# So what is this doing?!
x[c(TRUE, TRUE, FALSE)] # the vector is **recycled** two times
x[c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE)]

x[c(1, TRUE)]

c(1, TRUE) # TRUE is **recasted** to the numeric value 1

# Recasting goes from "simple" types to "complex" types.
c(1, "ha!")

x[c(1, FALSE)]

x[c(1, 1)]

x[c(2, TRUE)]


# If we want to define scalars, we can either use
y = 5 # or y <- 5
# or
y = c(5)

# The moral is that scalars are the same as 1-dim vectors. I.e:
y
# us the same as 
y[1]

y == y[1]

# try:
x[y] 

# What is
y[5]
# ?

# Answer:
# Other languages may have returned an error. R simply returns NA, 
# an actual (and important) value, standing for Not Available.

# Try the above commands from the console and then from a script (ctrl+enter).
# Examine the global environment.

# What do we get from the following?
y / 0
-y / 0
y / (-0)
y / (--0)
0 / 0
0 * Inf

# So we have NaN (not a number) which is a scalar value.

# We have also seen (not applicable). Let's explore:

TRUE | FALSE

TRUE & NA # it's either TRUE & TRUE = TRUE; TRUE & FALSE = FALSE
TRUE | NA # it's either TRUE | TRUE = TRUE; TRUE | FALSE = TRUE

NA == 5
NaN == 5

NA > 5
NaN > 5

10 * NA
10 * NaN

!NA
!NaN

# Exceptions:
NA ^ 0
0 ^ 0 

NaN ^ 0

NA | TRUE
NaN | TRUE

NA & FALSE
NaN & FALSE

# Try on your own:

# Propagation of missingness leads to a common mistake when
# determining which values in a vector are missing:
  
z <- c(NA, 5, 1, 10)

z == c(1, 5)
z == c(1, 5, 1 , 5)

z == NA

is.na(z)

# (See https://www.r-bloggers.com/difference-between-na-and-nan-in-r/ for more details)

# Part of the reason that R is so popular, is its flexible and powerful
# built-in vector arithmetic:
x + y
x * y
x ^ y
# and also:
y ^ x

# The built-in vector arithmetic is SO powerful, that it sometimes may 
# produce unexpected results. Example:

y = c(2, y) # What does this line do?
x * y
# What just happened here?

# Another type of vectors are logical vectors:
x < 6
# The result is a logical vector of length...

# Another extremely useful feature is the ability to subset vectors 
# in various ways, e.g. according to logical vectors:
x[x < 6]
# The result is a numerical vector of length 3

# Data structures: matrix -----

matrix(0, 1, 1)
help(matrix)
matrix(0, 1, 1) == 0
# What kind of object did we get from the above?
# Answer: matrix, it is different from vector in that it has row and column indices.

# Verify your answer using `is.matrix`:
M = (matrix(0, 1, 1) == 0)
is.matrix(M)

matrix(c(0, 2), 1, 1)
matrix(1:100, 1 , 1)
matrix(1:100, 2, 50)
matrix(1:100, 50, 2)
matrix(1:100, 50, 3)
matrix(1:73, 50, 3)

A = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3)
View(A)
# Matrices also have strong arithmetic commands:
A + 1
s = A ^ 5
# What happened in the above?

# And here?
A / c(2, 4, 5)
# Answer: R divides matrices by vectors COLUMN-WISE

# What do we learn from this example?
A / 3
A / 3 == 1
A[A/3 < 1.5 & A > 1]
# Answer: two element by element comparisons yield two logical matrices,
# which are then logical-and element by element. The resulting logical matrix 
# is used to subset A.

# We can approach columns, rows and cells directly:
A[1:2, ]
A[2, ]
A[ , c(1, 3, 1, 3, 3, 3)]
A[, 1]
A[2, 1]
# What is the type of data structure we got in each of the above?

B = matrix(7:12, nrow = 2)
A * B
B * A

A %*% B
A %*% t(B) # What does the function t do? What does the function %*% do?

B.t <- 6

C = matrix(seq(13, 19, 2), nrow = 2, ncol = 2, byrow = TRUE) # Examine help(seq)...


# What do you expect from each of the following lines?
A * C
C * A

A[ , 1:2] * C
C * A[ , 1:2]

A %*% C
C %*% A

# To sum, there are many strong and flexible operations. Mastery of those 
# can be extremely rewarding in creating elegant and fast scripts. However, users
# have to use extreme caution in applying them!