# Rewriting R code in C++ (based on Advanced R)

# Introduction
# ===

# Rcpp makes it very simple to connect C++ to R. 

# Typical bottlenecks that C++ can address include:
  
# - Loops that can't be easily vectorised because subsequent iterations depend on previous ones.

# - Recursive functions, or problems which involve calling functions millions of times. The
#   overhead of calling a function in C++ is much lower than in R.

# - Problems that require advanced data structures and algorithms that R doesn't provide. Through
#   the standard template library (STL), C++ has efficient implementations of many important data
#   structures, from ordered maps to double-ended queues.

# Getting started with C++
# ===

# First, load the `Rcpp` library:

library(Rcpp)

# `cppFunction()` allows writing C++ functions in R:

Rcpp::cppFunction(
'void RcppHelloWorld() {
  Rcout << "Hello World";
}')

# C++ is a compiled language. That is, code has to be compiled first and stored in 
# an executable form before we can use it. 

# cppFunction compiles the code that is passed to it as a charachter argument, and constructs 
# an R function that connects to the compiled C++ function.

# cppFunction implicitly runs the following lines:
# ```
# #include <Rcpp.h>
# using namespace Rcpp;
# ```

# The first line tells the C++ compiler we want to use Rcpp objects.
# The second line tells the compiler to look for objects that we call in the Rcpp library without 
# us being explicit. E.g., instead of having to type each time `Rcpp::Rcout` we can call `Rcout` directly.


# Once we run the above, the general environment includes the function `RcppHelloWorld`.

# Examine its contents:

RcppHelloWorld

# to see that it is stored somewhere, the call from R is a pointer to the executable object.

# Run it!

RcppHelloWorld()

# To define a function in C++ we use the following template:

# RETURN_TYPE FUNCTION_NAME(INPUT_TYPE INPUT, INPUT_TYPE INPUT, ...){
#   
#   //do something
#   
#   return RETURN_VALUE;
# }
# ```

# In our Hello World example we use a special type `void` that translates in R to `NULL`:
a <- RcppHelloWorld()
a


# Another example:

cppFunction('
int add_integers(int x, int y, int z) {
  int sum;
  sum = x + y + z;
  return(sum);
}
')

add_integers(1, 2, 3)
add_integers(1, 2, 3.5)
add_integers(1, 2, NA)
is.integer(Inf)
add_integers(1, 2, Inf)
add_integers(1, 2, "a")

cppFunction('
double add_doubles(double x, double y, double z) {
  double sum = x + y + z;
  return(sum);
}
')

add_doubles(1, 2, 3)

is.integer(add_doubles(1L, 2L, 3L))


add_doubles(1, 2, 3.5)
add_doubles(1, 2, NA)
add_doubles(1, 2, Inf)
add_doubles(1, 2, "a")


# Recall, in R:
is.integer(Inf)


is.double(Inf)

# When we add integers in R, it recasts our data behind the scenes if needed. In the following examples the
# integers 1L and 2L are converted to doubles:
1L + 2L + 3.5
is.integer(1L + 2L + Inf)

# And although R has a specific `NA` value for integers, C++ does not (though it has an NA value for floats):
1L + 2L + NA
is.integer(1L + 2L + NA)
add_integers(1, 2, NA)
add_doubles(1, 2, NA)

# The C++ function is much less forgiving compared to R functions. It will not recast data in the same way that R does. 
# As a matter of fact, this is one of the reasons why C++ compiled code is faster: 
# less overheads whenever we call a function. 

# A few more points about coding in C++:
# 1. Every line in C++ code has to end with a semicolon `;`
# 2. Every variable has to be declared before it may be used:
# E.g., we can't simply write:
# ```
# sum = x + y + z;
# ```
# but have to:
# ```
# int sum;
# sum = x + y + z;
# ```
# (where were x, y and z declared in our add functions?)
# (what is the slight difference in declaration style between the integer and float versions?)
# 3. The type of variable a functions returns is predetermined.
# ```
# int add_integers(...) {
#   int sum
#   return(int)
# }
# ```
# 4. `return`` statements must be explicit:
# In R, the two following funtions are equivalent:
foo <- function() { return(5) }
foo()
hoo <- function() { 5 }
hoo()

# In C++, the first version would not compile:
cppFunction(
  'int badFoo() {
    5;
   }')

badFoo()
# The second will:
cppFunction(
'int goodFoo() {
    return(5);
   }')
goodFoo()


x <- 5
typeof(x)
y <- c(5, 6)
typeof(y)

# 5. Scalars and vectors are different:
#   - The scalar equivalents of numeric, integer, character, and logical vectors are:
#                               double, int, String, and bool.
#   - The classes for the most common types of R vectors are:
#     NumericVector, IntegerVector, CharacterVector, and LogicalVector.



# In the following we will translate simple R functions to their C++ equivalents. 

# Scalar input, scalar output
# ===

# A scalar version of the sign() function which returns 1 if the integer input is positive, and -1 if it’s negative:
# An R version:
signR <- function(x) {
  if (x > 0) {
    1
  } else if (x == 0) {
    0
  } else {
    -1
  }
}

signR(155)

cppFunction('
int signC(int x) {
  if (x > 0) {
    return(1);
  } else if (x == 0) {
    return 0;
  } else {
    return -1;
  }
}')

signR(4)
signR(0)
signR(-4)


signC(4)
signC(0)
signC(0.01)
signC(-4)

# Exercise: what do you need to change in order to handle `Inf` and `-Inf`?
signC(Inf)
signC(-Inf)


cppFunction('
int signCd(double x) {
  if (x > 0) {
    return(1);
  } else if (x == 0) {
    return 0;
  } else {
    return -1;
  }
}')

signCd(1L)
signCd(Inf)
signCd(-Inf)
signCd(0.01)

# In the C++ version:

# The `if` syntax is identical - while there are some big differences between R and C++,
# there are also lots of similarities!

# C++ also has a while statement that works the same way as R’s. As in R you can use break
# to exit the loop, but to skip one iteration you need to use continue instead of next.

# Vector input, scalar output
# ===

# One big difference between R and C++ is that the cost of loops is much lower in C++.
# For example, we could implement the sum function in R using a loop. If you’ve been
# programming in R a while, you’ll probably have a visceral reaction to this function:
  
sumR <- function(x) {
  total <- 0
  for (i in x) {
    total <- total + i
  }
  return(total)
}



# In C++, loops have very little overhead, so it’s fine to use them.

# C++ x.size() is equivalent to R's length(x)


# C++: ++i is equivalent to i <- i + 1 in R

# C++:
# int j = 0;
# int i = 5;
# j = i++;  // j == 5, i == 6
# j = ++i; // j == 6, i == 6

cppFunction('
double sumC(NumericVector x) {
  int n = x.size();
  double total = 0;
  for(int i = 0; i < n; ++i) {
    total += x[i];
  }
  return total;
}')

# The C++ version is similar, but:
  
# - To find the length of the vector, we use the .size() method, which returns an integer.
#   In general, C++ methods are called with . (i.e., a full stop).

# The `for`` statement has a different syntax:
# ```
# for(init; check; increment)
# ```
# - This loop is initialized by creating a new variable called `i` with value 0.
#   Before each iteration we check that `i < n``, and terminate the loop if it isn't.
#   After each iteration, we increment the value of i by one, using the special prefix
#   operator ++ which increases the value of i by 1.

# - In C++, vector indices start at 0, which means that the last element is at position `n - 1`.

# - Use `=` for assignment, not `<-`.

# - C++ provides operators that modify in-place:
#   total += x[i] is equivalent to total = total + x[i].

#   Similar in-place operators are -=, *=, and /=.

# This is a good example of where C++ is much more efficient than R. As shown by the
# following microbenchmark, sumC() is competitive with the built-in (and highly optimised)
# sum(), while sumR() is several orders of magnitude slower.

x <- runif(1e3)
microbenchmark::microbenchmark(
  sum(x),
  sumC(x),
  sumR(x)
)

# Scoping in C++
# ===
# Variables exist ONLY in the scope in which they were defined. 

# Given vector x and minimal value of m, return the first index of x for which x is greater or equal to m.
# If none, return 0.

anyAboveR <- function(x, m) {
  for (i in 1:length(x)) {
    if (x[i] >= m) {
      break()
    }
  }
  if (i < length(x)) {
    return(i)    
  } else {
    return(0)
  }
}

x <- 2:18
m <- 5.5
anyAboveR(x, m)

# Exercise:
# Translate this code to C++

# x.size() -> length of a NumericVector (or any other Rcpp vector type)

cppFunction('
int anyAboveC(NumericVector x, double m) {
  int i;
  for (i = 0; i < x.size(); ++i) {
    if (x[i] >= m) {
      break;
    }
  }
  if (i < x.size()) {
    return( i + 1 );
  } else {
    return(0);
  }
}
')

x <- 2:18
m <- 5.5
anyAboveC(x, m)

# Vector input, vector output
# ===

# Next we’ll create a function that computes the Euclidean distance between a value
# and a vector of values:
  
pdistR <- function(x, ys) {
  return(sqrt(  (x - ys)^2 ))
}

# In R, it’s not obvious that we want x to be a scalar from the function definition, 
# we would need to make that clear in the documentation.

# In the C++ version because we have to be explicit about types:
  
cppFunction('
NumericVector pdistC(double x, NumericVector ys) {
  int n = ys.size();
  NumericVector out(n);

  for(int i = 0; i < n; ++i) {
    out[i] = sqrt(pow(ys[i] - x, 2.0));
  }
  return out;
}')

# Some new concepts:
  
# We create a new numeric vector of length `n` with a **constructor**:
# ```
# NumericVector out(n);
# ```

# Another useful way of making a vector is to copy an existing one: 
# ```
# NumericVector zs = clone(ys)
# ```

# - C++ uses pow(), not ^, for exponentiation.

# Because the R version is fully vectorised, it's already going to be fast:

x <- 0.5
y <- runif(1e6)
microbenchmark::microbenchmark(
  pdistR(0.5, y),
  sqrt(  (x - y)^2 ),
  pdistC(0.5, y)
)

# Using sourceCpp
# ===

# So far, we’ve used inline C++ with cppFunction().

# It is usually easier to use stand-alone C++ files and then source them into R using sourceCpp().
# This lets you take advantage of text editor support for C++ files (e.g., syntax highlighting) as
# well as making it easier to identify the line numbers in compilation errors.

# Your stand-alone C++ file should have extension .cpp, and needs to start with:
# ``` 
# #include <Rcpp.h>
# using namespace Rcpp;
# ```

# For each function that you want available within R, you need to prefix it with:
# ```
# // [[Rcpp::export]]
# ```

# You can embed R code in special C++ comment blocks. This is really convenient if you want to run some test code:
  
# /*** R
# # This is R code
# */
  
# To compile the C++ code, use 
# ```
# sourceCpp("path/to/file.cpp")
# ```


sourceCpp("timesTwo.cpp")

timesTwo(c(1,2,3))

# This will create the matching R functions and add them to your current session.
# Note that these functions can not be saved in a .Rdata file and reloaded in a later session;
# they must be recreated each time you restart R.

# For example, saving the following block of code in a file called "meanC.cpp" using sourceCpp
# to compile it and make it acessible to R implements mean in C++ and then compares
# it to the R built-in mean():

# ```
#   #include <Rcpp.h>
#   using namespace Rcpp;
# 
# // [[Rcpp::export]]
# double meanC(NumericVector x) {
#   int n = x.size();
#   double total = 0;
# 
#   for(int i = 0; i < n; ++i) {
#     total += x[i];
#   }
#   return total / n;
# }
# 
# /*** R
# x <- runif(1e5)
# microbenchmark::microbenchmark(
#   mean(x),
#   meanC(x)
# )
# */
# ```

sourceCpp("meanC.cpp")

# Notice that meanC() is faster than the built-in mean(). This is because it trades numerical accuracy for speed.

# For the remainder of this lecture C++ code will be presented stand-alone rather than wrapped in a
# call to cppFunction. To try compiling and/or modifying the examples you should paste them into a C++ source file
# that includes the elements described above. 

# This is easy to do in RMarkdown: all you need to do is specify `engine = "Rcpp"`` - see `Lecture11_CPP_EXAMPLE.Rmd`

# Exercises
# ===

# For each of the following functions, read the code and figure out what the corresponding base R function is (if
# you are not sure which R function it corresponds to, succinctly summarize what the function does).
# You might not understand every part of the code yet, but you should be able to figure out the basics of what the function does.

# double f1(NumericVector x) {
#   int n = x.size();
#   double y = 0;
#   
#   for(int i = 0; i < n; ++i) {
#     y += x[i] / n;
#   }
#   return y;
# }
mean()

# NumericVector f2(NumericVector x) {
#   int n = x.size();
#   NumericVector out(n);
# 
#   out[0] = x[0];
#   for(int i = 1; i < n; ++i) {
#     out[i] = out[i - 1] + x[i];
#   }
#   return out;
# }
cumsum()




# bool f3(LogicalVector x) {
#   int n = x.size();
# 
#   for(int i = 0; i < n; ++i) {
#     if (x[i]) return true;
#   }
#   return false;
# }
# ```
any()

# Convert the following functions into C++. For now, assume the inputs have no missing values.

all()
sourceCpp("allC.cpp")
# Answer:
""


# Check with:
allC(rep(TRUE, 5))
all(rep(TRUE, 5))

allC(c(rep(TRUE, 5), FALSE))
all(c(rep(TRUE, 5), FALSE))


cumprod()
# Answer:






sourceCpp("cumprodC_live.cpp")

# Check with:
cumprod(c(1, 2, 3, 4))
cumprodC(c(1, 2, 3, 4))

X <- rexp(1000, 0.7)
all.equal(cumprod(X), cumprodC(X))

var()
# Answer:
""

# Check with:
var(2.5)
varC(2.5)

var(c(1,3))
varC(c(1,3))

U <- runif(10000)
all.equal(var(U), varC(U))


# Lists and data frames
# ===

# Rcpp provides List and DataFrame classes.

# They are more useful for output than input. This is because lists and data frames
# can contain arbitrary classes but C++ needs to know their classes in advance.
# If the list has known structure, you can extract the components and manually convert
# them to their C++ equivalents with as().

# For example, the object created by lm(), the function that fits a linear model,
# is a list whose components are always of the same type. The following code
# illustrates how you might extract the mean percentage error (mpe()) of a linear
# model. This isn’t a good example of when to use C++, because it’s so easily
# implemented in R, but it shows how to work with an important class.

# Note the use of .inherits() and the stop() to check that the object really is a
# linear model.

cppFunction('
double mpe(List mod) {
  if (!mod.inherits("lm")) stop("Input must be a linear model");
  
  NumericVector resid = as<NumericVector>(mod["residuals"]);
  NumericVector fitted = as<NumericVector>(mod["fitted.values"]);
  
  int n = resid.size();
  double err = 0;
  for(int i = 0; i < n; ++i) {
    err += resid[i] / (fitted[i] + resid[i]);
  }
  return err / n;
}
')

mod <- lm(mpg ~ wt, data = mtcars)
mpe(mod)

# More accessible information can be found at 
# https://teuder.github.io/rcpp4everyone_en/


# Distributions in Rcpp
# ===
# In Rcpp, probability distribution functions with the same name are defined in two namespaces, R:: and Rcpp::.
# These differences are that the function defined in Rcpp:: namespace returns a vector, while the function in
# the R:: namespace returns a scalar. 

# The probability distribution functions defined in the Rcpp:: namespace has the same functionalities as those in R.

# The basic structures of the probability distribution functions defined in the Rcpp:: namespace are:
# ```
# NumericVector Rcpp::dXXX( NumericVector x, double par,                    bool log = false )
# NumericVector Rcpp::pXXX( NumericVector q, double par, bool lower = true, bool log = false )
# NumericVector Rcpp::qXXX( NumericVector p, double par, bool lower = true, bool log = false )
# NumericVector Rcpp::rXXX(           int n, double par )
# ```

# The basic structures of the probability distribution functions defined in the R:: namespace are:
# ```
# double R::dXXX( double x, double par,            int log )
# double R::pXXX( double q, double par, int lower, int log )
# double R::qXXX( double p, double par, int lower, int log )
# double R::rXXX(           double par )
# ```


# (Generally speaking, every function in the Rmath library can be called from the `R` namespace.
# https://github.com/RcppCore/Rcpp/blob/82d8a3a922fc158f2403b233b2ac6861c538d514/inst/include/Rcpp/Rmath.h)


# Exercise:
# ===
# Write a C++ function that computes the negative log likelihood value of a numeric vector for a gamma distribution
# with some shape and scale.

# Answer:
neg_log_lik_gamma_R <- function(x, shape, scale) {
  -sum(dgamma(x, shape = shape, scale = scale, log = TRUE))
}

cppFunction('
double neg_log_lik_gamma_C(NumericVector x, double shape, double scale) {
  return(-sum(Rcpp::dgamma(x, shape, scale, true)));
}
')

neg_log_lik_gamma_R(MASS::cats$Hwt, 19, 1)
neg_log_lik_gamma_C(MASS::cats$Hwt, 19, 1)


# A taste of RcppArmadillo
# ===

# - Rcpp classes do not provide us with the tools to avoid element-wise implementations of
#   many linear algebra computations and some lower level element wise operations.
# - Programming these methods by hand can be tedious and error-prone.

# - Armadillo is a C++ library for linear algebra and scientific computing.
# - Its syntax was built so that it resembles Matlab.
# - See http://arma.sourceforge.net/docs.html for very convenient documentation.

# - RcppArmadillo allows easy and fast access from R to Armadillo objects.


# - The main classes of interest are:
# ```
# arma::mat
# arma::vec (same as arma::colvec)
# arma::rowvec
# ```
# Which are clases for matrix and vector whose elements are of `double` type.
# The `vec` class is for column vectors, `rowvec` class for row vectors.
# I.e., unlike R (`numeric`) and Rcpp (`NumericVector`), `vec` and `rowvec` are in 
# essence 1 column (or 1 row) matrices.

# RcppArmadillo integrates Rcpp with Armadillo and allows easy back and forth conversion between R, Rcpp and Armadillo classes.

# Including RcppArmadillo
# ===

# Instead of 
# ```
# #include <Rcpp.h>
# ```

Use
# ```
# #include <RcppArmadillo.h>
# // [[ Rcpp :: depends ( RcppArmadillo )]]

# using namespace Rcpp;
# using namespace arma;
# ```

# Including these statements replaces the call to `Rcpp.h` which becomes impicit now.

# Examples:

cppFunction('
bool check_stop_deriv_Rcpp(NumericVector x, NumericVector stopping_deriv) {
  NumericVector abs_x = abs(clone(x));
  for (int i = 0; i < x.size(); ++i) {
    if (abs_x[i] > stopping_deriv[i]) {
      return false;
    }
  }
  return true;
}
')

library(RcppArmadillo)
cppFunction('
bool check_stop_deriv_arma(arma::vec x, arma::vec stopping_deriv) {
  return arma::all(abs(x) < stopping_deriv);
}
',
depends = "RcppArmadillo")


check_stop_deriv_Rcpp(runif(10, 0, 0.1), rep(0.1, 10))
check_stop_deriv_arma(runif(10, 0, 0.1), rep(0.1, 10))

N <- 100000
microbenchmark::microbenchmark(
  check_stop_deriv_Rcpp(runif(N), rep(0.1, N)),
  check_stop_deriv_arma(runif(N), rep(0.1, N))
)
# No substantial difference in performance, but syntax for Armadillo objects is much easier.


# Matrix multiplication is not defined in Rcpp, but is in Armadillo which is designed for linear algebra. E.g.:

A * B

cppFunction('
arma::mat matrix_multiply_element_arma(arma::mat A, arma::mat B) {
  return(A % B);
}',
depends = "RcppArmadillo")


A %*% B

cppFunction('
arma::mat matrix_multiply_arma(arma::mat A, arma::mat B) {
  return(A * B);
}',
depends = "RcppArmadillo")


(A = matrix(1:6, nrow = 3))
(B = matrix(7:14, nrow = 2))

matrix_multiply_arma(A, B)
matrix_multiply_arma(B, A)

matrix_multiply_element_arma(A, B)
t(B)[1:3, ]
matrix_multiply_element_arma(A, t(B)[1:3, ])


# Exercise:
# ===

# For these exercises, you need to find several things:
# 1. How to access vector/matrix elements in Rcpp / Armadillo.
# 2. How to initialize a vector of a certain length ("constructor")
# 3. How to find out the number of rows and the number of columns of a matrix.

# https://teuder.github.io/rcpp4everyone_en/080_vector.html
# member functions

# Consult the Rcpp documentation. Write a function that takes as input a `NumericMatrix`, and returns an 
# integer vector of length 2, whose first element is the number of rows of the matrix and second element is the number
# of columns of the matrix.

# Answer:
cppFunction('
IntegerVector mat_dim_rcpp(NumericMatrix A) {
  IntegerVector d(2);
  d[0] = A.nrow();
  d[1] = A.ncol();
  return(d);
} 
')

mat_dim_rcpp(A)
mat_dim_rcpp(B)

# http://arma.sourceforge.net/docs.html
# http://arma.sourceforge.net/docs.html#attributes

# Integer vector in Armadillo:  arma::uvec


# Consult the Armadillo documentation. Write a function that takes as input an `arma::mat` matrix, and returns an 
# integer (column) vector of length 2, whose first element is the number of rows of the matrix and second element is the number
# of columns of the matrix.

# Answer:
cppFunction('
arma::uvec mat_dim_arma(arma::mat A) {
  arma::uvec d(2);
  d(0) = A.n_rows;
  d(1) = A.n_cols;
  return(d);
}            
',
            depends = "RcppArmadillo")


a <- mat_dim_arma(A)
c(a)
is.vector(a)
is.matrix(a)

mat_dim_arma(B)


# Conversions between Rcpp and arma classes:
# ===

# 1. From Rcpp::NumericMatrix to arma::mat:
# ```
# NumericMatrix x;
# arma::mat y = as<arma::mat>(x) ;
# ```

# From arma::mat to Rcpp::NumericMatrix:
# ```
# arma::mat x
# NumericMatrix y = wrap(x);
# ```
