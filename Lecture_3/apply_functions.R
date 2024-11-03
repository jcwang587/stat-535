# Hw2, part 2, q 5, q 8:

# Q5:

rnorm(10000) # standard normal - mean 0, var 1
# between a and b

# a = 0.5, b = Inf

# mu, sigma, a, b
dnorm(b) # phi 
dnorm(a)
pnorm(a) # Phi
pnorm(b)

b <- Inf
dnorm(4)
dnorm(5)
dnorm(6)
# "phi(infity)" = lim phi(x) where x -> infinity

# When we compute the mean of the truncated normal, we can extend the two-sided formula with computations based
# on "phi(infity)" or "Phi(infity)"
b <- Inf
dnorm(b)
pnorm(b)

b * dnorm(b)

# Q8:
rnorm(n = 10, mean = 0, sd = 1)
runif(n = 20, min = -1, max = 1)

rnorm(10000) > 0.5

# Apply functions -----

# apply()

A <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, byrow = TRUE)
A
# Easy:
apply(A, MARGIN = 1, mean)
apply(A, MARGIN = 2, mean)
# easy because the output of the mean function is a scalar.

# What happens if we use a function whose output is a vector?
cumsum(1:3) 

A
apply(A, MARGIN = 1, cumsum)
# cumcum function returns a vector (one for each row of A). These vectors are returned in a matrix, 
# column by column.
t(apply(A, MARGIN = 1, cumsum))

# please compute "cummean". I.e. the first element in the output will be itself. The second the average of the
# first and sceond. etc.
apply(A, MARGIN = 1, cumsum)

# for one vector:
cumsum(c(1, 2, 3)) / 1:3

t(apply(A, MARGIN = 1, cumsum) / 1:3)

# lapply:
l <- list(c(1), c(1, 7), c(8, 3, 5, 7, 8))
lapply(l, cumsum)

l[[4]] <- "charcater!"
lapply(l, function(x) { sum(x^2) })

library(pointr)


