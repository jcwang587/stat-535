# Lecture 4 - Part II - Strings

# - Introduction to strings and string operations.
# - Extracting and manipulating string objects.


# Many datasets we deal with are in character form!

# - Web pages can be scraped.
# - Email can be analyzed for network properties.
# - Survey responses must be processed and compared.

# Even if you only care about numbers, it helps to be able to extract 
# them from text and manipulate them easily.


# Simplest Distinction -----

# ***Character***: a symbol in a written language, specifically what you can enter at a
# keyboard: letters, numerals, punctuation, space, newlines, etc.

# 'L', 'i', 'n', 'c', 'o', 'l', 'n'

# ***String***: a sequence of characters bound "together"
"Lincoln"

# Note: R does not have a separate type for characters and strings:

typeof("L")
typeof("Lincoln")
class("Lincoln")

# Making Strings -----

# Use single or double quotes to construct a string; 
# use base R `nchar()` or `stringr::str_length()` to find the number of characters in a string:

nchar("Lincoln")
library(tidyverse)
str_length("Lincoln")

nchar(c("Abraham", "Lincoln", NA))
str_length(c("Abraham", "Lincoln", NA))

nchar("Lincoln's")
str_length("Lincoln's")

nchar("\'Four\'")
str_length("\'Four\'")

# Whitespace -----

# The space, `" "` is a character; so are multiple spaces `"   "` and the empty string, `""`.
" "
"    "
""

# Some characters are special, so we have "escape characters" to specify them in strings.

  # - quotes within strings: `\"`
  # - backslash: `\\`
  # - tab: `\t`
  # - new line `\n` and carriage return
  # - `\r` --  only carriage return, move the cursor at the start of the line 
  #    (effectively erases everything from start of line to cursor)

cat("Dear all \"I am...")
cat("Dear Sir\\Madam, I am...")
cat("Dear all \t I am...")
cat("Dear all \n I am...")
cat("Dear all \r I am...")


# You'll also sometimes see strings like "\u00b5", this is a way of writing non-English 
# characters that works on all platforms:

x <- "\u00b5"
x

# Character Data Type -----

# One of the atomic data types, like `numeric` or `logical`.
# It can go into scalars, vectors, arrays, lists, or be the type of a column in a data frame.

length("Abraham Lincoln's beard")
length(c("Abraham", "Lincoln's", "beard"))

nchar("Abraham Lincoln's beard")
nchar(c("Abraham", "Lincoln's", "beard"))

# Character-Valued Variables -----

# They work just like others, e.g., with vectors:
president <- "Lincoln"
nchar(president)
presidents <- c("Fillmore", "Pierce", "Buchanan", "Lincoln", "Johnson")
presidents[3]
presidents[-(1:3)]


# Displaying Characters: print( ) and cat( ) -----

# We know `print()`, of course; `cat()` writes the string directly to the console.
# If you're debugging, `message()` is R's preferred syntax.

print("Abraham Lincoln")
cat("Abraham Lincoln")
cat(presidents)
message(presidents)

# Use `print` to see the special characters in a string. The `cat` function will not reveal them.

s <- "first\rsecond\n"
nchar(s)
print(s)
cat(s)

# `cat` prints text on the screen, `print` returns the object passed to the
# function, and a newline. You may want to use cat to print informative messages on screen.

p <- print(presidents)
p
p <- cat(presidents)
p

# Combining Vectors into Strings -----

# Converting one variable type to another is called ***casting***: 

as.character(7.2)
7.2e5
as.character(7.2e5)
7.2e12 
as.character(7.2e12)
as.character(c(7.2, 7.2e12))  


# Building Strings from Multiple Parts -----

# The `paste()` and `stringr::str_c()` functions concatenate several strings together:

paste("Everybody", "loves", "stats.")
str_c("Everybody", "loves", "stats.")

# Use the `sep` argument to control how the strings separated:
paste("Everybody", "loves", "stats.", sep = ", ")
str_c("Everybody", "loves", "stats.", sep = ", ")

# Note: `paste0` is a special case of `paste`:
paste("Everybody", "loves", "stats.", sep = "")
paste0("Everybody", "loves", "stats.")

# The functions are very forgiving about nonstring arguments. They try
# to convert them to strings using the `as.character` function:

paste("The square root of twice pi is approximately", sqrt(2*pi))
str_c("The square root of twice pi is approximately ", sqrt(2*pi))


# If one or more arguments are vectors of strings, `paste` and `str_c` will generate all 
# combinations of the arguments:

stooges <- c("Moe", "Larry", "Curly")
paste(stooges, "loves", "stats.")
str_c(stooges, "loves", "stats.", sep = " ")

# The `paste()` function is very flexible! With one vector argument, works like `as.character()`.
# With one vector argument, works like `as.character()`.

paste(41:45)
str_c(41:45)

# With 2 or more vector arguments, combines them with recycling:
paste(presidents, 41:45)
str_c(presidents, 41:45, sep = " ")

paste(presidents, c("R","D"))  # Not historically accurate!
str_c(presidents, c("R","D"), sep = " ") # str_c is more generous in warning messages.

paste(presidents,"(", c("R","D"), 41:45, ")")
str_c(presidents,"(", c("R","D"), 41:45, ")", sep = " ")

# Exercise: what happens if you give `sep` a vector?
paste(presidents, " (", 41:45, ")", sep = c("_", "*", "&"))
str_c(presidents, " (", 41:45, ")", sep = c("_", "*", "&"))

paste(presidents, " (", 41:45, ")", sep = c("*", "&", "_"))
str_c(presidents, " (", 41:45, ")", sep = c("*", "&", "_"))

# Like most other functions in R, missing values are contagious. 

# `paste()` automatically substitutes NA values with the string "NA":
x <- c("abc", NA)
paste0("|-", x, "-|")

# Use `str_replace_na()` to do the same with `str_c()`
str_c("|-", x, "-|")
str_c("|-", str_replace_na(x), "-|")


# Objects of length 0 are silently dropped. This is particularly useful in conjunction with if:

name <- "Jane Doe"
time_of_day <- "morning"
birthday <- FALSE
str_c(
  "Good ", time_of_day, " ", name,
  if (birthday) " and HAPPY BIRTHDAY",
  "."
)
paste0(
  "Good ", time_of_day, " ", name,
  if (birthday) " and HAPPY BIRTHDAY",
  "."
)


birthday <- TRUE
str_c(
  "Good ", time_of_day, " ", name,
  if (birthday) " and HAPPY BIRTHDAY",
  "."
)
paste0(
  "Good ", time_of_day, " ", name,
  if (birthday) " and HAPPY BIRTHDAY",
  "."
)

# Condensing Multiple Strings -----

# Producing one big string with `collapse`:
paste(presidents, " (", 41:45, ")", sep = "", collapse = "; ")
str_c(presidents, " (", 41:45, ")", sep = "", collapse = "; ")

# Default value of `collapse` is `NULL`, that is, it won't use it.


# Replacing Substrings -----

# Use sub to replace the *first instance* of a substring:
s <- "Curly is the smart one. Curly is funny, too."
sub("Curly", "Moe", s)

# Use `gsub()` to replace *all instances* of a substring:
gsub("Curly", "Moe", s)

# To remove a substring altogether, simply set the new substring to be empty:
s <- "For really tough problems, you need R and SAS."
sub(" and SAS", "", s)

# Substring Operations -----

# ***Substring***: a smaller string from a big string, but still a string in its own right. 

# A string is not a vector or a list, so we ***cannot*** use subscripts like `[[ ]]` or `[ ]`
# to extract substrings; we use `substr()` instead.

phrase <- "Christmas Bonus"
substr(phrase, start = 8, stop = 12)


# We can also use `substr` to replace elements:

substr(phrase, 13, 13) <- "g"
phrase

# substr() for String Vectors -----

# `substr()` and `str_sub()` work element by element. They take start and end 
# arguments which give the (inclusive) position
# of the substring:

presidents
substr(presidents, 2, 4)   # Second to fourth characters
str_sub(presidents, 2, 4)

substr(presidents, nchar(presidents)-1, nchar(presidents))   # Last two
substr(presidents, 20, 21)    # No such substrings so return the empty string
substr(presidents, 7, 7)      # Explain!

# With `str_sub`, negative numbers count backwards from end
str_sub(presidents, -3, -1)

# Note that the functions won't fail if the string is too short: it will just return
# as much as possible:
substr("a", 1, 5)
str_sub("a", 1, 5)

# You can also use the assignment form of `str_sub()` and `substr()` to modify 
# strings:
x

str_sub(x, 1, 1) <- str_to_lower(str_sub(x, 1, 1))
x

substr(x, 1, 1) <- toupper(substr(x, 1, 1))
x

# Getting the Current Date -----
# You need to know today's date:

Sys.Date()

# The `Sys.Date()` function returns a Date object. It seems to return a string because the
# result is printed inside double quotes. `Sys.Date` returned a Date object and then R
# converted that object into a string for printing purposes.

class(Sys.Date())


# Converting a String into a Date -----
# You have the string representation of a date, such as "2017-2-14", and you want to convert 
# that into a Date object. You can use `as.Date` that assumes the string looks like yyyy-mm-dd:

as.Date("2010-12-31")
class(as.Date("2010-12-31"))
typeof(as.Date("2010-12-31"))
attributes(as.Date("2010-12-31"))


# Converting a String into a Date -----
# To handle other formats, you must specify the format parameter of `as.Date`.
# Use format="%m/%d/%Y" if the date is in American style, for instance. Using
# `as.Date("12/31/2010")` will provide the error. Here is the correct way to convert:

as.Date("12/31/2010")
as.Date("12/31/2010", format = "%m/%d/%Y")

# Creating a Sequence of Dates -----
# You want to create a sequence of dates, such as a sequence of daily, monthly,
# or annual dates. The `seq` function is a generic function that has a version
# for Date objects. It can create a Date sequence similarly to the way it creates
# a sequence of numbers.

s <- as.Date("2019-01-01")
e <- as.Date("2019-02-01")
seq(from = s, to = e, by = 1)
seq(from = s, to = e, by = 7)

# Another typical use specifies a starting date (`from`), increment (`by`),
# and number of dates (`length.out`):

seq(from = s, by = 1, length.out = 7) # Dates, one day apart

# The increment (`by`) is flexible and can be specified in days, weeks, 
# months, or years:

seq(from = s, by = "month", length.out = 12) # First of the month for one year
seq(from = s, by = "3 months", length.out = 4) # Quarterly dates for one year

seq(from = s, by = "year", length.out = 10) # Year-start dates for one decade
seq(from = s, by = "10 years", length.out = 10) # Year-start dates for one century

# Be careful with `by="month"` near month-end. In this example, the end of February
# overflows into March, which is probably not what you want:

seq(as.Date("2010-01-29"), by = "month", len = 4)

# Example 1: -----
# Suppose we want to create five files, q1.pdf through q5.pdf, consisting of
# histograms of 100 random N(0,i) variates.

for (i in 1:5) {
 fname <- file.path(paste("q", i, ".pdf", sep = ""))
 pdf(fname) # pdf() starts graphics device. 
 hist(rnorm(100, sd = i))
 dev.off() # dev.off() shuts down the specified (by default the current) device. 
 }


# Example 2: -----
# Suppose we wish to test for a specified suffix in a filename.

testsuffix <- function(fn, suff) {
  parts <- strsplit(fn, ".", fixed = TRUE)
  nparts <- length(parts[[1]])
  return(parts[[1]][nparts] == suff)
}
testsuffix("HW1.html", suff = "html")
testsuffix("HW1.pdf", suff = "html")


# sprintf( ) -----
# The call `sprintf()` assembles a string from parts in a formatted manner. The function
# says to first print "the square of" and then print the decimal value of i:

i <- 8
s <- sprintf("the square of %d is %d", i, i^2)
s


# Using `sprintf` function in the earlier example:
for (i in 1:5) {
 fname <- sprintf("q%d.pdf", i)
 pdf(fname)
 hist(rnorm(100, sd = i))
 dev.off()
}