# Factors =====

# This chapter is based on Chapter 15 in R for Data Science and Chapter 3.4.1 in Advanced R

# Basic definitions =====

# A factor is a vector that can contain only predefined values. It is used to
# store categorical data.

# Factors are built on top of an integer vector with two attributes: 
# - a class, "factor", which makes it behave differently from regular integer vectors
# - levels, which define the set of allowed values.

x <- factor(c("a", "b", "b", "a"))
x

typeof(x)
attributes(x)

str(x)

# Factors are useful when you know the set of possible values but they’re not
# all present in a given dataset. In contrast to a character vector, when you
# tabulate a factor you’ll get counts of all categories, even unobserved ones:
  
(gender_char <- c("m", "m", "m"))
""
""
""
""
""
""


# Ordered factors =====
# Ordered factors are a minor variation of factors. 

# - they behave like regular factors, but the order of the levels is 
# meaningful (low, medium, high) (a property that is automatically leveraged
# by some modelling and visualization functions).

grade <- ordered(c("b", "b", "a", "c"), levels = c("a", "b", "c"))
grade

grade <- ordered(c("b", "b", "a", "c"), levels = c("c", "b", "a"))
grade

str(grade)


# In base R you tend to encounter factors very frequently because many base R functions
# (like read.csv() and data.frame()) automatically convert character vectors to factors.

# This is suboptimal because there’s no way for those functions to know the set of all
# possible levels or their correct order: the levels are a property of theory or
# experimental design, not of the data. Instead, use the argument stringsAsFactors = FALSE
# to suppress this behavior, and then manually convert character vectors to factors using
# your knowledge of the “theoretical” data. 

# While factors look like (and often behave like) character vectors, they are built on top
# of integers. So be careful when treating them like strings. Some string methods (like
# gsub() and grepl()) will automatically coerce factors to strings, others (like nchar())
# will throw an error, and still others will (like c()) use the underlying integer values.

# For this reason, it’s usually best to explicitly convert factors to character vectors
# if you need string-like behavior.

x
gsub(x, pattern = "a", replacement = "c")
nchar(x)
c(x, 1)


# Working with factors =====

# We’ll use the `forcats` package, which is part of the core tidyverse.
library(tidyverse)


# Creating factors

# Imagine that you have a variable that records month:

x1 <- c("Dec", "Apr", "Jan", "Mar")

# Using a string to record this variable has two problems:

# There are only twelve possible months, and there’s nothing saving you from typos:

x2 <- c("Dec", "Apr", "Jam", "Mar")

# It doesn’t sort in a useful way:
sort(x1)

# You can fix both of these problems with a factor.
# To create a factor you must start by creating a list of the valid levels:

month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

# Now you can create a factor:
""
""
""


# And any values not in the set will be silently converted to NA:
""
""


# If you want a warning, you can use readr::parse_factor():
y2 <- parse_factor(x2, levels = month_levels)

# If you omit the levels, they’ll be taken from the data in alphabetical order:
""


# To match the order of the levels with the order of the first appearance
# in the data. We can either:
""
""


# Or post-hoc:
f2 <- x1 %>% factor() %>% fct_inorder()
f2

# To access the set of valid levels directly:
levels(f2)

# For the rest of this chapter, we’re going to focus on `forcats::gss_cat`.
# It’s a sample of data from the General Social Survey, which is a long-running US survey
# conducted by the independent research organization NORC at the University of Chicago.

# The survey has thousands of questions, so in gss_cat there are a handful that will
# illustrate some common challenges you’ll encounter when working with factors.

gss_cat
# (To get more information about the variables use ?gss_cat.)

# gss_cat is a tibble.

# When factors are stored in a tibble, you can’t see their levels so easily. One way to see them is with count():
""
""



# Or with a bar chart:
""
""

# By default, `ggplot2` and `count()` will drop levels that don’t have any values. These levels still represent valid values
# that simply did not occur in this dataset.

# You can force them to display with `.drop = FALSE`:
""
""


# What should happen when we use `.drop = FALSE` with two variables?
gss_cat %>%
  count(race, year, .drop = FALSE) %>%
  tail(n = 15)

# Not ideal. That is because `year` is not a factor. Try:
gss_cat %>%
  count(race, factor(year), .drop = FALSE) %>%
  tail(n = 15)

# `.drop = FALSE` works as you hope it would with two (or more) variables only when both (or all) are factors.


# Another way: more explicitly (the `.drop` argument really belongs to `group_by`):
gss_cat %>%
  group_by(race, .drop = FALSE) %>%
  summarise(n = n())
  
# or with a bar plot:
ggplot(gss_cat, aes(race)) +
  geom_bar() +
  scale_x_discrete(drop = FALSE)

# or use `table()`:
table(gss_cat$race)

# and to complete the presentation, you can also use base R graphics `barplot()`:
""


# Exercises =====

# Based on the last example, what is the difference between the default usages for `barplot()` and
# `geom_bar()`? Provide a ggplot that performs the same as base barplot (note, you can transform a
# data frame with dplyr and use another pipe operator to send it to ggplot)

# Explore the distribution of rincome (reported income). What makes the default bar chart hard
# to understand? How could you improve the plot?

# What are the different categories for `relig`? For `partyid`?

# What is the most common relig in this survey? What’s the most common partyid?
# What is the most common combination of the two?

# Which relig does denom (denomination) apply to? How can you find out with a `dplyr`
# verbs? with `table()`? How can you find out with a visualization?


      
# Modifying factor order =====

# It’s often useful to change the order of the factor levels in a visualization.
# For example, imagine you want to explore the average number of hours spent watching
# TV per day across religions:

relig_summary <- gss_cat %>%
  group_by(relig) %>%
  summarise(tvhours = mean(tvhours, na.rm = TRUE))

ggplot(relig_summary, aes(tvhours, relig)) + geom_point()

# It is difficult to interpret this plot because there’s no overall pattern.
# We can improve it by reordering the levels of relig using fct_reorder().
# fct_reorder() takes three arguments:

    # f, the factor whose levels you want to modify.
    # x, a numeric vector that you want to use to reorder the levels.
    # Optionally, fun, a function that’s used if there are multiple values of x for each value of f. The default value is median.

relig_summary

relig_summary %>%
  mutate(relig = fct_reorder(relig, tvhours))

# Let's examine the effect of fct_reorder:
# First, on the factor as it is printed:
relig_summary$relig
fct_reorder(relig_summary$relig, relig_summary$tvhours)

# Focus on the levels:
levels(relig_summary$relig)
levels(fct_reorder(relig_summary$relig, relig_summary$tvhours))

# Have the printed values changed?
""


# Has the underlying integer vector changed?
""
""

""

# Plot it!
ggplot(relig_summary, aes(tvhours, fct_reorder(relig, tvhours))) +
  geom_point()

# In descending order:
ggplot(relig_summary, aes(tvhours, fct_reorder(relig, tvhours, .desc = TRUE))) +
  geom_point()

# Reordering religion makes it much easier to see that people in the “Don’t know” category
# watch much more TV, and Hinduism & Other Eastern religions watch much less.

# As you start making more complicated transformations, you could move them out of
# aes() and into a separate mutate() step. For example:

relig_summary %>%
  mutate(relig = fct_reorder(relig, tvhours, .desc = TRUE)) %>%
  ggplot(aes(tvhours, relig)) +
    geom_point()

# What changed?


# What if we create a similar plot looking at how average age varies across reported income level?

rincome_summary <- gss_cat %>%
  group_by(rincome) %>%
  summarise(
    age = mean(age, na.rm = TRUE)
  )

ggplot(rincome_summary, aes(age, fct_reorder(rincome, age))) + geom_point()

# Here, arbitrarily reordering the levels isn’t a good idea! That’s because rincome 
# already has a principled order that we shouldn’t mess with.

# Reserve fct_reorder() for factors whose levels are arbitrarily ordered.

# However, it does make sense to pull “Not applicable” to the front with the other
# special levels. You can use fct_relevel(). It takes a factor, f, and then any number
# of levels that you want to move to the front of the line.

ggplot(rincome_summary, aes(age, fct_relevel(rincome, "Not applicable"))) +
  geom_point()

# To illustrate more sharply:
ggplot(rincome_summary, aes(age, fct_relevel(rincome, c("Refused", "Not applicable")))) +
  geom_point()

# Another type of reordering is useful when you are coloring the lines on a plot.
# fct_reorder2() reorders the factor by the y values associated with the largest
# x values. This makes the plot easier to read because the line colors line up with the legend.

by_age <- gss_cat %>%
  filter(!is.na(age)) %>%
  count(age, marital) %>%
  group_by(age) %>%
  mutate(prop = n / sum(n))
by_age

ggplot(by_age, aes(age, prop, colour = marital)) +
  geom_line(na.rm = TRUE)

ggplot(by_age, aes(age, prop, colour = fct_reorder2(marital, age, prop))) +
  geom_line() +
  labs(colour = "marital")

# Finally, for bar plots, you can use fct_infreq() to order levels in increasing frequency:
# this is the simplest type of reordering because it doesn’t need any extra variables. You
# may want to combine with fct_rev().

gss_cat %>%
  mutate(marital = marital %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(marital)) +
    geom_bar()

# Exercises =====

# For each factor in gss_cat identify whether the order of the levels is arbitrary or principled.
# Can you reliably use the `is.ordered()` function here?


# Modifying factor levels =====

# More powerful than changing the orders of the levels is changing their values.
# This allows you to clarify labels for publication, and collapse levels for
# high-level displays. The most general and powerful tool is fct_recode(). It
# allows you to recode, or change, the value of each level. For example, take
# the gss_cat$partyid:

gss_cat %>% count(partyid)

# The levels are terse and inconsistent. Let’s tweak them to be longer and use a
# parallel construction.

gss_cat %>%
  mutate(partyid = fct_recode(partyid,
    "Republican, strong"    = "Strong republican",
    "Republican, weak"      = "Not str republican",
    "Independent, near rep" = "Ind,near rep",
    "Independent, near dem" = "Ind,near dem",
    "Democrat, weak"        = "Not str democrat",
    "Democrat, strong"      = "Strong democrat"
  )) %>%
  count(partyid)

# fct_recode() will leave levels that aren’t explicitly mentioned as is, and
#   will warn you if you accidentally refer to a level that doesn’t exist.

# To combine groups, you can assign multiple old levels to the same new level:

gss_cat %>%
  mutate(partyid = fct_recode(partyid,
    "Republican, strong"    = "Strong republican",
    "Republican, weak"      = "Not str republican",
    "Independent, near rep" = "Ind,near rep",
    "Independent, near dem" = "Ind,near dem",
    "Democrat, weak"        = "Not str democrat",
    "Democrat, strong"      = "Strong democrat",
    "Other"                 = "No answer",
    "Other"                 = "Don't know",
    "Other"                 = "Other party"
  )) %>%
  count(partyid)


# You must use this technique with care: if you group together categories that are truly
# different you will end up with misleading results.

# If you want to collapse a lot of levels, fct_collapse() is a useful variant of fct_recode().
# For each new variable, you can provide a vector of old levels:

gss_cat %>%
  mutate(partyid = fct_collapse(partyid,
    other = c("No answer", "Don't know", "Other party"),
    rep = c("Strong republican", "Not str republican"),
    ind = c("Ind,near rep", "Independent", "Ind,near dem"),
    dem = c("Not str democrat", "Strong democrat")
  )) %>%
  count(partyid)

# Sometimes you just want to lump together all the small groups to make a
# plot or table simpler. That’s the job of fct_lump():

gss_cat %>%
  mutate(relig = fct_lump(relig)) %>%
  count(relig)

# The default behavior is to progressively lump together the smallest groups, 
# ensuring that the aggregate is still the smallest group. In this case it’s
# not very helpful: it is true that the majority of Americans in this survey are
# Protestant, but we’ve probably over collapsed.

# Instead, we can use the n parameter to specify how many groups (excluding other)
# we want to keep:

gss_cat %>%
  mutate(relig = fct_lump(relig, n = 10)) %>%
  count(relig, sort = TRUE)


# Exercises =====

# How have the proportions of people identifying as Democrat, Republican, and Independent changed over time?

# Why shouldn't you use `fct_lump` to collapse rincome to fewer levels?



