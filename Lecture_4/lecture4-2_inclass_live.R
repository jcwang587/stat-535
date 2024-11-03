# Lecture 4 - Part II - Regular Expressions
# (Based on Chapter 14 of "R for Data Science")

# We will focus on the `stringr` package for string manipulation, which 
# is part of the core `tidyverse`.

library(tidyverse)

# Matching patterns with regular expressions -----

# Regexps are a very terse language that allow you to describe patterns in strings. 

# To learn regular expressions, we'll use `str_view()` and `str_view_all()`. These functions
# take a character vector and a regular expression, and show you how they match. 

# We'll start with very simple regular expressions and then gradually get more and more 
# complicated. 

# * Basic matches -----

# The simplest patterns match exact strings:

x <- c("apple", "banana", "pear")
str_view(x, "an")

# The next step up in complexity is `.`, which matches any character (except a newline):

str_view(x, ".a.")

str_view("banban", ".a.")


# But if `.` matches any character, how do you match the character `.`? 

# You need to use an "escape" to tell the regular expression you want to match it exactly, 
# not use its special behavior. 

# Like strings, regexps use the backslash, `\`, to escape special behavior. 
# To match an `.`, you need the regexp `\.`.

# Unfortunately this creates a problem. We use strings to represent regular expressions, and `\`
# is also used as an escape symbol in strings. 

# So `.` is a regular expression for any character except a newline.
# `\.` is a regular expression for the literal dot.
# "\" is the escape character for strings, and therefore "\\" is the way to denote `\` within a string.

regex("\\.")


# So to create the regular expression `\.` we need the string "\\.".

dot <- "\\."

# But the expression itself only contains one:
# show the string value:
cat(dot)

# This tells R to look for an explicit `.`:
str_view(c("abc", "a.c", "bef"), "a\\.c")

my_regex <- regex("a\\.c")
typeof(my_regex)
class(my_regex)

str_view(c("abc", "a.c", "bef"), "a.c")

# If `\` is used as an escape character in regular expressions, how do you match a literal `\`?
# We need to escape it, creating the regular expression `\\`. To create that regular expression,
# we need to use a string, which also needs to escape "\". That means to match a literal `\` 
# you need to write "\\\\" — you need four backslashes to match one!

x <- "a\\b"
writeLines(x)

str_view(x, "\\\\")


# We'll write a regular expression as `\` (ignore the ```) and strings that represent the regular expression as "\\.".

# > Exercises -----

# - How would you match the sequence "'\  ?
cat("\"\'\\\\")

str_view("\"'\\", "\"\'\\\\")

# - What patterns will the regular expression `\..\..\..` match? 
# literal-dot, any-character, literal-dot, any-character, literal-dot, any-character

str_view(c(".r.y.d", "....."), "\\..\\..\\..")

#   How would you represent it as a string?
".r.y.d"


# * Anchors -----

# By default, regular expressions will match any part of a string. It's
# often useful to anchor the regular expression so that it matches from the 
# start or end of the string. You can use:

#     `^` to match the start of the string.
#     `$` to match the end of the string.

x <- c("apple", "banana", "pear")
str_view(x, "^a")
str_view(x, "a$")

# To force a regular expression to only match a complete string, anchor it with both `^` and `$`:

x <- c("apple pie", "apple", "bad apple", "pineapple", "apble")
str_view(x, "apple")
str_view(x, "^ap.le$")


# You can also match the boundary between words with `\b`:
x <- c("summarise", "summary", "\tsum ", "opposum", "presumtuous")
str_view(x, "\\bsum\\b")


# > Exercises ----

# - How would you match the literal string "$^$" (not as a part of another string)?

str_view(c("dollarsignisliteralinstrings$^$andsoiscarret", "$^$"), "\\$\\^\\$")

# - Given the corpus of common words in stringr::words, create regular expressions that find 
#   all words that:
#     -- Start with "y".
str_view(stringr::words, "^y", match = TRUE)
#     -- End with "x"
str_view(stringr::words, "x$", match = TRUE)
#     -- Are exactly three letters long. (Don't cheat by using str_length()!)
str_view(stringr::words, "\\b...\\b", match = TRUE)
str_view("abd phd msp", "\\b...\\b", match = TRUE) # matching three letter words, potentially within a "sentence"

str_view(stringr::words, "^...$", match = TRUE)
str_view("abd phd msp", "^...$", match = TRUE)

#     -- Have seven letters or more.
str_view(stringr::words, "^.......", match = TRUE)
str_view(stringr::words, ".......", match = TRUE)

str_view(c("abc", "abcdefgt", "superkalifrajalisticexpialidouches"), "^.......", match = TRUE)
str_view(c("abc", "abcdefgt", "superkalifrajalisticexpialidouches"), ".......", match = TRUE)

str_view(stringr::words, ".......$", match = TRUE)
# Since this list is long, you might want to use the match argument to str_view() to show 
# only the matching or non-matching words.


# * Character classes and alternatives -----

# There are a number of special patterns that match more than one character. 
# `.` is one of them, which matches any character apart from a newline. 

# Four other useful tools:

#    \d: matches any digit.
#    \s: matches any whitespace (e.g. space, tab, newline).
#    [abc]: matches a, b, or c.
#    [^abc]: matches anything except a, b, or c.

# Remember, to create a regular expression containing `\d` or `\s`, you'll need to 
# escape the `\` for the string, so you'll type "\\d" or "\\s".

# A character class containing a single character is a nice alternative to backslash 
# escapes when you want to include a single metacharacter in a regex. For example, 
# look for a literal character that normally has special meaning in a regex:
str_view(c("abc", "a.c", "a*c", "a c"), "a[.]c")

str_view(c("abc", "a.c", "a*c", "a c"), ".[*]c")

str_view(c("abc", "a.c", "a*c", "a c"), "a[ ]")
str_view(c("abc", "a.c", "a*c", "a c"), "a ")

# This works for most (but not all) regex metacharacters: $ . | ? * + ( ) [ {. 
# Unfortunately, a few characters have special meaning even inside a character class 
# and must be handled with backslash escapes: ] \ ^ and -.

# You can use alternation to pick between one or more alternative patterns. For example, 
# `abc|d..f` 
# will match either '"abc"', or "deaf". Note that the precedence for `|` is low, so that 
# `abc|xyz` 
# matches `abc` or `xyz` not `abcyz` or `abxyz`:
str_view(c("abcyz", "abxyz"), "abc|xyz")

# Like with mathematical expressions, if precedence ever gets confusing, use parentheses 
# to make it clear what you want:
str_view(c("abcyz", "abxyz"), "ab(c|x)yz")
str_view(c("abcyz", "abxyz"), "ab[cx]yz")

str_view(c("grey", "gray"), "gr(e|a)y")


# > Exercises -----

# - Create regular expressions to find all words that:
#       -- Start with a vowel.
#       -- That only contain consonants. (Hint: thinking about matching "not"-vowels.)
#       -- End with ed, but not with eed.
#       -- End with ing or ise.
# - Empirically verify the rule "i before e except after c".
# - Is "q" always followed by a "u"?

# * Repetition -----

# The next step up in power involves controlling how many times a
# pattern matches:

#    ?: 0 or 1
#    +: 1 or more
#    *: 0 or more

x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
str_view(x, "CC?")
str_view(x, "CC+")
str_view(x, 'C[LX]+')

# Note that the precedence of these operators is high, so you can 
# write: colou?r to match either American or British spellings. 
# That means most uses will need parentheses, like bana(na)+.

# You can also specify the number of matches precisely:

#    {n}: exactly n
#    {n,}: n or more
#    {,m}: at most m
#    {n,m}: between n and m

str_view(x, "C{2}")
str_view(x, "C{2,}")
str_view(x, "C{2,3}")

# By default these matches are "greedy": they will match the longest 
# string possible. You can make them "lazy", matching the shortest string 
# possible by putting a ? after them. This is an advanced feature of 
# regular expressions, but it's useful to know that it exists:

str_view(x, "C{2,3}?")
str_view(x, "C[LX]+?")

# > Exercises -----

# - Describe the equivalents of ?, +, * in {m,n} form.
# - Describe in words what these regular expressions match: 
#   (read carefully to see if a regular expression or a string that 
#    defines a regular expression is being used)
#        `^.*$`
#        "\\{.+\\}"
#        `\d{4}-\d{2}-\d{2}`
#        "\\\\{4}"

# - Create regular expressions to find all words that:
#     -- Start with three consonants.
#     -- Have three or more vowels in a row.
#     -- Have two or more vowel-consonant pairs in a row.

# * Grouping and backreferences -----

# Earlier, we learned about parentheses as a way to disambiguate 
# complex expressions. Parentheses also create a numbered capturing 
# group (number 1, 2 etc.). A capturing group stores the part of the 
# string matched by the part of the regular expression inside the 
# parentheses. You can refer to the same text as previously matched by a 
# capturing group with backreferences, like \1, \2 etc. For example, the 
# following regular expression finds all fruits that have a repeated pair 
# of letters.

str_view(fruit, "(..)\\1", match = TRUE)

# > Exercises -----

# - Describe, in words, what these expressions will match:
#        (.)\1\1
#        "(.)(.)\\2\\1"
#        (..)\1
#        "(.).\\1.\\1"
#        "(.)(.)(.).*\\3\\2\\1"

# - Construct regular expressions to match words that:
#     -- Start and end with the same character.
#     -- Contain a repeated pair of letters (e.g. 
#        "church" contains "ch" repeated twice.)
#     -- Contain one letter repeated in at least three places (e.g. 
#        "eleven" contains three "e"s.)

# Tools -----

# We will now learn how to apply regular expressions to real problems. 
# In this section we'll learn a wide array of `stringr` functions that 
# let us:

#   - Determine which strings match a pattern.
#   - Find the positions of matches.
#   - Extract the content of matches.
#   - Replace matches with new values.
#   - Split a string based on a match.

# Because regular expressions are so  powerful, it's tempting to try and solve every 
# problem with a single regular expression. 
# Sometimes, instead of creating one complex regular expression, it's easier to write 
# a series of simpler regexps. If you get stuck trying to create a single regexp that 
# solves your problem, take a step back and think if you could break the problem down 
# into smaller pieces, solving each challenge before moving onto the next one.

# * Detect matches -----

# To determine if a character vector matches a pattern, use `str_detect()`. 
# It returns a logical vector the same length as the input:

x <- c("apple", "banana", "pear")
str_detect(x, "e")

# Remember that when you use a logical vector in a numeric context, FALSE becomes 0 and 
# TRUE becomes 1. That makes sum() and mean() useful if you want to answer questions 
# about matches across a larger vector:

# How many common words start with t?
sum(str_detect(words, "^t"))

# What proportion of common words end with a vowel?
mean(str_detect(words, "[aeiou]$"))

# When you have complex logical conditions (e.g. match a or b but not c unless d) 
# it's often easier to combine multiple `str_detect()` calls with logical operators, 
# rather than trying to create a single regular expression. 

# For example, here are two ways to find all words that don't contain any vowels:

# Find all words containing at least one vowel, and negate
no_vowels_1 <- !str_detect(words, "[aeiou]")
# Find all words consisting only of consonants (non-vowels)
no_vowels_2 <- str_detect(words, "^[^aeiou]+$")
identical(no_vowels_1, no_vowels_2)

# The results are identical, but the first approach is significantly easier to understand.
# If your regular expression gets overly complicated, try breaking it up into smaller 
# pieces, giving each piece a name, and then combining the pieces with logical operations.

# A common use of `str_detect()` is to select the elements that match a pattern. 
# You can do this with logical subsetting, or the convenient `str_subset()` wrapper:

words[str_detect(words, "x$")]
str_subset(words, "x$")

# A variation on `str_detect()` is `str_count()`: rather than a simple yes or no, 
# it tells you how many matches there are in a string:

x <- c("apple", "banana", "pear")
str_count(x, "a")

# On average, how many vowels per word?
mean(str_count(words, "[aeiou]"))

# It's natural to use str_count() to create new variables in a data frame:

df$vowels <- str_count(df$word, "[aeiou]")
df$consonants <- str_count(df$word, "[^aeiou]")
head(df)

# Note that matches never overlap. For example, in "abababa", how many times 
# will the pattern "aba" match? Regular expressions say two, not three:

str_count("abababa", "aba")
str_view_all("abababa", "aba")

# Note the use of `str_view_all()`. Many `stringr` functions come in pairs: one 
# function works with a single match, and the other works with all matches. The 
# second function will have the suffix `_all`.

# > Exercises -----

# - For each of the following challenges, try solving it by using both a single 
#   regular expression, and a combination of multiple `str_detect()` calls:

#   -- Find all words that start or end with x.
#   -- Find all words that start with a vowel and end with a consonant.
#   -- Are there any words that contain at least one of each different vowel? (don't try one regex too hard)
#   -- What word has the highest number of vowels? What word has the highest proportion
#      of vowels? (Hint: what is the denominator?)

# * Extract matches -----

# To extract the actual text of a match, use `str_extract()`. 
# Example: the Harvard sentences, which were designed to test VOIP systems, but are 
# also useful for practicing regexps. These are provided in `stringr::sentences`:

length(sentences)
head(sentences)

# Imagine we want to find all sentences that contain a color. We first create a 
# vector of color names, and then turn it into a single regular expression:

colors <- c("red", "orange", "yellow", "green", "blue", "purple")
color_match <- str_c(colors, collapse = "|")
color_match

# Now we can select the sentences that contain a color, and then extract the color
# to figure out which one it is:

has_color <- str_subset(sentences, color_match)
matches <- str_extract(has_color, color_match)
head(matches)

# Note that `str_extract()` only extracts the first match. We can see that most
# easily by first selecting all the sentences that have more than 1 match:

more <- sentences[str_count(sentences, color_match) > 1]
str_view_all(more, color_match)
str_extract(more, color_match)

# This is a common pattern for stringr functions, because working with a single match
# allows you to use much simpler data structures. To get all matches, 
# use `str_extract_all()`. It returns a list:

str_extract_all(more, color_match)

# If you use simplify = TRUE, `str_extract_all()` will return a matrix with short 
# matches expanded to the same length as the longest:
str_extract_all(more, color_match, simplify = FALSE)
str_extract_all(more, color_match, simplify = TRUE)

x <- c("a", "a b", "a b c")
str_extract_all(x, "[a-z]", simplify = FALSE)
str_extract_all(x, "[a-z]", simplify = TRUE)


# > Exercises -----

# - In the previous example, you might have noticed that the regular expression 
#   matched "flickered", which is not a color. Modify the regex to fix the problem.

# - From the Harvard sentences data, extract:
#     -- The first word from each sentence. (word = any contiguous set of letters)
#     -- All words ending in ing.
#     -- All plurals.

# * Grouped matches -----

# Earlier in this chapter we talked about the use of parentheses for clarifying 
# precedence and for backreferences when matching. You can also use parentheses 
# to extract parts of a complex match. For example, imagine we want to extract nouns 
# from the sentences. As a heuristic, we'll look for any word that comes after "a" or 
# "the". Defining a "word" in a regular expression is a little tricky, so we'll use a 
# simple approximation: a sequence of at least one character that isn't a space.

noun <- "(a|the) ([^ ]+)"

has_noun <- str_subset(sentences, noun)
head(has_noun, 10)

head(str_extract(has_noun, noun), 10)

# `str_extract()` gives us the complete match; 
# `str_match()` gives each individual component. 
# Instead of a character vector, it returns a matrix, with one column for the 
# complete match followed by one column for each group:

head(str_match(has_noun, noun), 10)

# (Unsurprisingly, our heuristic for detecting nouns is poor, and also picks up 
# adjectives like smooth and parked.)

# Like `str_extract()`, if you want all matches for each string, you'll need 
# `str_match_all()`.

# > Exercises -----

# - Find all words that come after a "number" like "one", "two", "three" etc. 
#   Pull out both the number and the word.

# - Find all contractions. Separate out the pieces before and after 
#   the apostrophe.

# * Replacing matches -----

# `str_replace()` and `str_replace_all()` allow you to replace matches with 
# new strings. The simplest use is to replace a pattern with a fixed string:

x <- c("apple", "pear", "banana")
str_replace(x, "[aeiou]", "-")
str_replace_all(x, "[aeiou]", "-")

# With `str_replace_all()` you can perform multiple replacements by supplying 
# a named vector:
x <- c("1 house", "2 cars", "3 people")
str_replace_all(x, c("1" = "one", "2" = "two", "3" = "three"))


# Instead of replacing with a fixed string you can use backreferences to insert 
# components of the match. In the following code, we flip the order of the 
# second and third words.
head(sentences, 5)
head(str_replace(sentences, "([^ ]+) ([^ ]+) ([^ ]+)", "\\1 \\3 \\2"), 5)

# > Exercises -----

# - Replace all forward slashes in a string with backslashes.
# - Implement a simple version of `str_to_lower()` using `replace_all()`.
# - Switch the first and last letters in words. Which of those strings are 
#   still words? Of these, which don't start and end with the same letter?

# * Splitting -----

# Use `str_split()` to split a string up into pieces. For example, we could 
# split sentences into words:

head(str_split(sentences, " "), 5)

# Because each component might contain a different number of pieces, this 
# returns a list. If you're working with a length-1 vector, the easiest thing 
# is to just extract the first element of the list:

str_split("a|b|c|d", "\\|")
str_split("a|b|c|d", "\\|")[[1]]
  
# Otherwise, like the other `stringr` functions that return a list, you can 
# use `simplify = TRUE` to return a matrix:

head(str_split(sentences, " ", simplify = TRUE), 5)

# Instead of splitting up strings by patterns, you can also split up by 
# character, line, sentence and word `boundary()`s:

x <- "This is a sentence.  This is another sentence."

str_split(x, "\\s")[[1]]

str_view_all(x, boundary("word"))
str_split(x, boundary("word"))[[1]]

# > Exercises -----

# - Split up a string like "apples, pears, and bananas" into individual components.
# - Why is it better to split up by boundary("word") than " "?
# - What does splitting with an empty string ("") do? Experiment, and then read 
#   the documentation.

# Counting Words with table() -----

# Example -----
# "al2.txt" file includes the following:

#     Text of Some Importance  
#     If we shall suppose that American slavery is one of those
#     offenses which, in the providence of God, must needs come, 
#     but which, having continued through His appointed time, He 
#     now wills to remove, and that He gives to both North and 
#     South this terrible war as the woe due to those by whom 
#     the offense came, shall we discern therein any departure 
#     from those divine attributes which the believers in a 
#     living God always ascribe to Him? Fondly do we hope, 
#     fervently do we pray, that this mighty scourge of war may
#     speedily pass away. Yet, if God wills that it continue 
#     until all the wealth piled by the bondsman's two hundred 
#     and fifty years of unrequited toil shall be sunk, and 
#     until every drop of blood drawn with the lash shall be 
#     paid by another drawn with the sword, as was said three 
#     thousand years ago, so still it must be said "the 
#     judgments of the Lord are true and righteous altogether."

al2 <- readLines("al2.txt")
length(al2) # number of lines
head(al2)

# `al2` is a vector, one element per line of text.

# Tabulate how often each word appears, put in order:
al2_words <- str_split(al2, boundary("word"))
wc <- table(unlist(al2_words))
wc <- sort(wc, decreasing = TRUE)
head(wc, 20)

# * The %>% (piping) operator -----

# The piping operator takes the contents of whatever it is to its left and 
# sends it as the **first** input to the function to its right. 

# So, the following code:
al2_words <- str_split(al2, boundary("word"))
wc <- table(unlist(al2_words))
wc <- sort(wc, decreasing = TRUE)
head(wc, 10)

# Is equivalent to:
str_split(al2, boundary("word")) %>%
  unlist() %>%
  table() %>%
  sort(decreasing = TRUE) %>%
  head(10)


# `names(wc)` gives all the distinct words in `al2.words` (***types***);
# `wc` counts how often they appear (***tokens***):
names(wc)

wc["years"]
wc["years,"]

wc["that"]
wc["That"]


# * Find matches -----

# `str_locate()` and `str_locate_all()` give you the starting and ending 
# positions of each match. These are particularly useful when none of the other
# functions does exactly what you want. You can use `str_locate()` to find the 
# matching pattern, `str_sub()` to extract and/or modify them.

# * Other types of pattern -----

# When you use a pattern that's a string, it's automatically wrapped into a call 
# to `regex()`:

# The regular call:
str_view(fruit, "nana")
# Is shorthand for
str_view(fruit, regex("nana"))

# You can use the other arguments of `regex()` to control details of the match:

# `ignore_case = TRUE` allows characters to match either their uppercase or lowercase forms. This always 
# uses the current locale:
bananas <- c("banana", "Banana", "BANANA")
str_view(bananas, "banana")
str_view(bananas, regex("banana", ignore_case = TRUE))

# `multiline = TRUE` allows ^ and $ to match the start and end of each line 
# rather than the start and end of the complete string:
x <- "Line 1\nLine 2\nLine 3"
str_extract_all(x, "^Line")[[1]]
str_extract_all(x, regex("^Line", multiline = TRUE))[[1]]

# `comments = TRUE` allows you to use comments and white space to make complex 
# regular expressions more understandable. Spaces are ignored, as is everything 
# after `#`. To match a literal space, you'll need to escape it: "\\ ".

phone <- regex("
  \\(?     # optional opening parens
  (\\d{3}) # area code
  [) -]?   # optional closing parens, space, or dash
  (\\d{3}) # another three numbers
  [ -]?    # optional space or dash
  (\\d{3}) # three more numbers
  ", comments = TRUE)

str_match("514-791-8141", phone)

# `dotall = TRUE` allows . to match everything, including \n.

# There are three other functions you can use instead of `regex()`:

# - `fixed()`: matches exactly the specified sequence of bytes. It ignores 
#   all special regular expressions and operates at a very low level. This 
#   allows you to avoid complex escaping and can be much faster than regular 
#   expressions. The following microbenchmark shows that it's about 3x faster 
#   for a simple example.
microbenchmark::microbenchmark(
  fixed = str_detect(sentences, fixed("the")),
  regex = str_detect(sentences, "the"),
  times = 20
)

# Beware using `fixed()` with non-English data. It is problematic because there 
# are often multiple ways of representing the same character. For example, there 
# are two ways to define "á": either as a single character or as an "a" plus an 
# accent:

a1 <- "\u00e1"
a2 <- "a\u0301"
c(a1, a2)
a1 == a2

# They render identically, but because they're defined differently, `fixed()` 
# doesn't find a match. Instead, you can use `coll()`, defined next, to respect
# human character comparison rules:

str_detect(a1, fixed(a2))
str_detect(a1, coll(a2))


# coll(): compare strings using standard collation rules. This is useful for
# doing case insensitive matching. Note that `coll()` takes a locale parameter
# that controls which rules are used for comparing characters. Unfortunately
# different parts of the world use different rules!

# That means you also need to be aware of the difference
# when doing case insensitive matches:
i <- c("I", "İ", "i", "ı")
i

str_subset(i, coll("i", ignore_case = TRUE))
str_subset(i, coll("i", ignore_case = TRUE, locale = "tr"))

# Both `fixed()` and `regex()` have `ignore_case` arguments, but they do not 
# allow you to pick the locale: they always use the default locale. You can 
# see what that is with the following code; more on stringi later.

stringi::stri_locale_info()

# The downside of `coll()` is speed; because the rules for recognizing which
# characters are the same are complicated, `coll()` is relatively slow compared
# to `regex()` and `fixed()`.

# As you saw with `str_split()` you can use `boundary()` to match boundaries. 
# You can also use it with the other functions:

x <- "This is a sentence."
str_view_all(x, boundary("word"))
str_extract_all(x, boundary("word"))

# > Exercises -----

# - How would you find all strings containing \ with `regex()` vs. with `fixed()`?
# - What are the five most common words in sentences?
