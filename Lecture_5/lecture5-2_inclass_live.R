# Agenda =====
# - Introduction to databases.
# - SQL: Structured Query Language. 
# - R-SQL interface.

# What is a database? =====
# A database is an organized collection of structured information, or data, typically 
# stored electronically on a computer system. It helps define things "from the ground up":

# - A **field** is a variable/quantity of interest.
# - A **record** is a collection of **fields**.
# - A **table** is a collection of records which all have the same fields (with different values).
# - A **database** is a collection of tables.

# Why do we need database software? =====

# - **Size**
#     - R keeps its data frames in memory.
#     - Industrial databases can be much bigger.
#     - Must work with selected subsets.
    
# - **Speed**
#     - Smart people have worked very hard on getting just what you want, quickly.

# - **Concurrency**
#     - Many users accessing the same database simultaneously.
#     - Lots of potential for trouble (two users want to change the same record at once).
    
# The client-server model, SQL =====

# - Databases live on a **server**, which manages them.
# - Users interact with the server through a **client** program.
# - Allows multiple users access the same database simultaneously.
# - **SQL** (**structured query language**) is a standard database software.
# - Mostly about **queries**, which are like doing row/column selections on 
#   a data frame in R.


# Consult https://www.sqltutorial.org/ for an introduction to SQL.

# `SELECT` =====

# Main tool in the SQL language: `SELECT`, which allows you to perform queries on a
# particular table in a database. It has the form:

# ```
# SELECT columns or computations
#   FROM table
#   WHERE condition
#   GROUP BY columns
#   HAVING condition
#   ORDER BY column [ASC | DESC]
#   LIMIT offset,count;
# ```
# Here, `WHERE`, `GROUP BY`, `HAVING`, `ORDER BY`, `LIMIT` are all optional. 



# Connecting R to SQL =====
  
# SQL is its own language, independent of R (similar to when you learned regexps).
# In order to try SQL, we need to connect a database to R. 
# 
# First, we need to install the packages `DBI`, `RSQLite`, then we load them into
# our R session with `library()`. `DBI` defines an interface for communication between
# R and relational database management systems. `RSQLite` embeds the SQLite database
# engine in R and provides an interface compliant with the `DBI` package.
# 
# For the following examples, download the baseball.db, and save it in your R 
# working directory.

options(max.print = 80)

library(DBI)
library(RSQLite)
drv = dbDriver("SQLite")
con = dbConnect(drv, dbname = "baseball.db")

# The object `con` is now a persistent connection to the database `baseball.db`.

# List tables in our database:
dbListTables(con)

# List fields in different tables:
dbListFields(con, "Master") 
dbListFields(con, "SchoolsPlayers") 
dbListFields(con, "Batting") 
dbListFields(con, "Pitching")
dbListFields(con, "Salaries")
# Terms explained: http://m.mlb.com/glossary/standard-stats


# Example =====
# Four columns from table `Batting`, SQL query:
# SELECT playerID, yearID, AB, H FROM Batting;

# This SQL query executed in R:
dbGetQuery(con, "SELECT playerID, yearID, AB, H, HR FROM Batting")
dbGetQuery(con, "select playerID, yearID, AB, H, HR fRoM Batting")
dbGetQuery(con, "select playerid, yEariD, Ab, h, HR fRoM Batting")


# The output can be assigned to a local variable:
db0 <- dbGetQuery(con, "SELECT playerID, yearID, AB, H, HR FROM Batting")

# Which is a ...:
str(db0)

# For legibility, it is often convenient to break the queries into different lines 
# and `paste()` them together:
dbGetQuery(
  con, 
  paste(
    "SELECT playerID, yearID, AB, H, HR",
    "FROM Batting"
    )
)





# Or as a single string broken across a few lines:
db0 <- 
  dbGetQuery(con, "SELECT 
             playerID, yearID, AB, 
             H, HR FROM Batting")

# All columns from table `Salaries`:
# ```
# SELECT * FROM Salaries;
# ```
dbGetQuery(con, "SELECT * FROM Salaries")

# As above, but by ascending value of `Salary`:
# ```
# SELECT * FROM Salaries ORDER BY Salary;
# ```
dbGetQuery(con, "SELECT * FROM Salaries ORDER BY Salary")

# Descending order:
# ```
# SELECT * FROM Salaries ORDER BY Salary DESC;
# ```
dbGetQuery(con, "SELECT * FROM Salaries ORDER BY Salary DESC")

# Top 5 salaries:
# ```
# SELECT * FROM Salaries ORDER BY Salary DESC LIMIT 5;
# ```
dbGetQuery(con, "SELECT * FROM Salaries ORDER BY Salary DESC LIMIT 5")

# Picking out rows meeting a condition:
# ```
# SELECT PlayerID, yearID, AB, H FROM Batting WHERE AB > 100 AND H > 0;
# ```
dbGetQuery(con, "SELECT PlayerID, yearID, AB, H FROM Batting WHERE AB > 100 AND H > 0")


# IN

# Find all players with last name Alou or Griffey:
     # SELECT nameGiven as Given, nameLast Last, birthYear AS born
     # FROM master
     # WHERE nameLast IN ("Alou", "Griffey")

dbGetQuery(con,
           paste(
             "SELECT nameGiven as Given, nameLast Last, birthYear AS born",
             "FROM master",
             "WHERE nameLast IN (\"Alou\", \"Griffey\")"
           )
)

# Use a LIKE statement with a WHERE clause to get partial string matching. 

# % 	Represents zero or more characters; bl% finds bl, black, blue, and blob
# _ 	Represents a single character; h_t finds hot, hat, and hit
# [] 	Represents any single character within the brackets; h[oa]t finds hot and hat, but not hit
# ^ 	Represents any character not in the brackets; h[^oa]t finds hit, but not hot and hat
# - 	Represents a range of characters; c[a-b]t finds cat and cbt

# Find all players with last name containing 'riff' substring:
# SELECT nameGiven as Given, nameLast Last, birthYear AS born
# FROM master
# WHERE nameLast LIKE "" 

dbGetQuery(con,
           paste(
           "SELECT nameGiven as Given, nameLast Last, birthYear AS born",
           "FROM master",
           "WHERE nameLast LIKE \"%RIFF%\""
           )
           )

# Find all players born in Puerto Rico ("P.R.") during the 1980's:
dbGetQuery(con,
           "SELECT nameGiven as Given, nameLast Last, birthYear AS born, birthCountry as Place
           FROM master
           WHERE birthCountry ==  \"P.R.\"     AND birthYear  LIKE  \"198%\"       ")



# Calculated Columns =====
# - SQL has some simple summary statistics:
# ```
# SELECT MIN(AB), AVG(AB), MAX(AB) FROM Batting;
# ```
(sum_db <- dbGetQuery(con, "SELECT MIN(AB), AVG(AB), MAX(AB) FROM Batting"))
""


# Or COUNT(variable):
dbGetQuery(con, "SELECT COUNT(PlayerID) FROM Batting")
dbGetQuery(con, "SELECT COUNT(*) FROM Batting")


# - It can do arithmetic:
# ```
# SELECT AB, H, H/CAST (AB AS REAL) FROM Batting;
# ```
dbGetQuery(con, "SELECT AB, H, H / CAST(AB AS REAL) FROM Batting")
dbGetQuery(con, "SELECT AB, H, H / AB FROM Batting")

# But:
dbGetQuery(con, "SELECT AB, H, H / CAST(AB AS REAL), MIN(AB) FROM Batting")
# That is, ....

# We will not cover here the rules of how to deal with infinity values and the likes in SQL, 
# but they are not the same as in R.

# - Calculated columns can be assigned with names:
# ```
# SELECT PlayerID, yearID, H / CAST (AB AS REAL) AS BattingAvg FROM Batting
# ORDER BY BattingAvg DESC LIMIT 5;
# ```
dbGetQuery(con,
           "SELECT PlayerID, yearID, H / CAST(AB AS REAL) AS BattingAvg FROM Batting
            ORDER BY BattingAvg DESC
            LIMIT 5")



# Aggregating =====
# We can do calculations on value-grouped subsets:
# ```
# SELECT playerID, SUM(salary) FROM Salaries GROUP BY playerID
# ```
dbGetQuery(con, "SELECT playerID, SUM(salary) FROM Salaries GROUP BY playerID")

# Selecting Again With HAVING =====
# - First cut of records is with `WHERE`.
# - Aggregation of record with `GROUP BY`.
# - Post-aggregation selection with `HAVING`.

# `HAVING` defines a limiting clause on an **aggregate** variable.
# It is similar to `WHERE` except for operating on summary statistics rather
# than individual rows.

# Note that the HAVING clause comes after the GROUP BY but before the ORDER BY.

# Players with 500+ RBIs since 2005:

# ```
# SELECT playerID, sum(RBI) as RBI_total
# FROM batting 
# WHERE  
# GROUP BY playerID
# HAVING  
# ORDER BY  
# ```

dbGetQuery(con,
           "SELECT playerID,      
           FROM batting
           WHERE          
           GROUP BY          
           HAVING               ")


# `ORDER BY` =====
# We can use the `ORDER BY` option in `SELECT` to specify an ordering for the
# rows. Default is ascending order; add `DESC` for descending:
  
dbGetQuery(con,
           "SELECT playerID, 
           FROM batting
           WHERE 
           GROUP BY 
           HAVING 
           ORDER BY        DESC")



# JOINS =====

# So far we have discussed working with single tables only. The SQL term for
# merging data from two or more tables is a `JOIN`. All joins are based on the idea of
# equating rows that match on one or more variables.
  
  # - Inner Joins - produce tables containing rows for which matches are found in both tables,
  # - Left Joins - produce tables containing rows for (at least) each row on the left table with
  #                additional columns from the right table,
  # - Right Joins - produce tables like left joins but reversing the role of the right and left tables.

# Inner Join =====

# When we want to supplement our tables with additional information about rows from **other** tables
# we could use an inner join:

dbGetQuery(con,
           "SELECT m.nameFirst First, m.nameLast Last, sum(RBI) as RBI_TOTAL
           FROM batting b
           INNER JOIN master m ON b.playerID == m.playerID
           WHERE yearID >= 2005
           GROUP BY b.playerID
           HAVING RBI_total >= 500
           ORDER BY -RBI_total")


# Outer Joins =====

# A "left join" – or a "left outer join" – adds columns from the right table to the left table 
# when matching rows are found.

# Rows from the left table with no matches from the right table are
# retained with columns from the right table filled in as NULL.

# When there are multiple matches of a row from the left table to rows in the right table,
# these each become a row in the new table.

# A right join is equivalent to a left join only that the roles between right and left are reversed.

# Left joins are particularly useful when the information in the right table is only applicable to a
# subset of the rows from the left table.

# Which US colleges and universities have produced the most “Rookie of the Year Awards” given to the
# best debuting player(s) each season?

# First, find the last college attended:
# ```
# SELECT * # FROM CollegePlaying
# ```
dbGetQuery(con, "SELECT * FROM SchoolsPlayers")


# Next, find all distinct awards in the AwardPlayers table:
dbListFields(con, "AwardsPlayers")

dbGetQuery(con, "SELECT DISTINCT(      ) FROM AwardsPlayers")

# Next we test a query for finding all Rookie of the Year Awards:
dbGetQuery(con,
           "SELECT *
           FROM AwardsPlayers
           WHERE                  ")


# Use a left join of the tables for ROY awards and last college attended
# to match winners to their schools. We need a left join as many of the
# winners may never have played collegiate baseball:

dbGetQuery(con,
           "SELECT roy.playerID playerID, roy.yearID year, lgID league, schoolID
           FROM AwardsPlayers roy
           LEFT JOIN
           (SELECT * FROM SchoolsPlayers) c
           ON c.playerID == roy.playerID
           WHERE awardID LIKE \"Rookie%\"")


# To complete the example, modify the query to display which schools have produced the
# most ROY awards in total:

""
""
""
""



# Nested queries
# ===

# We can also run queries on queries:
dbGetQuery(
  con,
  "SELECT COUNT(*) FROM
  (
  SELECT playerID
   FROM Master
   GROUP BY playerID
  )"
  )

# Which, in this case, is just a clumsy version of:
dbGetQuery(con, "SELECT COUNT(DISTINCT playerID) FROM Master")

# Or is it?
microbenchmark::microbenchmark(
  {
    dbGetQuery(
      con,
      "SELECT COUNT(playerID) FROM
      (
      SELECT playerID
       FROM Master
       GROUP BY playerID
      )")
  },
  {
    dbGetQuery(con, "SELECT COUNT(DISTINCT playerID) FROM Master")
  })



# Importing a table as a data frame: =====
batting <- dbReadTable(con, "Batting") 
class(batting) 
dim(batting)


# Now we could go on and perform R operations on `batting`, since 
# it's a data frame. To pick out five columns from the table "Batting"

dbGetQuery(con, "SELECT playerID, yearID, AB, H FROM Batting WHERE AB > 100 AND H > 0")

library(dplyr)
batting %>%
  select(playerID, yearID, AB, H) %>%
  filter(AB > 100, H >0)

library(Lahman)
batting[batting$AB > 100 & batting$H > 0, c("playerID", "yearID", "AB", "H")]

# Do you see a difference between the methods?
# The latter keeps the original row numbers as the names for the rows.
# and so:
example <- batting[batting$AB > 100 & batting$H > 0, c("playerID", "yearID", "AB", "H")]
""
""
""

# Compare to:
example2 <- 
  batting %>%
  select(playerID, yearID, AB, H) %>%
  filter(AB > 100, H > 0)
""
""
""


# To reiterate: this was simply to check our work, and we wouldn't actually
# want to do this on a large database, since it'd be much more inefficient
# to first read into an R data frame, and then call R commands.

# dbplyr =====

# However, we can use the `dbplyr` package which generates SQL queries based on the `dplyr` syntax:
library(dplyr)
library(dbplyr)
batting_link <- tbl(con, "batting")
str(batting_link)

# All dplyr calls are evaluated lazily, generating SQL that is only sent to the
# database **when you request the data**.

(example3 <- 
  batting_link %>%
  select(playerID, yearID, AB, H) %>%
  filter(AB > 100, H > 0))

str(example3)

# See query
example3 %>% show_query()

# execute query and retrieve results
(example4 <- example3 %>% collect())

str(example4)


# Perhaps not unexpectedly, using `dbplyr` incurs costs for overheads:
microbenchmark::microbenchmark(
  {
    example <- dbGetQuery(con, "SELECT playerID, yearID, AB, H FROM Batting WHERE AB > 100 AND H > 0")
  },
  {
    example3 <- 
      batting_link %>%
      select(playerID, yearID, AB, H) %>%
      filter(AB > 100, H > 0)
    example4 <- example3 %>% collect()
  }
)


# `dbDisconnect ( )` =====
# Once you are done operating with your SQLite database within R, it is important to call the
# function `dbDisconnect()`. This ensures that we release any resources that the database
# connection has been using, which is always a good practice.
dbDisconnect(con)
dbUnloadDriver(drv)

# And now the following:
dbGetQuery(con, "SELECT playerID, yearID, AB, H FROM Batting WHERE AB > 100 AND H > 0")
# will produce an error.