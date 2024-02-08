#' Unit 3 Data types
#' 
#' This is a raw script file for the same unit from the course textbook
#' You can follow the output of the code chunks in the text in this file
#' Feel free to experiment with the code as well
#' Use the "#" sign to add your own comments to the code!
#' 

1.2

3.0

1.5 + 2.2

typeof(3.0)

3  # here is a value that looks like an integer
typeof(3) # ..but is actually a double!

# Some integer values
2L

1L + 3L

-123456789L

typeof(-123456789L)

3.2L

3.5 + 1.2
3L + 1.3
4L + 1
4L + 1L

typeof(3.5 + 1.2)
typeof(3L + 1.3)
typeof(4L + 1)
typeof(4L + 1L)

-1.0e1000
1.0e1000

1.0e500
1.0e400
1.0e300
1.0e310
1.0e305
1.0e306
1.0e307
1.5e308
1.6e308
1.7e308
1.8e308
1.790e308

library(tidyverse)

"This is my first string."
"We love R!"
'"Data Science" is a lot of fun :-)'

typeof("We love R!")

as.integer("4786")

as.double("3.14")
as.double("456")
as.character(3.14159265)
as.character(-465)

str_length("1-5-1 Mejiro, Toshima-ku, Tokyo 171-8588 JAPAN")

my_string <- "song"

str_sub("song", 2, 3)

str_sub("song", 2)

str_sub("song", -3, -1)

str_sub("song", 2, 0)

str1 <- "data"
str2 <- "science"
str3 <- "rocks"

str_c(str1, str2, str3)
str_c(str1, str2, str3, sep = " ")

TRUE
FALSE

1 + 4 > 7 - 4

1 < (1 + 1) & (1 + 1) < 3

x <- 12
y <- 5
min(x, y) <= (x + y)/2
max(x, y) >= (x + y)/2

x <- 17
y <- 17
min(x, y) == (x + y)/2
max(x, y) == (x + y)/2

a <- TRUE
b <- FALSE
c <- TRUE
!a
a | b | c
a & b & c

print("Bach" > "Back")
print("Darth Vader" > "Dark Chocolate")
print("09:00 AM" > "Nine in the morning")
print("data science" > "Data Science")
print("abc" > "ABC" )

a <- "Jason"
b <- "Bourne"
c <- "Matt"
d <- "Damon"
a < b
a > b & c > d
a > b | d < c
!(a < b)

3.0 == TRUE
-5 == FALSE
0 == TRUE
0 == FALSE

my_vec <- 10

my_vec

my_vec[1]

my_vec[1][1]
my_vec[1][1][1]
my_vec[1][1][1][1]
my_vec[1][1][1][1][1]

my_vec[2]
my_vec[0]

my_vec2 <- numeric(5)

my_vec2[2] <- 2
my_vec2[4] <- 4
my_vec2


my_vec2[1] <- 1
my_vec2[3] <- 3
my_vec2[5] <- 5
my_vec2[1:5]
my_vec2[2:4]
my_vec2[2:2]

my_vec2[4:1]

my_vec2
my_vec2[-3]
my_vec2[-2:-4]
my_vec2[-4:-2]

c(6, 2, 3)
c("data", "science", "rocks", "my", "socks")
c(3.0, 4.0, 2.0, 2.2, -4.5, -25.7)

my_vec2[c(1,3,4)]
my_vec2[c(4,4,3,3,5,3,5,3)]

a <- c(2, 3, 4, 5, 1, 6)
b <- c(9, 8, 7, 1, 2, 1)

a + 1/2


a - 7
a * 3
b / 2
b %% 3

a + b
a - b
a * b
a / b
a %% b

length(a)
max(a)
min(a)

sum(a)

c(a, b)

c(a, 10)
c(78, a)

c(79, a, 17)

a > b
a >= 3

a[a > b]
a[a >= 3]

greetings <- c("hello", "goodbye", "hello", "hello", "goodbye")

greetings == "hello"

sum(greetings == "hello")

temps <- c(87.5, 87.5, 66.5, 90.0, 65.5, 71.0)
length(temps)

sum(temps)

mean(temps)

diff(temps)

mixed <- list("apple", 1.5, 2L, TRUE)
mixed

str(mixed)

mixed2 <- list(c("asparagus", "arrowroot", "tomato"), 
               c("mango", "kumquat"), 
               3.14159)
str(mixed2)

omg <- list(list(1,1), list(2,2,2), "hello world")
str(omg)

mixed2

str(mixed2[2])

str(mixed2[1:2])

mixed2[[2]]

mixed2[[2]][1]

a <- list("A big cat", c(1,2,3), 3.14159)


library(tidyverse)

s <- c("May 17, 2019", "Certified mail", "FL 33333", 
       "Oppa Locka", "to Mr. Haan", "arrived")
s

s
str_detect(s, "a")
str_detect(s, "a[iy]")
str_detect(s, "a$")
str_detect(s, "^a")
str_detect(s, "^a.*d$")
str_detect(s, "ppa")
str_detect(s, "3{4}")
str_detect(s, "[aeiou].[aeiou]")

str_view_all(s, "[aeiou].[aeiou]", html = TRUE)

s
str_detect(s, "[a-z]")
str_detect(s, "[A-Z]{2,5}")
str_detect(s, "[^a-zA-Z]")
str_detect(s, "[0-9]")

s

str_which(s, "[0-9]")

str_count(s, "[aeiou][A-Za-z]")

str_count(s, "[0-9]{3}")

str_locate(s, "[A-Za-z][aeiou]+")

x <- str_locate(s, "[A-Za-z][aeiou]+")
x
x[,1]
x[,2]
x[1,]
x[2,]

str_locate_all(s, "[A-Za-z][aeiou]+")

str_sub(s, 4, 7)
str_subset(s, "[a-zA-Z][^a-zA-Z]+[a-zA-Z]")
str_extract(s, "[a-zA-Z][^a-zA-Z]+[a-zA-Z]")
str_match(s, "[a-zA-Z][^a-zA-Z]+[a-zA-Z]")

str_length(s)
str_pad(s, 11, side="both", pad=".")
str_pad(s, 10, side="left", ".")
str_trunc(s, 3, side="right", ellipsis="_")
str_trunc(s, 5, side="center", "#")
str_trim("   abc    ", side="right")
str_trim("   abc    ", side="both")

s <- c("5/17/2019", "Certified mail", "FL 33333", 
       "Oppa Locka", "to Mr. Haan", "arrived")

scopy <- s
str_sub(scopy, 1, 3) <- "I am "
scopy
scopy <- s
str_sub(scopy, 0, 0) <- "I am "
scopy
s

t <- s
str_replace(t, "a", "oo")
t <- s
str_replace_all(s, "a", "oo")
t <- s
str_replace_all(s,"[0-9][0-9]", "##")

t <- s
str_to_lower(s)
t <- s
str_to_upper(s)
t <- s
str_to_title(s)

t <- s
str_dup(t, 3)

str_c(t)
str_c(t, collapse=":")

u <- str_c(t)
str_sort(u)
u <- str_c(t)
str_sort(u, decreasing=TRUE)
u <- str_c(t)
str_order(u, decreasing=TRUE)

birds <- c("Black-crowned Night-Heron-Nycticorax", 
           "Little-egret-Egretta garzetta")

str_view_all(birds, "-", html=TRUE)

dash_positions <- str_locate_all(birds, "-")
dash_positions

dash_positions[[1]]

tail(dash_positions[[1]], n=1) # why n = 1? why not 2?

last_dash <- tail(dash_positions[[1]], n=1)[1]
last_dash

str_sub(birds[1], end = last_dash - 1)

dash_positions <- str_locate_all(birds, "-")
last_dash <- tail(dash_positions[[1]], n=1)[1]
str_sub(birds[1], end = last_dash - 1)

## library(tidyverse)
## library(edsdata)
## library(gapminder)

a_happy_string <- "-4.5"
double_trouble <- 81.9

str1 <- "State"
str2 <- "Department"
str3 <- "Office"

## (2 - 1) == ((TRUE == TRUE) != FALSE)
## (10 - (FALSE/2 + max(TRUE, FALSE))) >= (TRUE + 1)
## (Inf > 5) == ((Inf > Inf) | (Inf >= Inf))

s1 <- "Fine"
s2 <- "Dine"
s3 <- "Sine"
s4 <- "Wine"

snew <- str_c(s1, s2, s3, s4, sep = " ")
snew

## pop_2025_to_2040[17]

bird_in_a_hand <- c("b","i","r","d",
                    "","i","n","","a","",
                    "h","a","n","d")
bird_in_a_hand

## str_remove_all(bird_in_a_hand, "[ab]")

fruits <- c(
  "apples and oranges and pears and bananas",
  "pineapples and mangos and guavas"
)

## str_split(fruits, "[and]")
## str_split(fruits_list, "[and]")
## str_split(fruits_unlisted, "and")

## str_subset(c("x", "y", "xx", "yy"), "x")

## badly_formed_ids <- c("CSC_100", "BIO 111", "MTH161H",
##                       "ECO--220", "MUS..160A")
## badly_formed_ids

## c("CSC100", "BIO111", "MTH161", "ECO220", "MUS160")
