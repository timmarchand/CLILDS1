## -----------------------------------------------------------
415
-3.56
1956.5436781


## -----------------------------------------------------------
5.07e-2


## -----------------------------------------------------------
1.5454786e-10


## -----------------------------------------------------------
3.4 * (1.7 + 2.3)


## -----------------------------------------------------------
2^5


## -----------------------------------------------------------
2 ** 5


## -----------------------------------------------------------
10.0 %% 2.3


## -----------------------------------------------------------
2 + 3 * 4 * 5 / 6 ** 2


## -----------------------------------------------------------
2 + (3 * 4 * 5 / 6) ** 2


## -----------------------------------------------------------
3.4 + 4.2


## -----------------------------------------------------------
a <- 3.4 + 4.2


## -----------------------------------------------------------
a


## -----------------------------------------------------------
a <- 5 * 15 # an assignment 
a # type its name to reveal its value


## ---- error=TRUE--------------------------------------------
barnie


## -----------------------------------------------------------
a_tasty_pie <- 314159
a_tasty_pie


## -----------------------------------------------------------
a_tasty_pie / 2


## ----error=TRUE---------------------------------------------
a <- barnie * 3


## -----------------------------------------------------------
a <- 5 


## -----------------------------------------------------------
b <- 3 


## -----------------------------------------------------------
summed <- a + b
summed


## -----------------------------------------------------------
a <- 10
summed


## -----------------------------------------------------------
summed <- a + b
summed


## ----eval=FALSE---------------------------------------------
## a
## b
## summed


## -----------------------------------------------------------
a <- 10


## -----------------------------------------------------------
a <- a + 5
a


## -----------------------------------------------------------
abs(-20)


## -----------------------------------------------------------
a <- -10.5
floor(a)
ceiling(a)
abs(a)


## -----------------------------------------------------------
sin(1)
cos(1)
tan(1)
asin(-1)
acos(-1)
atan(1)
log(10)
log10(10)


## -----------------------------------------------------------
max(4, 5, 3)
min(1, 2, 3, 4, 5, 6)


## -----------------------------------------------------------
1000 * 1.01 * 1.01 * 1.01 * 1.01 * 
  1.01 * 1.01 * 1.01 * 1.01 * 1.01 * 1.01 * 1.01 * 1.01


## -----------------------------------------------------------
200000 * 1.01 * 1.01 * 1.01 * 1.01 * 
  1.01 * 1.01 * 1.01 * 1.01 * 1.01 * 1.01 * 1.01 * 1.01


## -----------------------------------------------------------
200000 * 1.01 ^ 12
200000 * 1.01 ** 12


## -----------------------------------------------------------
principal <- 1000
rate <- 1.01
months <- 12
monthly <- principal * rate ^ months * (rate - 1) / (rate ^ months - 1)
total <- monthly * months


## -----------------------------------------------------------
monthly
total


## -----------------------------------------------------------
balloon <- principal * rate ^ months
balloon


## -----------------------------------------------------------
diff <- balloon - total
diff_percent <- diff / principal * 100
diff_percent


## ---- eval=FALSE--------------------------------------------
## install.packages("tidyverse")
## library(tidyverse)


## ----eval=FALSE---------------------------------------------
## library(tidyverse)
## library(edsdata)


## ----message=FALSE, warning=FALSE---------------------------
a <- 210
b <- 5


## ----eval=FALSE---------------------------------------------
## 4 <- 2 + 1


## ----eval=FALSE---------------------------------------------
## fifty <- 50
## ten <- 9
## eight <- fifty divides ten


## ----message=FALSE, warning=FALSE---------------------------
a_to_b_ang_dist <- 4.5 


## ----message=FALSE, warning=FALSE---------------------------
NS <- 1234 # the line NS has length 1,234 meters
ONC <- 40 # the angle ONC is 40 degrees
OSC <- 65 # the angle OSC is 65 degrees

