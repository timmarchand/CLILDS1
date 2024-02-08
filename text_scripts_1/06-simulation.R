## -----------------------------------------------------------
fair_coin <- c("heads", "tails")
sample(fair_coin, size = 1)


## -----------------------------------------------------------
two_groups <- c("kick", "receive")
sample(two_groups, size = 1)


## -----------------------------------------------------------
sample(two_groups, size = 8, replace = TRUE)


## ----eval=FALSE---------------------------------------------
## sample(two_groups, size = 8)


## -----------------------------------------------------------
sign <- function(x) {
  if (x > 0) {
    return("positive")
  }
}
sign(3)


## -----------------------------------------------------------
print(sign(-3))


## -----------------------------------------------------------
sign <- function(x) {
  if (x >= 0) {
    return("positive")
  }
}
sign(0)
sign(0.1)
print(sign(-0.1))  # force a print


## -----------------------------------------------------------
sign <- function(x) {
  if (x > 0) {
    return("positive")
  }
  if (x < 0) {
    return("negative")
  }
}
sign(2)
sign(-2)
print(sign(0))  # force a print


## -----------------------------------------------------------
sign <- function(x) {
  if (x > 0) {
    return("positive")
  } else if (x < 0) {
    return("negative")
  }
}
sign(3)
sign(-3)
print(sign(0))  # force a print


## -----------------------------------------------------------
sign <- function(x) {
  if (x > 0) {
    return("positive")
  } else if (x < 0) {
    return("negative")
  } else if (x == 0) {
    return("neither positive nor negative")
  }
}


## -----------------------------------------------------------
sign(0)


## -----------------------------------------------------------
sign <- function(x) {
  if (x > 0) {
    return("positive")
  } else if (x < 0) {
    return("negative")
  } else {
    return("neither positive nor negative")
  }
}


## -----------------------------------------------------------
sign(0)


## -----------------------------------------------------------
get_capital <- function(x) {
  if (x == "Florida") {
    return("Talahassee")
  } else if (x == "Georgia") {
    return("Atlanta")
  } else if (x == "Alabama") {
    return("Montgomery")
  } else {
    return("Oops, don't know where that is")
  }
}


## -----------------------------------------------------------
some_students <- tibble(
  name = c("Xiao", "Renji", "Timmy", "Christina"), 
  state = c("Florida", "Florida", "Alabama", "California")
)
some_students


## -----------------------------------------------------------
some_students |>
  mutate(capitol = map_chr(state, get_capital))


## ---- message = FALSE, warning = FALSE----------------------
library(tidyverse)


## -----------------------------------------------------------
one_flip <- function(x) {
  if (x == "heads") {
    return(2)
  } else if (x == "tails") {
    return(-1)
  } 
}


## -----------------------------------------------------------
c(one_flip("heads"), one_flip("tails"))


## -----------------------------------------------------------
sides <- c("heads","tails")
one_flip(sample(sides, size = 1))


## -----------------------------------------------------------
one_flip(sample(c("heads","tails"), size = 1))


## -----------------------------------------------------------
betting_one_round <- function() {
  # Net gain on one bet
  x <- sample(c("heads","tails"), size = 1) 
  if (x == "heads") {
    return(2)
  } else if (x == "tails") {
    return(-1)
  } 
}


## -----------------------------------------------------------
betting_one_round()


## -----------------------------------------------------------
for (hand in c("rock", "paper", "scissors")) {
  print(hand)
}


## -----------------------------------------------------------
for (season in c("spring", "summer", "fall", "winter")) {
  print(season)
}


## ---- error=TRUE--------------------------------------------
1:5
30:20
a <- 18
b <-  7
a:b
b:a


## -----------------------------------------------------------
for (i in 1:5) {
  print(i)
}


## -----------------------------------------------------------
for (i in 1:10) {
  print(betting_one_round())
}


## -----------------------------------------------------------
rounds <- 10
outcomes <- vector("integer", rounds)
for (i in 1:rounds) {
  outcomes[i] <- betting_one_round()
}


## -----------------------------------------------------------
outcomes


## -----------------------------------------------------------
rounds <- 10
outcomes <- replicate(n = 10, betting_one_round())


## -----------------------------------------------------------
outcomes


## -----------------------------------------------------------
sum(outcomes)


## -----------------------------------------------------------
rounds <- 1000
outcomes <- replicate(n = rounds, betting_one_round())


## -----------------------------------------------------------
length(outcomes)


## -----------------------------------------------------------
sum(outcomes)


## ----dpi=80,  fig.align="center", message = FALSE-----------
outcome_df <- tibble(outcomes)
ggplot(outcome_df, aes(x = outcomes)) + 
  geom_bar() + 
  coord_flip()


## ---- message = FALSE, warning = FALSE----------------------
library(tidyverse)


## -----------------------------------------------------------
sides <- c("heads", "tails")


## -----------------------------------------------------------
tosses <- sample(sides, size = 8, replace = TRUE)
tosses


## -----------------------------------------------------------
sum(tosses == "heads")


## -----------------------------------------------------------
outcomes <- sample(sides, size = 100, replace = TRUE)
num_heads <- sum(outcomes == "heads")
num_heads


## -----------------------------------------------------------
one_trial <- function() {
  outcomes <- sample(sides, size = 100, replace = TRUE)
  num_heads <- sum(outcomes == "heads")
  return(num_heads)
} 


## -----------------------------------------------------------
one_trial()


## -----------------------------------------------------------
# Number of repetitions
num_repetitions <- 10000

# simulate the experiment!
heads <- replicate(n = num_repetitions, one_trial())


## -----------------------------------------------------------
length(heads)


## -----------------------------------------------------------
heads[1]
heads[2]


## -----------------------------------------------------------
results <- tibble(
  repetition = 1:num_repetitions,
  num_heads = heads
)


## -----------------------------------------------------------
results


## ----dpi=80, fig.align="center", message = FALSE------------
ggplot(results) + 
  geom_histogram(aes(x = num_heads, y = after_stat(density)), 
                 color = "gray", fill = "darkcyan", 
                 breaks = seq(30.5, 69.6, 1))


## -----------------------------------------------------------
sample(c(-1, 0, 1), 1, prob=c(1/4, 2/4, 1/4))


## -----------------------------------------------------------
after_clerk_calculation <- function(grains) {
  grains + sample(c(-1, 0, 1), 1, prob=c(1/4, 2/4, 1/4))
}


## -----------------------------------------------------------
after_clerk_calculation(10)


## -----------------------------------------------------------
2 ** (0:9)


## ----echo=FALSE---------------------------------------------
set.seed(2)


## -----------------------------------------------------------
map_dbl(2 ** (0:9), after_clerk_calculation)


## ----echo=FALSE---------------------------------------------
set.seed(2)


## -----------------------------------------------------------
map_dbl(2 ** (0:9), after_clerk_calculation) |>
  sum()


## -----------------------------------------------------------
total_grains_received <- function(num_days) {
  map_dbl(2 ** (0:(num_days - 1)), after_clerk_calculation) |> 
    sum()
}


## -----------------------------------------------------------
total_grains_received(10)


## -----------------------------------------------------------
# Number of repetitions
num_repetitions <- 10000

# simulate the experiment!
grains <- replicate(n = num_repetitions, total_grains_received(10))


## -----------------------------------------------------------
results <- tibble(
  repetition = 1:num_repetitions,
  num_grains = grains
)
results


## ----dpi=80, fig.align="center", message = FALSE------------
ggplot(results) + 
  geom_histogram(aes(x = num_grains, y = after_stat(density)), 
                 color = "gray", fill = "darkcyan", bins = 18) + 
  geom_point(aes(x = 1023, y = 0), color = "salmon", size = 3)


## -----------------------------------------------------------
delicious_fruits <- c("apple", "banana", "pineapple", "mango")


## -----------------------------------------------------------
accumulate(delicious_fruits, \(acc, nxt) str_c(acc, nxt, sep = ":"))


## -----------------------------------------------------------
# str_c from stringr with collapse
str_c(delicious_fruits, collapse = ":")

# using accumulate!
accumulate(delicious_fruits, \(acc, nxt) str_c(acc, nxt, sep = ":")) |>
  last()


## -----------------------------------------------------------
accumulate(delicious_fruits, \(acc, nxt) str_c(acc, nxt, sep = ":"), 
           .init = "a")


## -----------------------------------------------------------
accumulate(541:546, \(acc, nxt) acc + 10, .init = 10)


## -----------------------------------------------------------
grains_after_day <- function(current_grains) {
  new_amount <- current_grains + after_clerk_calculation(current_grains)
  return(max(1, new_amount))
}


## -----------------------------------------------------------
accumulate(1:10, \(acc, nxt) grains_after_day(acc), .init = 1)


## -----------------------------------------------------------
total_grains_received <- function(num_days) {
  accumulate(1:10, \(acc, nxt) grains_after_day(acc), .init = 1) |> 
    last()
}


## -----------------------------------------------------------
num_repetitions <- 10000
grains <- replicate(n = num_repetitions, total_grains_received(10))


## -----------------------------------------------------------
grains |>
  tibble() |> 
  ggplot() + 
  geom_histogram(aes(x = grains, y = after_stat(density)), 
                 bins = 18, color = "gray", fill = "darkcyan") +
  geom_point(aes(x = 1023, y = 0), color = "salmon", size = 3)


## ---- message = FALSE, warning = FALSE----------------------
library(tidyverse)


## -----------------------------------------------------------
chosen_birthdays <- sample(1:365, size=23, replace=TRUE)
chosen_birthdays 


## -----------------------------------------------------------
sum(duplicated(chosen_birthdays))


## -----------------------------------------------------------
any_duplicates_in_class <- function() {
  chosen_bdays <- sample(1:365, size=23, replace=TRUE)
  num_duplicates <- sum(duplicated(chosen_bdays))
  if (num_duplicates > 0) {
    return(1)
  } else {
    return(0)
  }
}


## -----------------------------------------------------------
any_duplicates_in_class()


## -----------------------------------------------------------
one_birthday_trial <- function() {
  classrooms <- 100
  num_duplicates <- replicate(n = classrooms, any_duplicates_in_class())
  return(sum(num_duplicates) / classrooms)
}


## -----------------------------------------------------------
one_birthday_trial()


## -----------------------------------------------------------
# Number of repetitions 
num_repetitions <- 10000

# simulate the experiment!
bday_proportions <- replicate(n = num_repetitions, one_birthday_trial())

# and done!


## -----------------------------------------------------------
results <- tibble(
  repetition = 1:num_repetitions,
  proportions = bday_proportions
)
results


## ----dpi=80, fig.align="center", message = FALSE------------
ggplot(results) + 
  geom_histogram(aes(x = proportions, y = after_stat(density)), 
                 color = "gray", fill = "darkcyan", 
                 breaks = seq(0.35, 0.65, 0.01)) + 
  geom_point(aes(x = 0.51, y = 0), color = "salmon", size = 3)


## ----eval=FALSE---------------------------------------------
## library(tidyverse)
## library(edsdata)
## library(gapminder)


## ----message=FALSE, warning=FALSE---------------------------
alana


## ----eval=FALSE, message=FALSE, warning=FALSE---------------
## 
## 
## findings_from_one_walk(5) # an example call for a simulated "day 5"


## ----eval=FALSE, message=FALSE, warning=FALSE---------------
## map(1:80, findings_from_one_walk) |>
##   bind_rows()


## ----eval=FALSE, message=FALSE, warning=FALSE---------------
## 
## 
## one_alana_trial() # an example call


## ----eval=FALSE, message=FALSE, warning=FALSE---------------
## observed_witness3


## ----eval=FALSE, message=FALSE, warning=FALSE---------------
## jodie_predictions <- c("Tails", "Tails", "Heads",
##                        "Heads", "Tails", "Tails",
##                        "Heads", "Tails", "Tails",
##                        "Heads")
## outcomes          <- c("Tails", "Heads", "Heads",
##                        "Heads", "Heads", "Tails",
##                        "Heads", "Heads", "Heads",
##                        "Tails")


## ----eval=FALSE, message=FALSE, warning=FALSE---------------
## simulate_jodie_prediction <- function() {
##   # Jodie chooses either side of a coin with equal chance.
##   sample(c("Heads", "Tails"), prob = c(1/2, 1/2), size = 1)
## }
## simulate_cathy_coin <- function() {
##   # Cathy's coin is known to be biased towards "Heads" in a 3:2 ratio.
##   sample(c("Heads", "Tails"), prob = c(3/5, 2/5), size = 1)
## }


## ----eval=FALSE, message=FALSE, warning=FALSE---------------
## 
## 
## # An example call using the functions corresponding
## # to the game Cathy and Jodie are playing.
## one_flip_lollipop_wins(simulate_cathy_coin, simulate_jodie_prediction)


## ----eval=FALSE, message=FALSE, warning=FALSE---------------
## 
## # One round consists of 10 coin flips.
## gains_after_one_round(10, simulate_cathy_coin,
##                       simulate_jodie_prediction)


## ----eval=FALSE, message=FALSE, warning=FALSE---------------
## hist_from_simulation <- function(simulated_results) {
##   wins_tibble <- tibble(
##     repetition = 1:length(simulated_results),
##     gain = simulated_results
##   )
## 
##   g <- ggplot(wins_tibble) +
##     geom_histogram(aes(x = gain, y = after_stat(density)),
##                    bins=12,
##                    color = "gray", fill = "darkcyan")
##   return(g)
## }
## 
## hist_from_simulation(simulated_gains)


## ----eval=FALSE---------------------------------------------
## gains_after_one_round(1, simulate_cathy_coin, simulate_cathy_prediction)


## -----------------------------------------------------------
total_lollipops_after_play <- function(jodie_lollipops) {

}


## ----eval=FALSE---------------------------------------------
## play_until_empty_pile <- function() {
## 
## }


## ----eval=FALSE---------------------------------------------
## play_until_empty_pile()


## ----eval=FALSE, message=FALSE, warning=FALSE---------------
## simulate_dice_roll <- function() {
##   sample(1:6, size = 1)
## }
## simulate_dice_roll()


## ----eval=FALSE, message=FALSE, warning=FALSE---------------
## 
## has_one_appeared() # an example call


## ----eval=FALSE, message=FALSE, warning=FALSE---------------
## 
## 
## has_double_ones_appeared() # an example call


## ----eval=FALSE, message=FALSE, warning=FALSE---------------
## 
## chavalier_first_event() # example call


## ----eval=FALSE, message=FALSE, warning=FALSE---------------
## sim_results <- tibble(
##   first_event = simulated_values_first_event,
##   second_event = simulated_values_second_event)
## sim_results


## ---- echo=FALSE, fig.align="center", fig.asp=1/2-----------
knitr::include_graphics('images/monty_hall_goat.png')


## ---- message = FALSE, warning = FALSE----------------------
library(tidyverse)


## -----------------------------------------------------------
goats <- c("goat1", "goat2")


## -----------------------------------------------------------
other_goat <- function(x) {
  if (x == "goat1") {
    return("goat2")
  } else if (x == "goat2") {
    return("goat1")
  }
} 


## -----------------------------------------------------------
other_goat("goat1")
other_goat("goat2")
print(other_goat("goose"))


## -----------------------------------------------------------
hidden_behind_doors <- c("goat1", "goat2", "car")


## -----------------------------------------------------------
monty_hall_game <- function(x) {
  # Returns (contestant's guess, what Monty reveals, what remains behind the other door)
  pick <- sample(hidden_behind_doors, size = 1) 
  if (pick == "goat1") {
    return(c(pick, "goat2", "car"))
  } 
  
  if (pick == "goat2") {
    return(c(pick, "goat1", "car"))
  } 
  
  if (pick == "car") { 
    revealed <- sample(goats, size = 1)
    return(c(pick, revealed, other_goat(revealed)))
  }
}


## -----------------------------------------------------------
monty_hall_game()


## -----------------------------------------------------------
num_repetitions <- 10000
games <- map(1:num_repetitions, monty_hall_game)


## -----------------------------------------------------------
games[[1]]
games[[2]]
games[[3]]


## -----------------------------------------------------------
guess_column = map_chr(games, function(x) x[1])
head(guess_column)


## -----------------------------------------------------------
revealed_column = map_chr(games, function(x) x[2])
remaining_column = map_chr(games, function(x) x[3])


## -----------------------------------------------------------
game_results <- tibble(
  guess = guess_column,
  revealed = revealed_column,
  remaining = remaining_column
)
game_results


## -----------------------------------------------------------
original_choice <- game_results |> 
  group_by(guess) |>
  count() |>
  mutate(which_door = "original")
original_choice


## -----------------------------------------------------------
remaining_doors <- game_results |> 
  group_by(remaining) |>
  count() |>
  mutate(which_door = "remaining")
remaining_doors


## -----------------------------------------------------------
original_choice <- original_choice |> rename(item = guess)
remaining_doors <- remaining_doors |> rename(item = remaining)
stuck_together <- bind_rows(original_choice, remaining_doors)
stuck_together


## ----dpi=80,  fig.align="center", message = FALSE-----------
ggplot(stuck_together, aes(x = item, y = n, fill = which_door)) + 
  geom_bar(position="dodge", stat = "identity") + 
  coord_flip()


## ---- message = FALSE, warning = FALSE----------------------
library(tidyverse)


## -----------------------------------------------------------
rolls <- 1:51
results <- tibble(rolls = rolls,
                  prob_of_at_least_one_six = 1 - (5/6) ** rolls)
results


## ----dpi=80,  fig.align="center", message = FALSE-----------
ggplot(results) + 
  geom_point(aes(x = rolls, y = prob_of_at_least_one_six))


## -----------------------------------------------------------
filter(results, rolls == 50) |> as.data.frame()

