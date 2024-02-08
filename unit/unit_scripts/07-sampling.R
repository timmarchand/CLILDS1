library(tidyverse)

mpg

mpg_sub <- mpg |>
  slice(c(3, 25, 100))
mpg_sub

mpg_sub <- mpg |>
  slice(c(3, 25, 3, 25, 3, 25, 3, 25))
mpg_sub

mpg_sub <- mpg |>
  filter(manufacturer == "land rover")
mpg_sub

mpg_with_index <- mpg |>
  mutate(row_index = row_number()) %>%
  relocate(row_index, .before = manufacturer)
mpg_with_index

# Pick random start among rows 0 through 9; then every 10th row.
start <- sample(1:10, size = 1)
mpg_with_index |> 
  slice(seq(start, n(), by = 10))

library(tidyverse)

die <- tibble(face = 1:6)
die

ggplot(die) + 
  geom_histogram(aes(x = face, y =  after_stat(density)), 
                 bins = 6, color = "gray")

die |>
  slice_sample(n = 10, replace = TRUE)

sample_hist <- function(n) {
  die_sample <- die |> 
    slice_sample(n = n, replace = TRUE)
  ggplot(die_sample, aes(x = face, y = after_stat(density))) + 
    geom_histogram(bins = 6, color = "gray")
}

sample_hist(10)

sample_hist(100)

sample_hist(1000)

die |>
  slice_sample(n = 6, replace = FALSE)

ggplot(die) + 
  geom_histogram(aes(x = face, y =  after_stat(density)), bins = 6, 
                 color = "gray")

selected_face <- 2  # assume a 2 was rolled
slice(die, -selected_face) |>
  ggplot() + 
  geom_histogram(aes(x = face, y =  after_stat(density)), bins = 6, 
                 color = "gray")

library(tidyverse)
library(nycflights13)

flights

slice_min(flights, dep_delay)

slice_max(flights, dep_delay)

ggplot(flights, aes(x = dep_delay, y = after_stat(density))) + 
  geom_histogram(col="grey", breaks = seq(-50, 200, 1))

nrow(filter(flights, dep_delay > 150)) / nrow(flights)

delay_bins <- seq(-50, 150, 1)
ggplot(flights, aes(x = dep_delay, y = after_stat(density))) + 
  geom_histogram(col="grey", breaks = delay_bins)

delay_bins <- seq(-50, 150, 10)
ggplot(flights, aes(x = dep_delay, y = after_stat(density))) + 
  geom_histogram(col="grey", breaks = delay_bins)

nrow(filter(flights, dep_delay > -10 & dep_delay <= 0)) / nrow(flights)

sample_hist <- function(n) {
  flights_sample <- slice_sample(flights, n = n, replace = TRUE)
  ggplot(flights_sample, aes(x = dep_delay, y = after_stat(density))) + 
    geom_histogram(breaks = delay_bins, color = "gray")
}

sample_hist(10)

sample_hist(100)

sample_hist(1000)

library(tidyverse)

some_numbers <- c(8, 1, 8, 7, 9)
mean(some_numbers)

ones_and_zeros <- c(1, 1, 0, 0)
sum(ones_and_zeros)

mean(ones_and_zeros)

tibble(some_numbers) |>
  ggplot() + 
  geom_histogram(aes(x = some_numbers, y = after_stat(density)),
                 color = "gray", fill = "darkcyan", binwidth = 1)

tibble(some_numbers) |>
  ggplot() + 
  geom_histogram(aes(x = some_numbers, y = after_stat(density)),
                 color = "gray", fill = "darkcyan", binwidth = 1) +
  geom_point(aes(x = mean(some_numbers), y = -0.02), 
             color = "salmon", shape = 17, size = 4)

symmetric <- c(8, 8, 8, 7, 9)

ggplot(tibble(symmetric)) + 
geom_histogram(aes(x = symmetric, y = after_stat(density)),
               bins = 3,
               color = "gray", fill = "darkcyan") +
geom_point(aes(x = mean(symmetric), y = -0.02), 
           color = "salmon", shape = 17, size = 4)

mean(symmetric)
median(symmetric)

ggplot() + 
  geom_histogram(data = tibble(symmetric), aes(
                       x = symmetric, y = after_stat(density)),
                 color = "gray", fill = "darkcyan", binwidth = 1) +
  geom_histogram(data = tibble(some_numbers), aes(
                     x = some_numbers, y = after_stat(density)), alpha = 0.8,
                 color = "gray", fill = "salmon", binwidth = 1) + 
  geom_point(aes(x = mean(symmetric), y = -0.02), 
             color = "darkcyan", shape = 17, size = 4) + 
  geom_point(aes(x = mean(some_numbers), y = -0.02), 
             color = "salmon", shape = 17, size = 4)

median(some_numbers)
mean(some_numbers)

lots_of_repetition <- c(8, 8, 8, 1, 1, 8, 7, 7, 9, 9)
mean(lots_of_repetition)

library(tidyverse)
library(nycflights13)

delay_bins <- seq(-50, 150, 10)
ggplot(flights, aes(x = dep_delay, y = after_stat(density))) + 
  geom_histogram(col="grey", breaks = delay_bins)

sample_1000 <- slice_sample(flights, n = 1000, replace = TRUE)
ggplot(sample_1000, aes(x = dep_delay, y = after_stat(density))) + 
  geom_histogram(col="grey", breaks = delay_bins)

flights |>
  pull(dep_delay) |>
  median(na.rm = TRUE)

nrow(filter(flights, dep_delay <= -2)) / nrow(flights)

sample_1000 |>
  pull(dep_delay) |>
  median(na.rm = TRUE)

slice_sample(flights, n = 1000, replace = TRUE) |>
  pull(dep_delay) |>
  median(na.rm = TRUE)

set.seed(3)

lucky_number <- 6
d30_dice <- 1:30
sample(d30_dice, size = 10, replace=TRUE) + lucky_number

one_game <- function(lucky_number) {
  sample(1:30, size = 10, replace=TRUE) + lucky_number
}
one_game(6)

min_based <- function(sample) {
  min(sample) - 1
}

sample_rolls <- sample(d30_dice, size = 10000, replace = TRUE)

d30_expected_value <- mean(sample_rolls)
d30_expected_value

mean_based <- function(sample) {
  mean(sample) - d30_expected_value
}

simulate_one_stat <- function(estimator) {
  one_game(lucky_number = 6) |>
    estimator()
}

simulate_one_stat(min_based) # an example call 

reps <- 10000

mean_estimates <- replicate(n = reps, simulate_one_stat(mean_based)) 
min_estimates <- replicate(n = reps, simulate_one_stat(min_based)) 

estimate_tibble <- tibble(mean_est = mean_estimates, 
                      min_est = min_estimates) %>%
  pivot_longer(c(mean_est, min_est), 
               names_to = "estimator", values_to = "estimate")
estimate_tibble

bins <- seq(0, 22, 1)

dist_mean_tib <- tibble(
  estimator = c("min_est", "mean_est"),
  mean = c(min_estimates |> mean(), 
           mean_estimates |> mean()))
  
ggplot(estimate_tibble) + 
  geom_histogram(aes(x = estimate, y = after_stat(density),
                     fill = estimator), 
                 position = "identity", alpha = 0.5,
                 color = "gray", breaks = bins) +
  geom_point(data = dist_mean_tib, 
             aes(x = mean, y = 0, color = estimator), 
             shape = "triangle", size = 3) +
  scale_x_continuous(breaks = bins)

sampled <- flights |>
  slice_sample(n = 1000, replace = TRUE)

sampled |>
  pull(dep_delay) |>
  median(na.rm = TRUE)

one_sample_median <- function() {
  sample_median <- flights |>
    slice_sample(n = 1000, replace = TRUE) |>
    pull(dep_delay) |>
    median(na.rm = TRUE)
  return(sample_median)
}

num_repetitions <- 5000
medians <- replicate(n = num_repetitions, one_sample_median())

medians_df <- tibble(medians)
medians_df

ggplot(medians_df) + 
  geom_histogram(aes(x = medians, y = after_stat(density)), 
                 color = "gray", fill = "darkcyan", bins = 3) +
  geom_point(aes(x = mean(medians), y = 0), 
             shape = "triangle", 
             color = "salmon", size = 3)

library(tidyverse)
library(nycflights13)
library(edsdata)

median_delay <- flights |>
  pull(dep_delay) |>
  median(na.rm = TRUE)
median_delay

start <- sample(1:nrow(flights), size = 1)  
start

selected_rows <- seq(start, nrow(flights), 100)  

slice(flights, selected_rows) |> 
  ggplot(aes(x = dep_delay, y = after_stat(density))) + 
  geom_histogram(fill = "darkcyan", color = "gray", 
                 breaks = seq(-50, 150, 10)) +
  ggtitle(str_c("starting row = ", start))

mystery_flights <- mystery_flights |> 
  relocate(ID, .before = year)
mystery_flights 

start <- 2

selected_rows <- seq(start, nrow(mystery_flights), 100)  

slice(mystery_flights, selected_rows) |> 
  ggplot(aes(x = dep_delay, y = after_stat(density))) + 
  geom_histogram(fill = "darkcyan", color = "gray", 
                 breaks = seq(-50, 150, 10)) +
  ggtitle(str_c("starting row = ", start))

tibble(row_index = seq(2, nrow(mystery_flights), 100))

mystery_flights |>
  slice(c(2, 102, 202, 302, 402))

mystery_flights |>
  slice(c(1, 101, 201, 301, 401))

## library(tidyverse)
## library(edsdata)
## library(gapminder)

## twenty_sided_die_roll <- function() {
##   return(sample(seq(1:20), size = 1))
## }
## twenty_sided_die_roll()

library(edsdata)
sf_salary

## 
## # an example call using the full data
## plot_salary_histogram(with_first_letter)

## 
## plot_and_compute_mean_stat(with_first_letter) # an example call

## 
## filter_letter("Z") # an example call

## 
## max_based_estimate(one_sample) # an example call

## 
## mean_based_estimate(one_sample) # an example call

## 
## simulate_one_stat(dream_with_id, mean_based_estimate) # example call

## stats_tibble <- tibble(max_est = max_estimates,
##                       mean_est = mean_estimates) |>
##   pivot_longer(c(max_est, mean_est),
##                names_to = "estimator", values_to = "estimate")
## stats_tibble

## 
## mean_stat_from_sample(sf_salary11) # using the full data

## stat_tibble <- tibble(rep=1:10000,
##                       mean=mean_stats)

## letter_tibble <- tibble(
##   letter = LETTERS[1:26],
##   stat = letter_group_stats)
