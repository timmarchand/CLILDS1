mystery_coin <- tibble(face = c("Heads", "Tails"),
                       face_counts = c(4953, 5047)) 
mystery_coin

mystery_coin <- mystery_coin |>
  mutate(face_prop = face_counts / 10000)
mystery_coin

mystery_coin <- mystery_coin |>
  mutate(fair_prop = 1/2,
         abs_diff = abs(face_prop - fair_prop))
mystery_coin

mystery_coin |>
  summarize(tvd_value = sum(abs_diff) / 2)

observed_heads <- mystery_coin |> 
  filter(face == "Heads") |> 
  pull(face_counts)
observed_heads

library(tidyverse)

fair_coin <- c(1/2, 1/2)
sample_vector <- rmultinom(n = 1, size = 100, prob = fair_coin)
sample_vector

sample_vector / 100

sample_tibble <- tibble(
  face = c("Heads", "Tails"), 
  fair_probs = 1/2, 
  sample_counts = rmultinom(n = 1,
                          size = 10000,
                          prob = fair_probs))
sample_tibble

sample_heads <- sample_tibble |>
  filter(face == "Heads") |>
  pull(sample_counts)
sample_heads

one_simulated_statistic <- function() {
  sample_tibble <- tibble(
    face = c("Heads", "Tails"), 
    fair_probs = 1/2, 
    sample_counts = rmultinom(n = 1,
                            size = 10000,  
                            prob = fair_probs))
  
  sample_heads <- sample_tibble |>
    filter(face == "Heads") |>
    pull(sample_counts)
  
  return(sample_heads)
}

set.seed(1)

num_repetitions <- 1000
sample_stats  <- replicate(n = num_repetitions, 
                           one_simulated_statistic())

ggplot(tibble(sample_stats)) +
  geom_histogram(aes(x = sample_stats, y = after_stat(density)), 
                 fill = "darkcyan", color = 'gray', bins=14) +
  labs(x = "Number of heads")

ggplot(tibble(sample_stats)) +
  geom_histogram(aes(x = sample_stats, y = after_stat(density)), 
                 fill = "darkcyan", color = 'gray', bins = 12) +
  geom_point(aes(x = observed_heads, y = 0), size = 3, 
             color = "salmon") +
  labs(x = "Number of heads")

sum(sample_stats <= observed_heads) / length(sample_stats)

colors <- c(rep("salmon",4), rep("darkcyan",8))

ggplot(tibble(sample_stats)) +
  geom_histogram(aes(x = sample_stats, y = after_stat(density)), 
                 bins = 12, fill = colors, color = 'gray') +
  geom_point(aes(x = observed_heads, y = 0), size = 3, color = "red") +
  labs(x = "Number of heads")

library(tidyverse)

class_props <- tribble(~Race, ~Harvard, ~Almanac,
                             "White", 46.1, 63.6, 
                             "Black", 14.7, 11.5,
                             "Hispanic", 12.7, 12.3,
                             "Asian", 24.4, 8.1,
                             "Other", 2.1, 4.5)
class_props

class_props <- class_props |> 
  mutate(Harvard = Harvard / 100,
         Almanac = Almanac / 100)
class_props

compute_tvd <- function(x, y) {
  return(sum(abs(x - y)) / 2)
}

harvard_diff <- class_props |>
  summarize(compute_tvd(Harvard, Almanac)) |>
  pull()
harvard_diff

sample_vector <- rmultinom(n=1, size=2015, 
                           prob = pull(class_props, Almanac)) / 2015
sample_vector

class_props |>
  mutate(sample = sample_vector) |>
  summarize(compute_tvd(sample, Almanac)) |>
  pull()

one_simulated_class <- function(props) {
  props |>
    mutate(sample = rmultinom(n=1, size=2015, prob = Almanac) / 2015) |>
    summarize(compute_tvd(sample, Almanac)) |>
    pull()
}

num_repetitions <- 10000
sample_class_tvds <- replicate(n = num_repetitions, 
                               one_simulated_class(class_props))

ggplot(tibble(sample_class_tvds)) +
  geom_histogram(aes(x = sample_class_tvds, y = after_stat(density)), 
                 bins = 30, fill = "darkcyan", color = 'gray')

ggplot(tibble(sample_class_tvds)) +
  geom_histogram(aes(x = sample_class_tvds, y = after_stat(density)), 
                 bins = 70, fill = "darkcyan", color = 'gray') +
  geom_point(aes(x = harvard_diff, y = 0), size = 3, color = "salmon")

class_props_asian <- tribble(~Race, ~Harvard, ~Almanac,
                             "Asian", 24.4, 8.1,
                             "Other", 75.6, 91.9)
class_props_asian

class_props_asian <- class_props_asian |> 
  mutate(Harvard = Harvard / 100,
         Almanac = Almanac / 100)
class_props_asian

harvard_diff <- class_props_asian |>
  filter(Race == "Asian") |>
  summarize(abs(Harvard - Almanac)) |>
  pull()
harvard_diff

num_repetitions <- 10000
sample_class_tvds <- replicate(n = num_repetitions, 
                        one_simulated_class(class_props_asian))

ggplot(tibble(sample_class_tvds)) +
  geom_histogram(aes(x = sample_class_tvds, y = after_stat(density)), 
                 binwidth=0.002, fill = "darkcyan", color = 'gray')

ggplot(tibble(sample_class_tvds)) +
  geom_histogram(aes(x = sample_class_tvds, y = after_stat(density)), 
                 binwidth=0.002, fill = "darkcyan", color = 'gray') +
  geom_point(aes(x = harvard_diff, y = 0), size = 3, color = "salmon")

library(tidyverse)
library(edsdata)

set.seed(1)

csc_labs

lab_stats <- csc_labs |>
  group_by(section) |>
  summarize(midterm_avg = mean(midterm),
            count = n())
lab_stats

observed_statistic <- lab_stats |>
  filter(section == "H") |>
  pull(midterm_avg)
observed_statistic

random_sample <- csc_labs |>
  select(midterm) |>
  slice_sample(n = 9, replace = FALSE)
random_sample

random_sample |>
  summarize(mean(midterm)) |>
  pull()

one_simulated_mean <- function() {
  random_sample <- csc_labs |>
    select(midterm) |>
    slice_sample(n = 9, replace = FALSE)
  
  random_sample |>
    summarize(mean(midterm)) |>
    pull()
}

num_repetitions <- 10000
sample_means <- replicate(n = num_repetitions, one_simulated_mean())

ggplot(tibble(sample_means)) +
  geom_histogram(aes(x = sample_means, y = after_stat(density)), 
                 bins = 15, fill = "darkcyan", color = 'gray')

ggplot(tibble(sample_means)) +
  geom_histogram(aes(x = sample_means, y = after_stat(density)), 
                 bins = 15, fill = "darkcyan", color = 'gray') + 
  geom_point(aes(x = observed_statistic, y = 0), 
             size = 3, color = "salmon")

sorted_samples <- sort(sample_means)
sorted_samples[length(sorted_samples)*0.1]

ggplot(tibble(sample_means)) +
  geom_histogram(aes(x = sample_means, y = after_stat(density)), 
                 bins = 15, fill = "darkcyan", color = 'gray') +
  geom_vline(xintercept = sorted_samples[length(sorted_samples)*0.1],
             size = 2, color = "purple")

sorted_samples[length(sorted_samples)*0.05]

ggplot(tibble(sample_means)) +
  geom_histogram(aes(x = sample_means, y = after_stat(density)), bins = 15,
                 fill = "darkcyan", color = 'gray') +
  geom_vline(xintercept = sorted_samples[length(sorted_samples)*0.05], 
             size = 2, color = "purple")

ggplot(tibble(sample_means)) +
  geom_histogram(aes(x = sample_means, y = after_stat(density)), bins = 15,
                 fill = "darkcyan", color = 'gray') +
  geom_vline(xintercept = sorted_samples[length(sorted_samples)*0.05], 
             size = 2, color = "purple") + 
  geom_point(aes(x = observed_statistic, y = 0), size = 3, color = "red")

colors <- c(rep("salmon",8), rep("darkcyan",7))
ggplot(tibble(sample_means)) +
  geom_histogram(aes(x = sample_means, y = after_stat(density)), bins = 15,
                 fill = colors, color = 'gray') +
  geom_point(aes(x = observed_statistic, y = 0), size = 3, color = "red")

sum(sample_means <= observed_statistic) / num_repetitions

colors <- c(rep("salmon",6), rep("darkcyan",5), rep("salmon",4))
ggplot(tibble(sample_means)) +
  geom_histogram(aes(x = sample_means, y = after_stat(density)), bins = 15,
                 fill = colors, color = 'gray') +
  guides(x = "none", y = "none") + 
  labs(x = "", y="")

library(tidyverse)
library(edsdata)

finals

finals |>
  group_by(class) |>
  count()

ggplot(finals) + 
  geom_histogram(aes(x = grade, y = after_stat(density), fill = class),
                 bins = 10, color = "gray",
                 alpha = 0.7, position = "identity")

finalsA <- finals |>
  filter(class == 'A') |> pull(grade)
finalsB <- finals |>
  filter(class == 'B') |> pull(grade)

observed_statistic <- mean(finalsB) - mean(finalsA)
observed_statistic

mean_diff <- function(a, b) {
  return(mean(a) - mean(b))
}

mean_diff(finalsB, finalsA)

1:10

shuffled <- sample(1:10)
shuffled

shuffled[1:5]  # shuffled group A
shuffled[6:10] # shuffled group B

one_difference <- function(a, b, compute_statistic) {
  pot <- c(a, b)
  sample_indices <- sample(1 : length(pot))
  shuffled_a <- pot[sample_indices[1 : length(a)]]
  shuffled_b <- pot[sample_indices[(length(a) + 1) : length(pot)]]
  return(compute_statistic(shuffled_a, shuffled_b))
}

differences <- replicate(n = 10000, 
                         one_difference(finalsA, finalsB, mean_diff))

ggplot(tibble(differences)) + 
  geom_histogram(aes(x = differences, y = after_stat(density)), 
                 col="grey", fill = "darkcyan", bins = 20) +
  geom_point(aes(x = observed_statistic, y = 0), 
             color = "salmon", size = 3)

sum(differences >= observed_statistic) / 10000

my_athletes <- athletes |>
  filter(Year > 2000)

my_athletes |>
  count(Season) |>
  mutate(prop = n / sum(n))

my_athletes |>
  ggplot() + 
    geom_histogram(aes(x = Weight, y = after_stat(density), 
                       fill = Season),
                   bins = 13, color = "gray",
                   alpha = 0.7, position = "identity")

mean_abs_diff <- function(a, b) {
  return(abs(mean(a) - mean(b)))
}

winter_weights <- my_athletes |> 
  filter(Season == "Winter") |> 
  pull(Weight)
summer_weights <- my_athletes |> 
  filter(Season == "Summer") |> 
  pull(Weight)

observed_statistic <- mean_abs_diff(winter_weights, summer_weights)
observed_statistic

set.seed(3)

differences <- replicate(n = 10000, 
        one_difference(winter_weights, summer_weights, mean_abs_diff))

ggplot(tibble(differences)) + 
  geom_histogram(aes(x = differences, y = after_stat(density)), 
                 col="grey", fill = "darkcyan", bins = 15) +
  geom_point(aes(x = observed_statistic, y = 0), 
             color = "salmon", size = 3)

sum(differences >= observed_statistic) / 10000

## library(tidyverse)
## library(edsdata)
## library(gapminder)

galaxy_acceptance <- tribble(
  ~Ethnicity, ~Applied, ~Accepted,
  "White",  925, 811, 
  "NH/OPI", 50, 7, 
  "Hispanic", 601, 348, 
  "Black", 331, 236, 
  "Asian", 237, 101, 
  "AI/AN", 84, 30)
galaxy_acceptance

## ipeds2020 <- tribble(~Ethnicity, ~`2020`,
##     "White", 9316458,
##     "NH/OPI", 46144,
##     "Hispanic", 3538778,
##     "Black", 2254757,	
##     "Asian", 1285154,
##     "AI/AN",  115951)

## total_admitted <- galaxy_distribution |> pull(Accepted) |> sum()
## prop_accepted <- ipeds2020_dist |> pull(prop_accepted)
## rmultinom(n = 1, size = total_admitted, prob = prop_accepted)

## 
## compute_tvd(galaxy_distribution |> pull(prop_accepted)) # example

## 
## one_simulated_tvd()  # an example call

## ggplot(tibble(sample_tvds)) +
##   geom_histogram(aes(x = sample_tvds, y = after_stat(density)),
##                  bins = 15,
##                  fill = "darkcyan", color = 'gray') +
##   geom_point(aes(
##     x = compute_tvd(galaxy_distribution |> pull(prop_accepted)),
##                  y = 0), size = 3, color = "salmon")

weird_dice_probs <- c(1/6, 0/6, 3/6, 0/6, 1/6, 1/6)
rmultinom(n = 1, size = 10, prob = weird_dice_probs)

## set.seed(2022)
## jerry_die_dist <- tibble(
##   face = 1:6,
##   prob = sample_prop(weird_dice_probs, 10)
## )
## jerry_die_dist

## fair_die_dist <- tibble(
##   face = seq(1:6),
##   prob = rep(1/6, 6)
## )
## fair_die_dist

## ggplot(jerry_die_dist) +
##   geom_bar(aes(x = as.factor(face), y = prob), stat = "identity")

## jerry_die_dist |>
##   summarize(mean = sum(face * prob))

## fair_die_dist |>
##   summarize(mean = sum(face * prob))

## mystery_test_stat1 <- function(dist) {
##   x <- dist |>
##     summarize(mean = sum(face * prob)) |>
##     pull(mean)
##   y <- fair_die_dist |>
##     summarize(mean = sum(face * prob)) |>
##     pull(mean)
##   return(abs(x-y))
## }

## 
## one_simulated_stat(mystery_test_stat1) # an example call

## simulate_dice_experiment <- function(observed_dist, stat_func) {
## 
##   p_value_cutoff <- 0.05
##   print(paste("P-value: ",
##           (sum(test_stats >= observed_stat) / length(test_stats))))
##   ggplot(tibble(test_stats)) +
##     geom_histogram(aes(x = test_stats, y = after_stat(density)),
##                    bins=10, color = "gray", fill='darkcyan') +
##     geom_vline(aes(xintercept=quantile(test_stats,
##                                        1-p_value_cutoff)),
##                color='red') +
##     geom_point(aes(x=observed_stat,y=0),size=4,color='orange')
## }

## mystery_test_stat2 <- function(dist) {
##   sum(abs((dist |> pull(prob)) -
##             (fair_die_dist |> pull(prob))) /2)
## }
## 
## mystery_test_stat2(jerry_die_dist) # an example call

## experiment_rejects_null <- function(die_probs,
##                       stat_func, p_value_cutoff, num_repetitions) {
##   observed_dist <- tibble(
##     face = 1:6,
##     prob = sample_prop(die_probs, 10)
##   )
## 
## 
##   p_value <- sum(test_stats >= observed_stat) / num_repetitions
##   return(p_value < p_value_cutoff)
## }

house <- tribble(~gender, ~num_members,
                 "Female", 101, 
                 "Male",   334)
house

## sample(c("Female", "Male"), size = 10,
##        replace = TRUE, prob = c(0.5, 0.5))

## 
## one_sample_house <- function(gender_prop, house_size) {
## 
## 
## }
## 
## total_seats <- house |> pull(num_members) |> sum()
## one_sample_house(c(0.5, 0.5), total_seats) # an example call

## 
## simulate_one_stat(c(0.5, 0.5), 100) # an example call

## ggplot(tibble(test_stats)) +
##   geom_histogram(aes(x = test_stats), bins=18, color="gray") +
##   geom_point(aes(x = observed_value_house, y = 0),
##              size=2, color="red")

library(edsdata)
house_primary

## ggplot(tibble(test_stats)) +
##   geom_histogram(aes(x = test_stats), bins=18, color="gray") +
##   geom_point(aes(x = observed_value_house, y = 0),
##              size=2, color="red")

cathy_heads_game <- tibble(
  trial = 1:10, 
  num_heads = c(12, 13, 13, 11, 17, 10, 10, 14, 9, 15),
  num_tails = 20 - num_heads 
) 
cathy_heads_game

## 
## one_test_stat(10) # an example call after cathy's 10 trials

library(edsdata)
csc1234

## observed_diff_tibble <- tibble(
##   pair = c("A-B", "C-B", "C-A"),
##   test_stat = observed_differences
## )
## observed_diff_tibble

## 
## scores_permute_test() # an example call

## differences_tibble <- tibble(
##             `A-B` = test_stat_differences[1,],
##             `C-B` = test_stat_differences[2,],
##             `C-A` = test_stat_differences[3,])
## differences_tibble
