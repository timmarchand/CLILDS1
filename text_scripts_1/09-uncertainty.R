## ---- message = FALSE, warning = FALSE----------------------
library(tidyverse)
library(nycflights13)


## ----dpi=80,  fig.align="center", message = FALSE, warning = FALSE----
ggplot(flights) + 
  geom_histogram(aes(x = dep_delay, y = after_stat(density)), 
                 color="grey", bins = 30)


## -----------------------------------------------------------
flights150 <- flights |>
  filter(dep_delay <= 150)


## ----dpi=80,  fig.align="center", warning = FALSE-----------
ggplot(flights150) + 
  geom_histogram(aes(x = dep_delay, y = after_stat(density)), 
                 color="grey", bins = 30)


## -----------------------------------------------------------
dep_delays <- flights150 |> pull(dep_delay)


## -----------------------------------------------------------
median(dep_delays)


## -----------------------------------------------------------
mean(dep_delays)


## -----------------------------------------------------------
min(dep_delays)


## -----------------------------------------------------------
max(dep_delays)


## -----------------------------------------------------------
flights |>
  pull(dep_delay) |>
  max(na.rm = TRUE)


## ---- message = FALSE, warning = FALSE----------------------
library(tidyverse)
library(edsdata)


## ----message = FALSE----------------------------------------
finals


## -----------------------------------------------------------
nrow(finals)


## -----------------------------------------------------------
scores <- finals |>
  pull(grade)


## -----------------------------------------------------------
max(scores)
min(scores)


## ----dpi=80,  fig.align="center", message = FALSE-----------
ggplot(finals) + 
  geom_histogram(aes(x = grade, y = after_stat(density)), 
                 col="grey", fill = "darkcyan", bins = 10)


## -----------------------------------------------------------
quantile(scores, c(0.5), type = 1)


## -----------------------------------------------------------
median(scores)


## -----------------------------------------------------------
quantile(scores, c(0.05, 0.2, 0.9, 0.95, 0.99, 1), type = 1)


## -----------------------------------------------------------
sum(scores < 17) / length(scores)


## ---- error=TRUE--------------------------------------------
quantile(scores, c(0), type = 1)


## ---- error=TRUE--------------------------------------------
quantile(scores, c(0/4, 1/4, 2/4, 3/4, 4/4), type = 1)


## -----------------------------------------------------------
quantile(scores, type = 1)


## -----------------------------------------------------------
quantile(scores, c(0.025, 0.975), type = 1)


## -----------------------------------------------------------
middle_area <- 0.95
quantile(scores, 0.5 + (middle_area / 2) * c(-1, 1), type = 1)


## -----------------------------------------------------------
middle_area <- 0.90
quantile(scores, 0.5 + (middle_area / 2) * c(-1, 1), type = 1)


## ---- message = FALSE, warning = FALSE----------------------
library(tidyverse)
library(nycflights13)


## ----dpi=80,  fig.align="center", warning = FALSE-----------
ggplot(flights) + 
  geom_histogram(aes(x = air_time, y = after_stat(density)), 
                 col="grey", fill = "darkcyan", bins = 20)


## -----------------------------------------------------------
flights400 <- flights |>
  filter(air_time < 400) |>
  drop_na()


## ----dpi=80,  fig.align="center", warning = FALSE-----------
ggplot(flights400) + 
  geom_histogram(aes(x = air_time, y = after_stat(density)), 
                 col="grey", fill = "darkcyan", bins = 20)


## -----------------------------------------------------------
pop_mean <- flights400 |>
  pull(air_time) |>
  mean()
pop_mean


## -----------------------------------------------------------
flights_sample <- flights400 |>
  filter(month == 9 | month == 10)


## ----dpi=80,  fig.align="center", warning = FALSE-----------
ggplot(flights_sample) + 
  geom_histogram(aes(x = air_time, y = after_stat(density)), 
                 col="grey", fill = "darkcyan", bins = 20)


## -----------------------------------------------------------
sample_mean <- flights_sample |>
  pull(air_time) |>
  mean()
sample_mean


## -----------------------------------------------------------
air_times <- flights_sample |> 
  pull(air_time)


## -----------------------------------------------------------
sample_mean <- air_times |> 
  sample(replace = TRUE) |>
  mean()
sample_mean


## -----------------------------------------------------------
one_sample_mean <- function() {
  sample_mean <- flights_sample |> 
    pull(air_time) |>
    sample(replace = TRUE) |>
    mean()
  return(sample_mean)
}


## -----------------------------------------------------------
one_sample_mean()


## -----------------------------------------------------------
one_sample_value <- function(df, label, statistic) {
  sample_value <- df |> 
    pull({{ label }}) |>
    sample(replace = TRUE) |>
    statistic()
  return(sample_value)
}


## -----------------------------------------------------------
one_sample_value(flights_sample, air_time, mean)


## -----------------------------------------------------------
bstrap_means <- replicate(n = 10000, 
                    one_sample_value(flights_sample, air_time, mean))


## ----dpi=80,  fig.align="center", message = FALSE-----------
df <- tibble(bstrap_means)
ggplot(df, aes(x = bstrap_means, y = after_stat(density))) + 
  geom_histogram(col="grey", fill = "darkcyan", bins = 8)


## -----------------------------------------------------------
desired_area <- 0.95
middle95 <- quantile(bstrap_means, 
                     0.5 + (desired_area / 2) * c(-1, 1), type = 1)
middle95


## ----dpi=80,  fig.align="center", message = FALSE-----------
df <- tibble(bstrap_means)

ggplot(df, aes(x = bstrap_means, y = after_stat(density))) + 
  geom_histogram(col="grey", fill = "darkcyan", bins = 8) +
  geom_segment(aes(x = middle95[1], y = 0, 
                   xend = middle95[2], yend = 0), 
                   size = 2, color = "salmon") 


## -----------------------------------------------------------
pop_mean


## -----------------------------------------------------------
flights_sample <- flights400 |> 
  slice_sample(n = 10000, replace = FALSE)


## ----dpi=80,  fig.align="center", warning = FALSE-----------
ggplot(flights_sample) + 
  geom_histogram(aes(x = air_time, y = after_stat(density)), 
                 col="grey", fill = "darkcyan", bins = 20)


## -----------------------------------------------------------
sample_mean <- flights_sample |>
  pull(air_time) |>
  mean()
sample_mean


## -----------------------------------------------------------
bstrap_means <- replicate(n = 10000, 
                  one_sample_value(flights_sample, air_time, mean))


## -----------------------------------------------------------
desired_area <- 0.95
middle95 <- quantile(bstrap_means, 
                     0.5 + (desired_area / 2) * c(-1, 1), type = 1)
middle95


## ----dpi=80,  fig.align="center", message = FALSE-----------
df <- tibble(bstrap_means)

ggplot(df, aes(x = bstrap_means, y = after_stat(density))) + 
  geom_histogram(col="grey", fill = "darkcyan", bins = 8) +
  geom_segment(aes(x = middle95[1], y = 0, 
                   xend = middle95[2], yend = 0), 
                   size = 2, color = "salmon") +
  geom_point(aes(x = pop_mean, y = 0), color = "red", size = 3)


## -----------------------------------------------------------
all_the_bootstraps <- function() {
  desired_area <- 0.95
    
  flights_sample <- flights400 |> 
    slice_sample(n = 10000, replace = FALSE)
  
  bstrap_means <- replicate(n = 10000, 
                    one_sample_value(flights_sample, air_time, mean))
  middle95 <- quantile(bstrap_means, 
                       0.5 + (desired_area / 2) * c(-1, 1), type = 1)
  return(middle95)
}


## -----------------------------------------------------------
intervals <- replicate(n = 100, all_the_bootstraps())


## -----------------------------------------------------------
intervals[,1]


## -----------------------------------------------------------
intervals[,2]


## -----------------------------------------------------------
left_column <- intervals[1,]
right_column <- intervals[2,]


## -----------------------------------------------------------
interval_df <- tibble(
  replication = 1:100,
  left = left_column,
  right = right_column
)
interval_df


## -----------------------------------------------------------
interval_df |>
  filter(left <= pop_mean & right >= pop_mean) |>
  nrow()


## ----dpi=80,  fig.align="center", message = FALSE-----------
ggplot(interval_df) + 
  geom_segment(aes(x = left, y = replication, 
                   xend = right, yend = replication), 
               color = "salmon") +
  geom_vline(xintercept = pop_mean, color = "red") + 
  labs(x = "Air time (minutes)")


## ----message = FALSE----------------------------------------
library(tidyverse)


## -----------------------------------------------------------
one_sample_value <- function(df, label, statistic) {
  sample_value <- df |> 
    pull({{ label }}) |>
    sample(replace = TRUE) |>
    statistic()
  return(sample_value)
}


## -----------------------------------------------------------
library(NHANES)
NHANES


## -----------------------------------------------------------
# BEGIN SOLUTION
NHANES_relevant <- NHANES |>
  drop_na(c(SleepHrsNight)) |>
  filter(between(Age, 18, 60)) |> 
  mutate(healthy_sleep = between(SleepHrsNight, 7, 9)) |>
  relocate(healthy_sleep, .before = SurveyYr)
NHANES_relevant
# END SOLUTION


## -----------------------------------------------------------
NHANES_relevant


## ----dpi=80,  fig.align="center", warning = FALSE-----------
ggplot(NHANES_relevant) + 
  geom_bar(aes(x = healthy_sleep),
                 col="grey", fill = "darkcyan", bins = 20)


## -----------------------------------------------------------
NHANES_relevant |>
  summarize(prop = mean(healthy_sleep))


## -----------------------------------------------------------
# Do the bootstrap!
bstrap_means <- replicate(n = 10000, 
                one_sample_value(NHANES_relevant, healthy_sleep, mean))


## -----------------------------------------------------------
desired_area <- 0.95
middle <- quantile(bstrap_means, 
                   0.5 + (desired_area / 2) * c(-1, 1), type = 1)
middle


## ----dpi=80,  fig.align="center", message = FALSE-----------
df <- tibble(bstrap_means)
ggplot(df, aes(x = bstrap_means, y = after_stat(density))) + 
  geom_histogram(col="grey", fill = "darkcyan", bins = 13) +
  geom_segment(aes(x = middle[1], y = 0, xend = middle[2], yend = 0), 
                   size = 2, color = "salmon") +
  labs(x = "Proportion of healthy sleepers")


## -----------------------------------------------------------
desired_area <- 0.99
middle <- quantile(bstrap_means, 
                   0.5 + (desired_area / 2) * c(-1, 1), type = 1)
middle


## ----dpi=80,  fig.align="center", message = FALSE-----------
df <- tibble(bstrap_means)
ggplot(df, aes(x = bstrap_means, y = after_stat(density))) + 
  geom_histogram(col="grey", fill = "darkcyan", bins = 13) +
  geom_segment(aes(x = middle[1], y = 0, xend = middle[2], yend = 0), 
                   size = 2, color = "salmon") +
  labs(x = "Proportion of healthy sleepers")


## -----------------------------------------------------------
desired_area <- 0.80
middle <- quantile(bstrap_means, 
                   0.5 + (desired_area / 2) * c(-1, 1), type = 1)
middle


## ----dpi=80,  fig.align="center", message = FALSE-----------
df <- tibble(bstrap_means)
ggplot(df, aes(x = bstrap_means, y = after_stat(density))) + 
  geom_histogram(col="grey", fill = "darkcyan", bins = 13) +
  geom_segment(aes(x = middle[1], y = 0, xend = middle[2], yend = 0), 
                   size = 2, color = "salmon") +
  labs(x = "Proportion of healthy sleepers")


## ----message=FALSE, warning=FALSE---------------------------
lucky_numbers <- c(5, 10, 17, 25, 31, 36, 43)
lucky_numbers


## ----message=FALSE, warning=FALSE---------------------------
rock_bands <- tibble(
  band_initial = c("P", "L", "A", "Y", "U"),
  proportion = c(0.35, 0.22, 0.20, 0.12, 0.11), 
  votes = proportion * 200
)
rock_bands


## ----eval=FALSE, message=FALSE, warning=FALSE---------------
## one_resampled_statistic <- function(num_resamples) {
## 
## }
## 
## one_resampled_statistic(100) # a sample call


## ----eval=FALSE, message=FALSE, warning=FALSE---------------
## bstrap_props_tibble <- replicate(n = trials,
##                           one_resampled_statistic(num_resamples),
##                           simplify = FALSE) |>
##                        bind_rows()
## bstrap_props_tibble


## ----eval=FALSE, message=FALSE, warning=FALSE---------------
## bstrap_props_tibble |>
##   filter(vote %in% c("P", "Y", "U")) |>
##   group_by(vote) |>
##   summarize(ci = list(cf95(proportion))) |>
##   unnest_wider(ci)


## ----message=FALSE, warning=FALSE---------------------------
library(palmerpenguins)
penguins


## ----eval=FALSE, message=FALSE, warning=FALSE---------------
## one_sample_value <- function(df, label, statistic) {
##   sample_value <- df |>
##     pull({{label}}) |>
##     sample(replace = TRUE) |>
##     statistic()
##   return(sample_value)
## }


## ----eval=FALSE, message=FALSE, warning=FALSE---------------
## print(get_mean_quantile(0.9))
## print(pop_mean)


## ----eval=FALSE, message=FALSE, warning=FALSE---------------
## interval_df <- tibble(
##   replication = 1:100,
##   left = mean_intervals[1,],
##   right = mean_intervals[2,]
## )
## interval_df


## ----eval=FALSE, message=FALSE, warning=FALSE---------------
## ggplot(interval_df) +
##   geom_segment(aes(x = left, y = replication,
##                    xend = right, yend = replication),
##                color = "magenta") +
##   geom_vline(xintercept = pop_mean, color = "red")


## ----eval=FALSE, message=FALSE, warning=FALSE---------------
## resample_procedure <- function(pop_df,
##                                label,
##                                initial_sample_size,
##                                n_resamples,
##                                stat) {
## 
## }


## ----eval=FALSE, message=FALSE, warning=FALSE---------------
## get_quantile <- function(pop_df,
##                          label,
##                          initial_sample_size,
##                          n_resamples,
##                          stat,
##                          desired_area) {
## 
## }


## ----eval=FALSE, message=FALSE, warning=FALSE---------------
## conf_interval_test <- function(pop_df,
##                                label,
##                                init_samp_size,
##                                n_resamples, stat_func,
##                                desired_area, num_intervals) {
## 
## }


## ----message=FALSE, warning=FALSE---------------------------
library(edsdata)
csc1234


## ----eval=FALSE, message=FALSE, warning=FALSE---------------
## 
## resample_tibble(csc1234)  # an example call


## ----eval=FALSE, message=FALSE, warning=FALSE---------------
## 
## csc1234_one_resample()  # an example call


## ----eval=FALSE, message=FALSE, warning=FALSE---------------
## differences_tibble <- tibble(
##         `A-B` = map_dbl(resampled_differences, function(x) x[1]),
##         `C-B` = map_dbl(resampled_differences, function(x) x[2]),
##         `C-A` = map_dbl(resampled_differences, function(x) x[3])) |>
##   pivot_longer(`A-B`:`C-A`,
##                names_to = "Section Pair",
##                values_to = "Statistic")  |>
##   mutate(`Section Pair` =
##            factor(`Section Pair`, levels=c("A-B", "C-B", "C-A")))
## differences_tibble


## ----eval=FALSE---------------------------------------------
## print(observed_differences)
## differences_tibble |>
##   ggplot() +
##   geom_histogram(aes(x = Statistic, y = after_stat(density)),
##                      color = "gray", fill = "darkcyan", bins = 20) +
##   geom_segment(data = section_intervals,
##                aes(x = left, y = 0, xend = right, yend = 0),
##                    size = 2, color = "salmon") +
##   facet_wrap(~`Section Pair`)

