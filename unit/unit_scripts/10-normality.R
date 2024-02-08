library(tidyverse)

sample_scores <- c(78, 89, 98, 90, 96, 90, 84, 91, 98, 76)
sample_scores

mu <- mean(sample_scores)
diffs_from_mu_squared <- (sample_scores - mean(sample_scores)) ** 2
mu
diffs_from_mu_squared

variance <- sum(diffs_from_mu_squared) / length(sample_scores)
variance

sdev <- sqrt(variance)
sdev

tibble(sample_scores) |>
  ggplot() + 
  geom_histogram(aes(x = sample_scores, y = after_stat(density)),
                 color = "gray", fill = "darkcyan", binwidth = 3) + 
  scale_x_continuous(breaks = seq(70, 100, 4))

sample_scores2 <- c(88, 89, 93, 90, 86, 90, 84, 91, 93, 80)
sample_scores2

mu2 <- mean(sample_scores2)
diffs_from_mu_squared2 <- (sample_scores2 - mean(sample_scores2)) ** 2
variance2 <- sum(diffs_from_mu_squared2) / length(sample_scores2)
sdev2 <- sqrt(variance2)

mu2
sdev2

sum(diffs_from_mu_squared)
sum(diffs_from_mu_squared2)

tibble(sample_scores2) |>
  ggplot() + 
  geom_histogram(aes(x = sample_scores2, y = after_stat(density)),
                 color = "gray", fill = "darkcyan", binwidth = 3) + 
  scale_x_continuous(breaks = seq(70, 100, 4))

c(sdev,sd(sample_scores))
c(sdev2,sd(sample_scores2))

library(tidyverse)
library(datasauRus)

star <- datasaurus_dozen |>
  filter(dataset == "star") 
bullseye <- datasaurus_dozen |>
  filter(dataset  == "bullseye")

starwars

starwars_clean <- starwars |> 
  drop_na(height)

ggplot(starwars) + 
  geom_histogram(aes(x = height, y = after_stat(density)),
                 color = "gray", fill = "darkcyan", bins = 20)

mean_height <- starwars_clean |>
  summarize(mean(height)) |> 
  pull()
mean_height

sd_height <- starwars_clean |>
  summarize(sd(height)) |>
  pull() 
sd_height

starwars_clean |>
  arrange(height) |>
  head(1)

66 - mean_height

(66 - mean_height) / sd_height

starwars_clean |>
  arrange(desc(height)) |>
  head(1)

(264 - mean_height) / sd_height

starwars_clean |>
  mutate(su = scale(height)) |>
  select(name, height, su)

contest <- tribble(~name, ~sweet, ~coolblood, ~hodgepodge,
                        "Ashley",  9, 5, 9,
                        "Bruce",   8, 6, 4,
                        "Cathryn", 7, 5, 5,
                        "Drew",    8, 2, 1,
                        "Emily",   9, 7, 7,
                        "Frank",   6, 1, 1,
                        "Gail",    8, 4, 4,
                        "Harry",   5, 3, 3
                        ) 
contest

contest |> mutate(
  sweet_su = scale(sweet),
  hodge_su = scale(hodgepodge),
  cool_su = scale(coolblood),
  raw_sum = sweet + coolblood + hodgepodge, 
  scaled_sum = sweet_su + hodge_su + cool_su
  ) |>
  select(name, raw_sum, scaled_sum)

star_x <- star |>
  pull(x)
head(star_x)

bullseye_x <- bullseye |>
  pull(x)
head(bullseye_x)

sd(star_x)
sd(bullseye_x)

mean(star_x)
mean(bullseye_x)

datasaurus_dozen |>
  filter(dataset == "star" | dataset == "bullseye") |>
  ggplot() + 
  geom_histogram(aes(x = x, y = after_stat(density), fill = dataset),
                 color = "gray", position = "identity",
                 alpha = 0.7, bins = 10) + 
  scale_x_continuous(breaks = seq(15, 90, 5))

library(tidyverse)
library(edsdata)

ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1), color = "black") +
  scale_x_continuous(breaks = seq(-3, 3, 1)) +
  ylab("ɸ(z)") +
  xlab("z") +
  ggtitle("N(μ = 0, σ = 1) -∞ < z < ∞") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1), color = "black") +
  geom_point(aes(x = -1, y = 0.24), color = "red", size = 3) + 
  geom_point(aes(x = 1, y = 0.24), color = "red", size = 3) + 
  scale_x_continuous(breaks = seq(-3, 3, 1)) +
  ylab("ɸ(z)") +
  xlab("z") +
  ggtitle("N(μ = 0, σ = 1) -∞ < z < ∞") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1)) +
  stat_function(fun = dnorm, 
                xlim = c(-3,-1), geom = "area", fill = "salmon") +
  scale_x_continuous(breaks = seq(-3, 3, 1)) +
  ylab("ɸ(z)") +
  xlab("z") +
  ggtitle("N(μ = 0, σ = 1) -∞ < z < -1") +
  theme(plot.title = element_text(hjust = 0.5))

pnorm(-1)

ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1)) +
  stat_function(fun = dnorm, 
                xlim = c(-1,3), geom = "area", fill = "salmon") +
  scale_x_continuous(breaks = seq(-3, 3, 1)) +
  ylab("ɸ(z)") +
  xlab("z") +
  ggtitle("N(μ = 0, σ = 1) -1 < z < ∞") +
  theme(plot.title = element_text(hjust = 0.5))

1 - pnorm(-1)

ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1)) +
  stat_function(fun = dnorm, 
                xlim = c(-1,1), geom = "area", fill = "salmon") +
  stat_function(fun = dnorm, 
                xlim = c(-3,-1), geom = "area", fill = "darkcyan") +
  scale_x_continuous(breaks = seq(-3, 3, 1)) +
  ylab("ɸ(z)") +
  xlab("z") +
  ggtitle("N(μ = 0, σ = 1) -1 < z < 1") +
  theme(plot.title = element_text(hjust = 0.5))

pnorm(1) - pnorm(-1)

ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1)) +
  stat_function(fun = dnorm, 
                xlim = c(-2,2), geom = "area", fill = "salmon") +
  scale_x_continuous(breaks = seq(-3, 3, 1)) +
  ylab("ɸ(z)") +
  xlab("z") +
  ggtitle("N(μ = 0, σ = 1) -2 < z < 2") +
  theme(plot.title = element_text(hjust = 0.5))

pnorm(2) - pnorm(-2)

pnorm(3) - pnorm(-3)

my_athletes <- athletes |>
  drop_na() |>
  select(c(Height, Weight, Age))

my_athletes |>
  pivot_longer(everything()) |>
  ggplot(aes(x = value, y = after_stat(density), fill = name)) +
  geom_histogram(bins=15, color = "gray") +
  facet_wrap(~name, scales = "free") +
  guides(fill = "none")

summarized <- athletes |>
  summarize(mean(Height), sd(Height))
summarized

height_mean <- summarized[[1]]
height_sd <- summarized[[2]]

height_norm <- dnorm(athletes |> pull(Height), 
                     mean = height_mean, sd = height_sd)

ggplot(athletes, aes(x = Height, y = after_stat(density))) + 
  geom_histogram(col="grey", fill = "darkcyan", bins=14) +
  geom_line(mapping = aes(x = Height, y = height_norm), 
            color = "salmon") +
  labs(x = "Height") +
  ggtitle("Normal curve and the histogram") 

pnorm(200, mean = height_mean, sd = height_sd)

num_rows <- athletes |>
  filter(Height <= 200) |>
  nrow()
num_rows / nrow(athletes)

pnorm(181, mean = height_mean, sd = height_sd)

num_rows <- athletes |>
  filter(Height <= 181) |>
  nrow()
num_rows / nrow(athletes)

library(tidyverse)
library(gapminder)

df <- tribble(~event, ~probability,
              "as expected", 2/4,
              "double counted", 1/4,
              "forgot", 1/4
              )
ggplot(df) + 
  geom_bar(aes(x = event, y = probability), 
                 stat = "identity", fill = "darkcyan")

allotment <- sample(c(0, 1, 2), prob = c(1/3, 2/3, 1/3), 
                    replace = TRUE, size = 1) 
allotment

one_simulation <- function() {
  allotments <- sample(c(0, 1, 2), 
                       prob = c(1/4, 2/4, 1/4), 
                       replace = TRUE, size = 64)
  return(sum(allotments))
} 
one_simulation()

set.seed(1000)

num_repetitions <- 10000
net_allotments <- replicate(n = num_repetitions, one_simulation())

results <- tibble(
  repetition = 1:num_repetitions,
  net_allotments = net_allotments
)
results

ggplot(results) + 
  geom_histogram(aes(x = net_allotments, y = after_stat(density)), 
                 color = "gray", fill = "darkcyan", bins = 15)

results |>
  pull(net_allotments) |>
  sd()

curve <- dnorm(net_allotments, mean = 64, sd = 5.66)

ggplot(results) + 
  geom_histogram(aes(x = net_allotments, y = after_stat(density)), 
                 color = "gray", fill = "darkcyan", bins = 15) +
  geom_line(mapping = aes(x = net_allotments, y = curve), 
            color = "salmon") 

NHANES_relevant <- NHANES |>
  drop_na(BPSysAve)

NHANES_relevant |>
  ggplot() +
  geom_histogram(aes(x = BPSysAve, fill = Gender, 
                     y = after_stat(density)), 
                 color = "gray", alpha = 0.7,
                 position = "identity", bins=15)

summary_stats <- NHANES_relevant |>
  group_by(Gender) |>
  summarize(mean = mean(BPSysAve),
            sd = sd(BPSysAve))
summary_stats

NHANES_female <- NHANES_relevant |>
  filter(Gender == "female")
NHANES_male <- NHANES_relevant |>
  filter(Gender == "male")

one_simulation <- function(df, label, sample_size) {
  df |>
    slice_sample(n = sample_size, replace = TRUE) |>
    summarize(mean({{ label }})) |>
    pull()
} 

one_simulation(NHANES_female, BPSysAve, 100)

set.seed(1000)

num_repetitions <- 10000
sample_means <- replicate(n = num_repetitions, 
                          one_simulation(NHANES_female, BPSysAve, 100))

female_results <- tibble(
  repetition = 1:num_repetitions,
  sample_mean = sample_means, 
  gender = "female"
)
female_results

set.seed(1000)

sample_means <- replicate(n = num_repetitions, 
                          one_simulation(NHANES_male, BPSysAve, 100))

male_results <- tibble(
  repetition = 1:num_repetitions,
  sample_mean = sample_means,
  gender = "male"
)
male_results

results <- bind_rows(female_results, male_results)

ggplot(results) +
  geom_histogram(aes(x = sample_mean, y = after_stat(density)), 
                 bins = 20, color = "gray", 
                 position = "identity", alpha = 0.8) + 
  facet_wrap(~gender, scales = "free")

dnorm1 <- dnorm(female_results$sample_mean, 
                mean = mean(female_results$sample_mean), 
                sd = sd(female_results$sample_mean))
dnorm2 <- dnorm(male_results$sample_mean, 
                mean = mean(male_results$sample_mean), 
                sd = sd(male_results$sample_mean))

ggplot() +
  geom_histogram(data = results, aes(x = sample_mean, 
                                     y = after_stat(density), fill = gender), 
                 bins = 20, color = "gray", 
                 position = "identity", alpha = 0.8) + 
  geom_line(data = female_results, mapping = aes(x = sample_mean, y = dnorm1), 
            color = "salmon4") +
  geom_line(data = male_results, mapping = aes(x = sample_mean, y = dnorm2), 
            color = "darkcyan") + 
  scale_x_continuous(breaks = seq(110, 128, 2))

summary_stats

17.81539 / sqrt(100)

## library(tidyverse)
## library(edsdata)

rain_record <- tibble(had_rain = c(1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 
                                   1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 
                                   0, 0, 0, 0, 1, 0))
rain_record

## 
## rainy_proportion_after_period(5) # an example call

## rainy_experiment(30, 5)

## draw_ggplot_for_rainy_experiment <- function(sample_size) {
##   g <- rainy_experiment(30,sample_size) |>
##     ggplot(aes(x = sample_proportion)) +
##     geom_histogram(aes(y = after_stat(density)),
##                    fill = "darkcyan",
##                    color="gray",
##                    bins=50)
##   return(g)
## }
## draw_ggplot_for_rainy_experiment(10)

## nycflights_sd <- flights |>
##   summarize(sd = sd(dep_delay, na.rm = TRUE)) |>
##   pull(sd)
## nycflights_sd

## 
## theory_sd(10) # an example call

## one_sample_mean <- function(sample_size) {
##   one_sample_mean <- flights |>
##     slice_sample(n = sample_size, replace = FALSE) |>
##     summarize(mean = mean(dep_delay, na.rm = TRUE)) |>
##     pull(mean)
##   return(one_sample_mean)
## }
## 
## one_sample_mean(10) # an example call

## sd_tibble <- tibble(
##     sample_size = seq(10, 100, 5),
##     theory_sds = map_dbl(sample_size, theory_sd),
##     sample_sds = map_dbl(sample_size, sample_sd)
##     ) |>
##   pivot_longer(c(theory_sds,sample_sds),
##                names_to = "category",
##                values_to = "sd")
## 
## sd_tibble

library(edsdata)
applications

## library(nycflights13)
## flights <- flights |>
##   drop_na()

## 
## # example calls
## var_from_sample(10)
## var_from_sample(100)
## var_from_sample(1000)

## hist_for_sample <- function(sample_size) {
## 
## }

## map(c(10, 20, 50, 100, 1000, 10000), hist_for_sample)
