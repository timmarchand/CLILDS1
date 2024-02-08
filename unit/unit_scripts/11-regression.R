library(tidyverse)
library(datasauRus)
library(edsdata)

scatter_with_r <- function(r) {
  x <- rnorm(10000)
  z <- rnorm(10000)
  y = r*x + (sqrt(1-r**2))*z
  g <- tibble(x = x, y = y) %>%
    ggplot() +
    geom_point(aes(x = x, y = y), color = "darkcyan", alpha = 0.8) +
    xlab("") +
    ylab("")
  return(g)
}

set.seed(1)

corr_tibble

corr_tibble |>
  filter(dataset == "linear") |>
  ggplot() + 
  geom_point(aes(x = x, y = y), color = "darkcyan") 

corr_tibble |>
  filter(dataset == "linear") |>
  ggplot() + 
  geom_point(aes(x = x, y = y), color = "darkcyan") +
  geom_line(data = data.frame(x = c(-1,1), y = c(-1,1)),
            aes(x = x, y = y), color = "blue", size = 1)

corr_tibble |>
  filter(dataset == "perf") |>
  ggplot() + 
  geom_point(aes(x = x, y = y), color = "darkcyan") +
  geom_line(data = tibble(x = c(-1,1), y = c(-1,1)),
            aes(x = x, y = y), color = "blue", size = 1)

corr_tibble |>
  filter(dataset == "null") |>
  ggplot() + 
  geom_point(aes(x = x, y = y), color = "darkcyan") +
  geom_line(data = data.frame(x = c(-1,1), y = c(-1,1)),
            aes(x = x, y = y), color = "blue", size = 1)

g1 <- scatter_with_r(0.87) +
  labs(title = "r = 0.87")
g2 <- scatter_with_r(0.29) +
  labs(title = "r = 0.29")
g3 <- scatter_with_r(0) +
  labs(title = "r = 0")
g4 <- scatter_with_r(-0.46) +
  labs(title = "r = -0.46")
library(patchwork)
g1 | g2 | g3 | g4

corr_tibble |> 
  filter(dataset == 'linear') |>
  transmute(x = x, 
            y = y,
            x_su = scale(x),
            y_su = scale(y)) 

linear_df_standard <- corr_tibble |> 
  filter(dataset == 'linear') |>
  transmute(x = x, 
            y = y,
            x_su = scale(x),
            y_su = scale(y),
            prod = x_su * y_su) 
linear_df_standard

r <- linear_df_standard |>
  pull(prod) |>
  mean()
r

linear_df_standard |>
  summarize(r = cor(x, y))

swapped <- corr_tibble |>
  filter(dataset == 'linear') |>
  transmute(x_new = y, y_new = x)

ggplot(swapped) + 
  geom_point(aes(x = x_new, y = y_new), color = "darkcyan") +
  geom_line(data = tibble(x = c(-1,1), y = c(-1,1)),
            aes(x = x, y = y), color = "salmon", size = 1)

swapped |>  
  summarize(r = cor(x_new, y_new))

weird <- tibble(
  x = c(1, 2, 3, 4, 5),
  y = c(0, 4, 3, 2, 1)
)

weird |>
  ggplot() + 
  geom_point(aes(x = x, y = y), 
             color = "darkcyan", size = 6)

weird %>%
  summarize(r = cor(x, y))

curvy <- corr_tibble |> 
  filter(dataset == "curvy")

curvy %>%
  summarize(cor(x, y))

ggplot(curvy) + 
  geom_point(aes(x = x, y = y), color = "darkcyan")

datasaurus_dozen |>
  filter(dataset == "bullseye" | dataset == "star") |>
  group_by(dataset) |>
  summarize(r = cor(x, y))

datasaurus_dozen |>
  filter(dataset == "bullseye" | dataset == "star") |>
  ggplot() +
  geom_point(aes(x = x, y = y, color = dataset)) +
  facet_wrap( ~ dataset, ncol = 2)

library(tidyverse)

trees_data <- tibble(datasets::trees)

slice_head(trees_data, n=5)

ggplot(trees_data) + 
  geom_point(aes(x = Height, y = Girth), color = "darkcyan")

trees_data |> 
  filter(between(Height, 75 - 1, 75 + 1))

trees_data |> 
  mutate(close_to_75 = between(Height, 75 - 1, 75 + 1)) |>
  ggplot() +
    geom_point(aes(x = Height, y = Girth, color = close_to_75))

trees_data |> 
  filter(between(Height, 75 - 1, 75 + 1)) |>
  summarize(prediction = mean(Girth))

nn_predict <- function(x, tib, x_label, y_label, 
                       threshold) {
  tib |>
    filter(between({{ x_label }}, 
                   x - threshold, 
                   x + threshold)) |>
    summarize(avg = mean({{ y_label }})) |>
    pull(avg)
}

trees_data |>
  summarize(r = cor(Girth, Height))

girth_height_su <- trees_data |>
  transmute(Girth_su = scale(Girth),
         Height_su = scale(Height))
girth_height_su

ggplot(girth_height_su) + 
  geom_point(aes(x = Height_su, y = Girth_su), color = "darkcyan")

ggplot(girth_height_su) + 
  geom_point(aes(x = Height_su, y = Girth_su), color = "darkcyan") + 
  geom_abline(aes(slope = 1, intercept = 0), color = "blue", size = 1) +
  geom_segment(aes(x = Height_su, y = Girth_su,
                   xend = Height_su, yend = 1 * Height_su + 0), color = "blue", alpha = 0)

with_nn_predictions <- girth_height_su |>
  mutate(prediction = 
           map_dbl(Height_su, 
                   nn_predict, girth_height_su, Height_su, Girth_su, 1))
with_nn_predictions

ggplot(with_nn_predictions) + 
  geom_point(aes(x = Height_su, y = Girth_su), color = "darkcyan") + 
  geom_abline(aes(slope = 1, intercept = 0), color = "blue", size = 1) +
  geom_segment(aes(x = Height_su, y = Girth_su,
                   xend = Height_su, yend = 1 * Height_su + 0), color = "blue", 
               alpha = 0) +
  geom_point(aes(x = Height_su, y = prediction), color = "salmon")

ggplot(with_nn_predictions) + 
  geom_point(aes(x = Height_su, y = Girth_su), color = "darkcyan") + 
  geom_abline(aes(slope = 1, intercept = 0), color = "blue", size = 1) +
  geom_segment(aes(x = Height_su, y = Girth_su,
                   xend = Height_su, yend = 1 * Height_su + 0), color = "blue", alpha = 0) +
  geom_abline(slope = 0.5192801, intercept = 0, color = "purple", size = 1) + 
  geom_point(aes(x = Height_su, y = prediction), color = "salmon")

plot_errors <- function(df, y, x, slope, intercept) {
  ggplot(df) + 
  geom_point(aes(x = {{ x }}, y = {{ y }}), color = "darkcyan", alpha = 0.5) + 
  geom_abline(aes(slope = slope, intercept = intercept), color = "purple", size = 1)  +
  geom_segment(aes(x = {{ x }}, y = {{ y }},
                   xend = {{ x }}, yend = slope * {{ x }} + intercept), color = "blue", alpha = 0.4)
}
plot_errors(girth_height_su, Girth_su, Height_su, 0.5192801, 0) +
  labs(title = "Residuals for line y = r * x")

library(patchwork)
g1 <- plot_errors(girth_height_su, Girth_su, Height_su, 1, 0) +
  labs(title = "Residuals for the line y = x")
g2 <- plot_errors(girth_height_su, Girth_su, Height_su, 0, 0) +
  labs(title = "Residuals for the line y = 0")
g1 + g2

line_rss <- function(params) {
  slope <- params[1]
  intercept <- params[2]
  
  x <- pull(girth_height_su, Height_su)
  y <- pull(girth_height_su, Girth_su)
  fitted <- slope * x + intercept
  return(sum((y - fitted) ** 2))
}

params <- c(0.5192, 0)
line_rss(params)

params <- c(1, 0)
line_rss(params)

params <- c(0, 0)
line_rss(params)

my_quadratic_equation <- function(x) {
  (x - 2) ** 2 + 3
}

toy_df <- tibble(
                 x = seq(1, 3, 0.1),
                 quadratic = (x - 2) ** 2 + 3) %>%
  pivot_longer(c(quadratic), 
               names_to = "dataset", values_to = "y") %>%
  select(x, y, dataset) %>%
  arrange(dataset)

toy_df %>% 
  filter(dataset == "quadratic") %>%
  ggplot() + 
  geom_line(aes(x = x, y = y),
            color = "darkcyan",
            size = 2) + 
  geom_point(aes(x = 1, y = my_quadratic_equation(1)),
             color = "salmon",
             size = 4) 

initial_guess <- c(1)  # start with a guess at x = 1
best <- optim(initial_guess, my_quadratic_equation)

best |>
  pluck("par")

initial_guess <- c(1,0)  # start with a guess, the y = x line 
best <- optim(initial_guess, line_rss)

best |> 
  pluck("par")

ggplot(girth_height_su) + 
  geom_point(aes(x = Height_su, y = Girth_su), color = "darkcyan") + 
  geom_abline(aes(slope = best$par[1], intercept = best$par[2]), 
              color = "purple", size = 1) +
  geom_abline(aes(slope = 1, intercept = 0), color = "gray", alpha = 1) +
  geom_abline(aes(slope = 0, intercept = 0), color = "gray", alpha = 1) +
  geom_segment(aes(x = Height_su, y = Girth_su,
                   xend = Height_su, yend = 1 * Height_su + 0), 
               color = "blue", alpha = 0) 

ggplot(girth_height_su) + 
  geom_point(aes(x = Height_su, y = Girth_su), color = "darkcyan") + 
  geom_abline(aes(slope = best$par[1], intercept = best$par[2]), color = "purple", size = 1) +
  geom_abline(aes(slope = 1, intercept = 0), color = "gray", alpha = 1) +
  geom_abline(aes(slope = 0, intercept = 0), color = "gray", alpha = 1) +
  geom_vline(xintercept = -1, color = "blue") + 
  geom_vline(xintercept = 1, color = "blue") + 
  geom_point(aes(x = -1, y = 0), size = 2, color = "blue") + 
  geom_point(aes(x = -1, y = -0.5), size = 2, color = "blue") + 
  geom_point(aes(x = -1, y = -1), size = 2, color = "blue") + 
  geom_point(aes(x = 1, y = 0), size = 2, color = "blue") + 
  geom_point(aes(x = 1, y = 0.5), size = 2, color = "blue") + 
  geom_point(aes(x = 1, y = 1), size = 2, color = "blue") + 
  geom_segment(aes(x = Height_su, y = Girth_su,
                   xend = Height_su, yend = 1 * Height_su + 0), color = "blue", alpha = 0)

lmod <- lm(Girth_su ~ Height_su, data = girth_height_su)

lmod

lmod_original_units <- lm(Girth ~ Height, data = trees_data)
lmod_original_units

my_pred <- predict(lmod_original_units, 
                   newdata = tibble(Height = c(75)))
my_pred

## summary(lmod)

library(tidyverse)
library(tidymodels)
library(palmerpenguins)

my_penguins <- penguins |>
  drop_na()
my_penguins

ggplot(my_penguins) + 
  geom_point(aes(x = bill_length_mm, y = bill_depth_mm), 
             color = "darkcyan")

my_penguins |>
  summarize(r = cor(bill_length_mm, 
                    bill_depth_mm))

g1 <- ggplot(penguins,
       aes(x = bill_length_mm)) + 
  geom_histogram(aes(y = ..density..), 
                   fill = "darkcyan", 
                   bins = 40,
                   color = "gray") + 
  geom_density(color = "salmon", size=1)

g2 <- ggplot(penguins,
       aes(x = bill_depth_mm)) + 
    geom_histogram(aes(y = ..density..), 
                   fill = "darkcyan", 
                   bins = 40,
                   color = "gray") +
  geom_density(color = "salmon", size=1)

g1 + g2 

linear_reg()

linear_reg() |>
  set_engine("lm") # ordinary least squares 

lmod_parsnip <- linear_reg() |>
  set_engine("lm") |>
  fit(bill_depth_mm ~ bill_length_mm, data = penguins)
lmod_parsnip

linear_reg() %>%
  set_engine("lm") %>%
  fit(bill_depth_mm ~ bill_length_mm, data = penguins) %>%
  tidy() # broom

lmod_augmented <- lmod_parsnip |>
  pluck("fit") |>
  augment()
lmod_augmented

ggplot(lmod_augmented) + 
  geom_point(aes(x = bill_length_mm, 
                 y = bill_depth_mm), 
             color = "darkcyan") + 
  geom_line(aes(x = bill_length_mm, 
                y = .fitted), 
            color = "purple", 
            size = 2)

## ggplot(my_penguins,
##        aes(x = bill_length_mm, y = bill_depth_mm)) +
##   geom_point(color = "darkcyan") +
##   geom_smooth(method = "lm", color = "purple", se = FALSE)

ggplot(my_penguins) + 
  geom_point(aes(x = bill_length_mm, 
                 y = bill_depth_mm,
                 color = species))

lmod_parsnip_factor <- linear_reg() |>
  set_engine("lm") %>%
  fit(bill_depth_mm ~ bill_length_mm + species,
      data = penguins) 

lmod_parsnip_factor |>
  tidy()

lmod_aug_factor <- lmod_parsnip_factor |>
  pluck("fit") |>
  augment()
lmod_aug_factor

ggplot(lmod_aug_factor,
       aes(x = bill_length_mm)) + 
  geom_point(aes(y = bill_depth_mm,
                 color = species)) + 
  geom_line(aes(y = .fitted, 
                group = species),
            color = "purple",
            size = 1)

g1 <- ggplot(lmod_augmented) + 
  geom_point(aes(x = bill_length_mm, 
                 y = bill_depth_mm), 
             color = "darkcyan") + 
  geom_line(aes(x = bill_length_mm, 
                y = .fitted), 
            color = "purple", 
            size = 2)

g2 <- ggplot(lmod_aug_factor,
       aes(x = bill_length_mm)) + 
  geom_point(aes(y = bill_depth_mm,
                 color = species)) + 
  geom_line(aes(y = .fitted, 
                group = species),
            color = "purple",
            size = 1)

g1 + g2

library(tidyverse)
library(tidymodels)
library(palmerpenguins)
library(edsdata)

my_athletes <- athletes |>
  filter(Year > 2008, Season == "Summer") |>
  distinct(ID, .keep_all = TRUE)
my_athletes

ggplot(my_athletes) + 
  geom_point(aes(x = Height, y = Weight), 
             color = "darkcyan")

my_athletes |>
  summarize(r = cor(Height, Weight))

draw_true_reg_lines <- function(sample_size, 
                            slope, intercept) {
  tib <- tibble(
    x = rnorm(sample_size, 10, 5), 
    y = slope * x + intercept, 
    # error ~ normal w. mean 0
    y_noisy = y + rnorm(sample_size, 0, 6) 
  )
  
  tib %>%
    ggplot() + 
    geom_line(aes(x = x, y = y), 
              lty = "dotted", 
              size = 1) +
    geom_point(aes(x = x, 
                   y = y_noisy),
               color = "darkcyan") +
    geom_smooth(aes(x = x, 
                    y = y_noisy), 
                method = "lm", 
                se = FALSE, 
                color = "purple", 
                size = 1) +
    labs(title = paste("sample size:", sample_size))
}

set.seed(14)
g1 <- draw_true_reg_lines(10, 4, 25)
g2 <- draw_true_reg_lines(50, 4, 25)
g1 + g2

lmod_parsnip <- linear_reg() |>
  set_engine("lm") |>
  fit(Weight ~ Height, data = my_athletes)

lmod_parsnip |>
  tidy()

test_data_tib <- tibble(
  Height = c(190))

lmod_parsnip |>
  predict(test_data_tib)

predict190_from_scatter <- function(tib) {
  lmod_parsnip <- linear_reg() |>
    set_engine("lm") |>
    fit(Weight ~ Height, data = tib)
  
  lmod_parsnip |>
    predict(tibble(Height = c(190))) |>
    pull()
}

my_athletes |>
  specify(Weight ~ Height) # infer package

resampled_scatter_plots <- my_athletes |>
  specify(Weight ~ Height) |>
  generate(reps = 1000, type = "bootstrap") 
resampled_scatter_plots

resampled_scatter_plots |>
  nest() 

bstrap_predictions <- resampled_scatter_plots |>
  nest() |> 
  mutate(prediction = map_dbl(data,
                        predict190_from_scatter))
bstrap_predictions

middle <- bstrap_predictions |> 
  get_confidence_interval(level = 0.95)
middle

ggplot(bstrap_predictions, 
       aes(x = prediction, y = after_stat(density))) + 
  geom_histogram(col="grey", fill = "darkcyan", bins = 13) +
  geom_segment(aes(x = middle[1][[1]], y = 0, 
                   xend = middle[2][[1]], yend = 0), 
                   size = 2, color = "salmon") 

lmod_parsnip |>
  predict(test_data_tib,
          type = "conf_int", 
          level = 0.95)

slope_from_scatter <- function(tib) {
  lmod_parsnip <- linear_reg() |>
    set_engine("lm") |>
    fit(Weight ~ Height, data = tib)
  
  lmod_parsnip |>
    tidy() |>
    pull(estimate) |>
    last() # retrieve slope estimate as a vector
}

bstrap_slopes <- my_athletes |>
  specify(Weight ~ Height) |>
  generate(reps = 1000, type = "bootstrap") |>
  nest() |>
  mutate(prediction = map_dbl(data, slope_from_scatter))
bstrap_slopes

middle <- bstrap_slopes |>
  get_confidence_interval(level = 0.95)
middle

ggplot(bstrap_slopes, 
       aes(x = prediction, y = after_stat(density))) + 
  geom_histogram(col="grey", fill = "darkcyan", bins = 13) +
  geom_segment(aes(x = middle[1][[1]], y = 0, 
                   xend = middle[2][[1]], yend = 0), 
                   size = 2, color = "salmon") 

lmod_parsnip |>
  pluck("fit") |>
  confint()

library(tidyverse)
library(edsdata)

diagnostic_examples <- corr_tibble |>
  filter(dataset %in% c("linear", "cone", "quadratic"))
diagnostic_examples

ggplot(diagnostic_examples) + 
  geom_point(aes(x = x, y = y, color = dataset)) +
  facet_wrap( ~ dataset, ncol = 3)

diagnostic_examples |>
  group_by(dataset) |>
  summarize(r = cor(x, y))

augmented_from_dataset <- function(dataset_name) {
  dataset <- diagnostic_examples |>
    filter(dataset == dataset_name)
  
  lmod_parsnip <- linear_reg() |>
    set_engine("lm") |>
    fit(y ~ x, data = dataset)
  
  lmod_parsnip |>
    pluck("fit") |>
    augment()
}

augmented_cone <- augmented_from_dataset("cone")
augmented_linear <- augmented_from_dataset("linear")
augmented_quadratic <- augmented_from_dataset("quadratic")

ggplot(augmented_linear) + 
  geom_point(aes(x = .fitted, y = .resid), color = "red") + 
  geom_hline(yintercept = 0, color = "gray",
             lty = "dashed", size = 1)

ggplot(augmented_cone) + 
  geom_point(aes(x = .fitted, y = .resid), color = "red") +
  geom_hline(yintercept = 0, color = "gray",
             lty = "dashed", size = 1)

ggplot(augmented_quadratic) + 
  geom_point(aes(x = .fitted, y = .resid), color = "red") +
  geom_hline(yintercept = 0, color = "gray",
             lty = "dashed", size = 1)

quadratic_dataset <- corr_tibble |>
  filter(dataset == "quadratic")

lmod_parsnip <- linear_reg() |>
    set_engine("lm") |>
    fit(y ~ x + I(x^2), data = quadratic_dataset)
  
augmented_quad_revised <- lmod_parsnip |>
  pluck("fit") |>
  augment()

ggplot(augmented_quad_revised) + 
  geom_point(aes(x = .fitted, y = .resid), color = "red") + 
  geom_hline(yintercept = 0, color = "gray",
             lty = "dashed", size = 1)

## library(tidyverse)
## library(edsdata)
## library(gapminder)

## library(tidymodels)

library(edsdata)
candy

## g_candy_plot +
##   geom_smooth(aes(x = sugarpercent_su, y = winpercent_su),
##               method = "lm", se = FALSE)

## ggplot(candy,
##        aes(x = sugarpercent, y = winpercent)) +
##   geom_point(color = "darkcyan") +
##   geom_abline(aes(slope = candy_slope,
##                   intercept = candy_intercept),
##               size = 1, color = "salmon") +
##   geom_point(aes(x = 0.3,
##                  y = candy_pred30),
##              size = 3, color = "purple") +
##   geom_point(aes(x = 0.7,
##                  y = candy_pred70),
##              size = 3, color = "purple")

## ggplot(candy_residuals,
##        aes(x = sugarpercent, y = residual)) +
##   geom_point(color = "darkred")

## lmod_augmented |>
##   ggplot(aes(x = sugarpercent, y = winpercent, color = chocolate)) +
##   geom_point() +
##   geom_line(aes(y = .fitted))

## 
## print(paste("simple linear regression RSS :",
##             lmod_simple_rss))
## print(paste("linear regression with factor RSS :",
##             lmod_with_factor_rss))

## slope_from_scatter <- function(tib) {
##   lmod_parsnip <- linear_reg() |>
##     set_engine("lm") |>
##     fit(winpercent ~ sugarpercent, data = tib)
## 
##   lmod_parsnip |>
##     tidy() |>
##     pull(estimate) |>
##     last() # retrieve slope estimate as a vector
## }

library(lterdatasampler)

and_vertebrates

library(edsdata)
mango_exams

## library(gapminder)
## a <- filter(gapminder, continent == "Africa")
## ggplot(a) +
##    geom_point(aes(x = log(gdpPercap), y = log(lifeExp)))
## ggplot(a) +
##    geom_point(aes(x = log(gdpPercap), y = log(lifeExp)))
## ggplot(a) +
##   geom_histogram(aes(x = log(gdpPercap), y = after_stat(density)))
## ggplot(a) +
##   geom_histogram(aes(x = log(lifeExp), y = after_stat(density)))
## 
## lmod <- lm(log(lifeExp) ~ log(gdpPercap), data = a)
## summary(lmod)
## ggplot(lmod, aes(x = .fitted, y = .resid)) +
##   geom_point()

## library(tidyverse)
## library("reshape2")

## glimpse(mtcars)

## #melt your data
## df_melt <- melt(mtcars,"mpg")
## 
## ggplot(df_melt,aes(value,mpg)) +
##   geom_point() +
##   facet_wrap(.~variable, ncol = 2, scales = "free")

## lmod <- lm(mpg ~ disp + I(disp^2), data = mtcars)
## summary(lmod)

## ggplot(mtcars) +
##   geom_point(aes(x = disp, y = resid(lmod)))

## lmod <- lm(mpg ~ disp + I(disp^2) + cyl + hp + drat + wt + qsec, data = mtcars)
## summary(lmod)

## lmod <- lm(mpg ~ wt + hp, data = mtcars)
## summary(lmod)
