library(tidyverse)

ggplot()

library(tidyverse)

mpg

## glimpse(mpg)

ggplot(data = mpg)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

no_sports_cars <- filter(mpg, class != "2seater")
ggplot(data = no_sports_cars) +
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

ggplot(data = no_sports_cars) +
  geom_point(mapping = aes(x = displ, y = hwy, color = "blue"))

ggplot(data = no_sports_cars) +
  geom_point(mapping = aes(x = displ, y = hwy), 
             color = "blue")

no_sports_cars <- no_sports_cars |> 
  mutate(japanese_make = 
           manufacturer %in% c("honda", "nissan", "subaru", "toyota"))

ggplot(data = no_sports_cars) +
  geom_point(mapping = aes(x = displ, y = hwy, color = japanese_make))

ggplot(data = no_sports_cars) +
  geom_point(mapping = aes(x = displ, y = hwy, color = cyl))

ggplot(data = no_sports_cars) +
  geom_point(mapping = aes(x = displ, y = hwy, color = as_factor(cyl)))

ggplot(no_sports_cars) + 
  geom_point(aes(x = cty, y = hwy, color = as_factor(cyl)))

no_sports_cars |> 
  filter(cty < 10 | cty > 32.5) |> 
  relocate(cty, .after = year) |>
  relocate(hwy, .before = cyl)

ggplot(data = no_sports_cars) +
  geom_point(mapping = aes(x = displ, y = hwy, color = cty))

ggplot(data = no_sports_cars) +
  geom_point(mapping = aes(x = displ, y = hwy, color = cty)) +
  scale_color_gradient(low = "yellow3", high="blue")

ggplot(data = no_sports_cars) +
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ as_factor(cyl), nrow = 2)

ggplot(no_sports_cars) + 
  geom_point(aes(x = cty, y = hwy, alpha = class))

ggplot(no_sports_cars) + 
  geom_point(aes(x = cty, y = hwy, alpha = displ, shape = class))

ggplot(no_sports_cars) + 
  geom_point(aes(x = cty, y = hwy, shape = class, color = class))

ggplot(no_sports_cars) +
  geom_point(mapping = aes(x = cty, y = hwy, color = class), 
             position = "jitter")

no_sports_cars_pivot <- no_sports_cars |> 
  pivot_longer(cols = c(hwy,cty), 
               names_to = "eff_type", 
               values_to = "efficiency") 

ggplot(no_sports_cars_pivot) +
  geom_point(aes(x = displ, y = efficiency, 
                 alpha = eff_type, shape = class), 
             position = "jitter")

ggplot(no_sports_cars_pivot) +
  geom_point(aes(x = displ, y = efficiency, 
                 alpha = eff_type, shape = class))

library(tidyverse)

df_airmiles <- data.frame(
  miles = as.matrix(airmiles), date = time(airmiles)) |>
  tibble()
df_airmiles

df <- tibble(x = seq(-2.5, 2.5, 0.25), 
             f1 = 2 * x - x * x + 20, 
             f2 = 3 * x - 10, 
             f3 = -x + 50 * sin(x))
df

df_long <- df |>
  pivot_longer(c(f1, f2, f3), names_to = "type", values_to = "y") |>
  select(x, y, type)
df_long

ggplot(data = df_long) + 
  geom_line(mapping = aes(x = x, y = y, group = type))

ggplot(data = df_long) + 
  geom_line(mapping = aes(x = x, y = y, color = type))

ggplot(data = df_long) + 
  geom_line(mapping = aes(x = x, y = y, color = type), 
            size = 2, linetype = "longdash") + 
  xlab("x values") + 
  ylab("y values")

only_f3 <- df_long |>
  filter(type == "f3")

## ggplot(data = only_f3) +
##   geom_line(mapping = aes(x = x, y = y))

df_long |>
  filter(type == "f3") |>
  ggplot() + 
  geom_line(mapping = aes(x = x, y = y)) 

ggplot(data = df_airmiles) + 
  geom_line(mapping = aes(x = date, y = miles))

## ggplot(df, aes(x = x)) +
##   geom_line(aes(y = y, color = "y(x)"), size = 1, linetype = "solid") +
##   geom_line(aes(y = z, color = "z(x)"), size = 1, linetype = "dashed") +
##   geom_line(aes(y = w, color = "w(x)"), size = 2, linetype = "dotted") +
##   scale_color_manual("",
##     breaks = c("y(x)", "z(x)", "w(x)"),
##     values = c("blue", "forestgreen", "pink")) +
##   xlab("x value") +
##   ylab("y(x)|z(x)|w(x)")

ggplot(data = df_long) + 
  geom_smooth(aes(x = x, y = y, color = type), 
              se = FALSE)

ggplot(data = df_long) + 
  geom_smooth(mapping = aes(x = x, y = y, color = type), se = FALSE) + 
  geom_line(mapping = aes(x = x, y = y, color = type))

ggplot(data = df_airmiles) + 
  geom_point(mapping = aes(x = date, y = miles)) + 
  geom_line(mapping = aes(x = date, y = miles)) + 
  geom_smooth(mapping = aes(x = date, y = miles), 
              se = FALSE)

ggplot(data = df_airmiles) + 
  geom_point(mapping = aes(x = date, y = miles)) + 
  geom_line(mapping = aes(x = date, y = miles)) + 
  geom_smooth(mapping = aes(x = date, y = miles), 
              method = "lm", se = FALSE)

ggplot(data = mpg) + 
    geom_point(mapping = aes(x = displ, y = hwy, color = class)) + 
    geom_smooth(mapping = aes(x = displ, y = hwy), 
                se = FALSE)

no_sports_cars <- filter(mpg, class != "2seater")
ggplot(data = no_sports_cars) + 
    geom_point(mapping = aes(x = displ, y = hwy, color = class)) + 
    geom_smooth(mapping = aes(x = displ, y = hwy), 
                se = FALSE)

no_sports_cars <- filter(mpg, class != "2seater")
ggplot(data = no_sports_cars, 
       mapping = aes(x = displ, y = hwy)) + 
    geom_point(mapping = aes(color = class)) + 
    geom_smooth(se = FALSE)

## no_sports_cars <- filter(mpg, class != "2seater")
## ggplot(no_sports_cars,
##        aes(x = displ, y = hwy)) +
##     geom_point(aes(color = class)) +
##     geom_smooth(se = FALSE)

ggplot(no_sports_cars, 
       aes(x = displ, y = hwy, color = class)) + 
    geom_point() + 
    geom_smooth(se = FALSE, method = "lm")

different_dataset <- mpg |>
  filter(class == "midsize")

ggplot(mpg, aes(x = displ, y = hwy)) + 
    geom_point(aes(color = class)) + 
    geom_smooth(data = different_dataset, se = FALSE, method = "lm")

library(tidyverse)
library(faraway)
happy <- tibble(happy)

happy

diamonds

diamonds |>
  group_by(cut) |>
  summarize(count = n())

ggplot(diamonds, aes(x = cut)) + 
  geom_bar()

store_pies <- tribble(
  ~pie,             ~sold,
  "Pecan",            906,
  "Key Lime",         620,
  "Pumpkin",          202,
  "Apple",            408,
  "Mississippi mud",  551
)
ggplot(data = store_pies) +
  geom_bar(mapping = aes(x = pie, y = sold), 
           stat = "identity")

happy_students <- group_by(happy, happiness = happy) |> 
  summarize(number = n())
happy_students

ggplot(happy_students) + 
  geom_bar(aes(x = happiness, y = number), 
           stat = "identity") +
  labs(x = "Happy score", 
       y = "Count")

ggplot(happy_students) + 
  geom_bar(aes(x = as_factor(happiness), y = number), 
           stat = "identity") +
  labs(x = "Happy score", 
       y = "Count")

ggplot(happy_students) + 
  geom_bar(aes(x = reorder(happiness, desc(number)), y = number), 
           stat = "identity") +
  labs(x = "Happy score", 
       y = "Count")

ggplot(happy_students) + 
  geom_bar(aes(x = as_factor(happiness), y = number), 
           stat = "identity") +
  labs(x = "Happy score", 
       y = "Count")

ggplot(happy) +
  geom_bar(aes(x = happy, color = as_factor(happy)))
ggplot(happy) +
  geom_bar(aes(x = happy, fill = as_factor(happy)))

## ggplot(happy) +
##   geom_bar(aes(x = happy, color = as_factor(happy))) +
##   labs(color="happy")
## ggplot(happy) +
##   geom_bar(aes(x = happy, fill = as_factor(happy))) +
##   labs(fill="happy")

ggplot(happy) +
  geom_bar(aes(x = happy, fill = as_factor(love))) +
  labs(fill = "love")

ggplot(happy) +
  geom_bar(aes(x = happy, fill = as_factor(love)), 
           position = "fill")  +
  labs(fill = "love")

ggplot(happy) +
  geom_bar(aes(x = happy, fill = as_factor(love)), 
           position = "dodge")+
  labs(fill = "love")

ggplot(happy) +
  geom_bar(aes(x = as_factor(happy), fill = as_factor(love)), 
           position = "dodge") +
  labs(x = "Happy Score", 
       fill = "love",
       title = "relation between happiness and love") 

ggplot(happy) +
  geom_bar(aes(x = happy, fill = as_factor(love))) + 
  coord_flip()

ggplot(happy) +
  geom_bar(aes(x = happy, fill = as_factor(love))) + 
  coord_polar()

ggplot(happy) +
  geom_bar(aes(x = happy, fill = as_factor(love))) + 
  coord_flip() +
  coord_cartesian(ylim=c(1,10))

library(lterdatasampler)

and_salamanders <- and_vertebrates |>
  filter(species == "Coastal giant salamander")
and_salamanders

ggplot(and_salamanders) +
  geom_point(aes(x = length_1_mm, y = weight_g), alpha = 0.4) 

ggplot(and_salamanders) +
  geom_point(aes(x = length_1_mm, y = weight_g), alpha = 0.4) +
  scale_x_log10() + 
  scale_y_log10()

library(tidyverse)
library(patchwork)

mpg_sub <- select(mpg, manufacturer, model, hwy) 
mpg_sub

ggplot(mpg_sub, aes(x = hwy)) +
  geom_histogram(fill = "darkcyan", color = "gray")

library(tidyverse)
library(faraway)
library(grid)
library(edsdata)
library(gapminder)
library(gridExtra)
library(palmerpenguins)

g1<- penguins |> 
  filter(species == "Adelie") |> 
  ggplot() + 
  geom_histogram(aes(x = flipper_length_mm), 
                 bins = 10, color = "gray", fill="darkcyan") + 
  labs(title = "flipper length (mm)",
       subtitle = "palmerpenguins package") +
  theme(plot.title = element_text(size = 11, colour = 'black'),
        plot.subtitle = element_text(size = 8, colour = 'black'))

g2 <- gapminder |> 
  filter(gdpPercap < 60000) |> 
  ggplot() + 
  geom_histogram(aes(x = gdpPercap), color = "gray",
                 fill="darkcyan", bins = 25) + 
  labs(title = "GDP per capita",
       subtitle = "Gapminder data") +
  theme(plot.title = element_text(size = 11, colour = 'black'),
        plot.subtitle = element_text(size = 8, colour = 'black'))

g3 <- longjump |>
  filter(year == 2012) |>
  ggplot() + 
  geom_histogram(aes(x = distance), bins = 12, 
                 fill = "darkcyan", color = "gray") + 
  labs(title = "Men's Long Jump Results", subtitle = "London Olympics 2012 qualifier") +
  theme(plot.title = element_text(size = 11, colour = 'black'),
        plot.subtitle = element_text(size = 8, colour = 'black'))

g1 + g2 + g3

knitr::include_graphics('images/bins-1.png')

knitr::include_graphics('images/bins-2.png')

bins <- seq(10,50,1)
ggplot(mpg, aes(x = hwy)) +
  geom_histogram(fill = "darkcyan", color = "gray", breaks = bins)

bins <- seq(10,50,5)
ggplot(mpg, aes(x = hwy)) +
  geom_histogram(fill = "darkcyan", color = "gray", breaks = bins)

bins <- seq(10,50,1)
binned <- mpg_sub |>
  mutate(bin = cut(hwy, breaks = bins)) |>
  count(bin, .drop = FALSE)
binned

bins <- seq(10,50,1)
binned <- mpg_sub |>
  mutate(bin = cut(hwy, breaks = bins)) |>
  count(bin, .drop = TRUE)
binned

bins2 <- seq(10,50,5)
binned2 <- mpg_sub |>
  mutate(bin = cut(hwy, breaks = bins2)) |>
  count(bin, .drop = TRUE)
binned2

bin_width <- 5

bins <- seq(10, 50, bin_width)

binned <- mpg_sub |>
  mutate(bin = cut(hwy, breaks = bins)) |>
  count(bin, .drop = TRUE)
binned

binned <- binned |>
  mutate(proportion = n/nrow(mpg_sub)) 
binned

binned <- binned |>
  mutate(density = proportion/bin_width) 
binned

ggplot(binned, aes(x = bin, y = density)) +
  geom_histogram(fill = "darkcyan", color = "gray", 
                 stat = "identity")

ggplot(mpg, aes(x = hwy)) +
  geom_histogram(aes(y = after_stat(density)), 
                 fill = "darkcyan", color = "gray", breaks = bins)

uneven_bins <- c(10, 15, 30, 45)
ggplot(mpg, aes(x = hwy)) +
  geom_histogram(aes(y = after_stat(density)), 
                 fill = "darkcyan", color = "grey", 
                 breaks = uneven_bins, position = "identity")

mpg_sub |>
  mutate(bin = cut(hwy, breaks = uneven_bins)) |>
  count(bin, .drop = FALSE)

g1 <- ggplot(mpg, aes(x = hwy)) +
  geom_histogram(fill = "darkcyan", color = "grey", 
                 breaks = uneven_bins, position = "identity")

uneven_bins <- c(10, 15, 30, 45)

g2 <- ggplot(mpg, aes(x = hwy)) +
  geom_histogram(aes(y = after_stat(density)), 
                 fill = "darkcyan", color = "grey", 
                 breaks = uneven_bins, position = "identity") 

g1 + g2

first_subset <- mpg |>
  filter(class %in% c("minivan", "midsize", "suv"))
second_subset <- mpg |>
  filter(class %in% c("compact", "subcompact", "2seater"))

bin_choices <- c(10, 15, 20, 22, 27, 30, 45)

g1 <- ggplot(first_subset, aes(x = hwy)) +
  geom_histogram(aes(y = after_stat(density)), 
                 fill = "darkcyan", 
                 color = "grey", 
                 breaks = bin_choices) +
  labs(title = "minivan, midsize, suv")

g2 <- ggplot(second_subset, aes(x = hwy)) +
  geom_histogram(aes(y = after_stat(density)), 
                 fill = "darkcyan", 
                 color = "grey", 
                 breaks = bin_choices) +
  labs(title = "compact, subcompact, 2seater")

g1 + g2

g1 <- ggplot(first_subset, aes(x = hwy)) +
  geom_histogram(aes(y = after_stat(density)), 
                   fill = "white", color = "grey", 
                   breaks = bin_choices) +
  geom_density(adjust = 3, fill = "purple", alpha = 0.3) +
  labs(title = "minivan, midsize, suv")

g2 <- ggplot(second_subset, aes(x = hwy)) +
  geom_histogram(aes(y = after_stat(density)), 
                   fill = "white", color = "grey", 
                   breaks = bin_choices) +
  geom_density(adjust = 3, fill = "purple", alpha = 0.3) + 
  labs(title = "compact, subcompact, 2seater")

g1 + g2

bins <- seq(10,50,5)
ggplot(mpg, aes(x = hwy)) +
  geom_histogram(aes(fill = class), breaks = bins, 
                 position = "stack")

bins <- seq(10,50,5)
ggplot(mpg, aes(x = hwy)) +
  geom_histogram(aes(fill = class), breaks = bins, 
                 position = "dodge")

library(tidyverse)
library(patchwork)
library(mapview)
library(tigris)

us <- map_data("state") |>
  as_tibble() |>
  select(long, lat, group, region)
us

us_map <- ggplot(us) +
  geom_polygon(aes(x = long, y = lat, group = group), 
               fill = "white", color = "black") +
  coord_quickmap()
us_map

storms2006 <- storms |> 
  filter(year == 2006) 
storms2006

us_map + 
  geom_point(data = storms2006, 
             aes(x = long, y = lat, color = name))

states_sf <- states(cb = TRUE)

## states_sf

states_sf |> slice_head(n=2)

fl_sf <- states_sf |> 
  filter(NAME == "Florida")
fl_sf

fl_sf |>
  ggplot() + 
  geom_sf() + 
  theme_void()

fl_county_sf <- counties("FL", cb = TRUE)

## fl_county_sf

fl_county_sf |>
  slice_head(n=2)

fl_county_sf |>
  ggplot() + 
  geom_sf() +
  theme_void()

library(edsdata)
fl_election_returns <- election |>
  filter(year %in% c(2008, 2020), state_po == "FL") 
fl_election_returns

candidates_levels <- c("DONALD J TRUMP", "JOHN MCCAIN", 
                       "BARACK OBAMA", "JOSEPH R BIDEN JR")

fl_county_winner <- fl_election_returns |> 
  group_by(year, county_name) |>
  slice_max(candidatevotes) |>
  ungroup() |>
  mutate(candidate = factor(candidate, 
                            levels = candidates_levels))
fl_county_winner

with_election <- fl_county_sf |>
  mutate(NAME = str_to_upper(NAME)) |>
  left_join(fl_county_winner, by = c("NAME" = "county_name")) 

with_election |>
  filter(year == 2020) |>
  ggplot() + 
  theme_void() + 
  geom_sf(aes(fill = candidate)) 

election2008_g <- with_election |>
  filter(year == 2008) |>
  ggplot() + 
  theme_void() + 
  geom_sf(aes(fill = candidate)) 

election2020_g <- with_election |>
  filter(year == 2020) |>
  ggplot() + 
  theme_void() + 
  geom_sf(aes(fill = candidate)) 

election2008_g + election2020_g

with_election |>
  filter(year == 2020) |>
  mapview(zcol = "candidate")

political_palette <- colorRampPalette(c('red', 'blue'))

with_election |>
  filter(year == 2020) |>
  mapview(zcol = "candidate", col.regions = political_palette)

with_election |>
  filter(year == 2020, NAME %in% c("HILLSBOROUGH", "MANATEE")) |>
  mapview(zcol = "candidate", col.regions = political_palette)

library(tidyverse)
library(viridisLite)
library(viridis)
library(tidycensus)
library(patchwork)
library(mapview)
library(edsdata)

scorecard_relevant <- scorecard_fl |>
  select(INSTNM:NPT4_PRIV) 
with_net_tuition <- scorecard_relevant |>
  mutate(across(everything(), function(x) na_if(x, "NULL"))) |> 
  filter(!is.na(NPT4_PUB) | !is.na(NPT4_PRIV)) |>
  mutate(NPT4_PUB = as.double(NPT4_PUB),
         NPT4_PRIV = as.double(NPT4_PRIV))
with_clean_zip <- with_net_tuition |>
  mutate(ZIP = str_replace(ZIP, "\\-[:number:]+", "")) |>
  relocate(ZIP, .before = CITY) |>
  filter(str_starts(ZIP, "331")) 
tuition_tidy <- with_clean_zip |>
  mutate(ISPUB = !(is.na(NPT4_PUB))) |>
  unite("NPT4", NPT4_PUB:NPT4_PRIV, na.rm = TRUE) |>
  mutate(NPT4 = as.integer(NPT4))

tidycensus::census_api_key(Sys.getenv("CENSUS_API_KEY"))

## census_api_key("YOUR API KEY", install = TRUE)

datasets <- c("sf1", "sf2", "sf3", "sf4", "pl", 
              "as", "gu", "mp", "vi", "acs1", "acs3",
              "acs5", "acs1/profile", "acs3/profile", 
              "acs5/profile", "acs1/subject",
              "acs3/subject", "acs5/subject", 
              "acs1/cprofile", "acs5/cprofile")

acs1_variables <- load_variables(year = 2019, 
                                 dataset = "acs1", 
                                 cache = TRUE)
acs1_variables

acs1_variables |> 
  filter(!str_starts(label, "Estimate!!Total:"))

acs1_variables |> 
  filter(name == "B01001_003")

acs1_variables_median_income <- acs1_variables |> 
  filter(str_detect(label, "Median family income"))
acs1_variables_median_income

boys_lt5_states <- get_acs(year = 2019, 
                           geography = "state", 
                           variable = "B01001_003")
boys_lt5_states

girls_hs_ny <- get_acs(
  year = 2019,
  geography = "county",
  state = "NY",
  variable = "B01001_030")
girls_hs_ny

asians_flzip <- get_acs(
  year = 2019,
  geography = "zcta",
  state = "FL",
  variable = "B01001D_001")
asians_flzip

race_variables <- acs1_variables |>
  filter(str_detect(name, "B01001._001")) |>
  mutate(concept_short = str_extract(concept, regex("(?<=\\().{11}")))
race_variables

race_variables_names <- c("B01001A_001", "B01001B_001", "B01001C_001", 
                          "B01001D_001", "B01001E_001", "B01001I_001")

fl_races_county <- get_acs(
  year = 2019,
  geography = "county",
  state = "FL",
  variable = race_variables_names)
fl_races_county

fl_races_county_geo <- get_acs(
  year = 2019,
  geography = "county",
  state = "FL",
  variable = race_variables_names,
  geometry = TRUE)

## fl_races_county_geo

fl_races_county_geo |>
  slice_head(n=2)

options(tigris_use_cache = TRUE)

fl_races_county_geo <- get_acs(
  state = "FL",
  geography = "county",
  variables = race_variables_names,
  summary_var = "B01001_001",
  geometry = TRUE)

## fl_races_county_geo

fl_races_county_geo |>
  slice_head(n=2)

with_race_percent <- fl_races_county_geo |> 
  mutate(percent = 100 * estimate / summary_est)

with_race_percent <- with_race_percent |>
  inner_join(race_variables, by = c("variable" = "name"))

with_race_percent |>
  ggplot(aes(fill = percent)) +
  facet_wrap(~concept_short) +
  geom_sf(color = NA) +
  theme_void() + 
  scale_fill_viridis_c() + 
  labs(fill = "% of population\n(2019 Census)")

bins <- seq(0, 50000, 5000)

tuition_tidy |>
  ggplot() + 
  geom_histogram(aes(x = NPT4, color = ISPUB, y = after_stat(density)), 
                 breaks = bins, color = "gray") +
  facet_wrap(~ISPUB)

acs1_variables |> 
  filter(name == "B07011_001")

median_income_zip <- get_acs(
  year = 2019,
  geography = "zcta",
  variable = "B07011_001",
  geometry = TRUE, 
  state = "FL") |>
  mutate(ZIP = str_replace(NAME, "ZCTA5 ", ""))
median_income_zip

income_with_tuition <- median_income_zip |> 
  full_join(tuition_tidy, by = "ZIP") 

## View(income_with_tuition)

knitr::include_graphics('images/view_tuition.png')

with_disparity <- income_with_tuition |> 
  mutate(tuition_to_income = NPT4/estimate)

with_disparity |> 
  filter(str_detect(ZIP, "^33")) |>
  ggplot(aes(fill = tuition_to_income)) +
  geom_sf(color = NA) +
  theme_void() + 
  scale_fill_viridis_c(option = "inferno") +
  labs(fill = "Net tuition")

with_disparity |> 
  filter(str_detect(ZIP, "^331")) |>
  ggplot(aes(fill = tuition_to_income)) +
  geom_sf(color = NA) +
  theme_void() + 
  scale_fill_viridis_c(option = "inferno") +
  labs(fill = "Net tuition")

tuition_g <- with_disparity |> 
  filter(str_detect(ZIP, "^331")) |>
  ggplot(aes(fill = NPT4)) +
  geom_sf(color = NA) +
  theme_void() + 
  scale_fill_viridis_c(option = "inferno") +
  labs(fill = "Net tuition")

disparity_g2 <- with_disparity |> 
  filter(str_detect(ZIP, "^331")) |>
  ggplot(aes(fill = tuition_to_income)) +
  geom_sf(color = NA) +
  theme_void() + 
  scale_fill_viridis_c(option = "inferno") +
  labs(fill = "Tuition to income ratio")

tuition_g + disparity_g2

color_palette <- colorRampPalette(inferno(20))

with_disparity |> 
  filter(str_detect(ZIP, "^331")) |>
  mapview(zcol = "tuition_to_income", col.regions = color_palette,
          layer.name = 'tuition to income')

## library(tidyverse)
## library(edsdata)
## library(gapminder)
## library(lterdatasampler)
## library(palmerpenguins)

## ggplot(storms) +
##   geom_bar(aes(x=wind,fill=category))

## ggplot(storms) +
##   geom_bar(aes(x=category,
##                y=wind),
##            stat="identity")

## ggplot(storms) +
##   geom_bar(aes(x=category,
##                fill=as_factor(wind)))

## ggplot(storms) +
##   geom_histogram(aes(x = as.factor(wind),
##                      y = "Count",
##                      fill = as.factor(category)),
##                  stat='identity')

longjump

## world <- map_data("world") |>
##   mutate(name = region) |>
##   select(long, lat, group, name)
## 
## world_map <- ggplot(world) +
##   geom_polygon(aes(x = long, y = lat, group = group),
##                fill = "white", color = "grey50") +
##   coord_quickmap()
## world_map

nysalary_cleaned

## g +
##   geom_text(stat = "bin", aes(y = stat(density),
##                               label = round(stat(density),5)),
##             vjust = -0.2, size=2, breaks=c(seq(0, 10,2), 15, 20, 40))

knitr::include_graphics('images/abs_ggplot.png')

knitr::include_graphics('images/lott_moody_2020_figure3a.png')

## foreign_relevant <- foreign_relevant |>
##   drop_na(Year) |>
##   filter(if_any(everything(), function(x) x != "x")) |>
##   mutate(
##     Country = as.factor(Country),
##     across(where(is.character), as.numeric))
## usa_relevant <- usa_relevant |>
##   drop_na(Year)

## library(tidyverse)
## library(viridisLite)
## library(viridis)
## library(tidycensus)

## acs1vars <- load_variables(year = 2019, dataset = "acs1")
## acs1vars

## ggplot(data = mpg, mapping = aes(x = `displ`, y = `hwy`))

## ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + geom_point()

## ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
##   geom_point(data = mpg, mapping = aes(x = displ, y = hwy))

## ggplot(mpg2) + geom_boxplot(aes(x = as_factor(class), y = hwy))
