library(tidyverse)

## mtcars

mtcars |>
  slice_head(n=3)

## ?mtcars

bakery1 <- tribble(~week, ~gingerbread, ~`chocolate peppermint`, ~`macadamia nut`,
                  1, 10, 23, 12,
                  2, 16, 21, 16,
                  3, 25, 20, 24,
                  4, 12, 18, 20,
                 )

bakery1

bakery2 <- as_tibble(t(bakery1), rownames = "week") |>
  slice(-1)
colnames(bakery2) <- c("week", 1,2,3,4)

bakery2

bakery_tidy <- bakery1 |>
  pivot_longer(gingerbread:`macadamia nut`, 
               names_to = "type", values_to = "sales")
bakery_tidy

forecast_sales <- tibble(
  week          = c(1, 2, 3),
  forecast = c("200-300", "300-400", "200-500")
) |> 
  print()

library(tidyverse)

## mtcars

mtcars |>
  slice_head(n=3)

nrow(mtcars) # how many rows in the dataset?
ncol(mtcars) # how many columns? 
colnames(mtcars) # what are the names of the columns? 

mtcars_tibble <- tibble(mtcars)
mtcars_tibble

mtcars_tibble <- tibble(rownames_to_column(mtcars, var = "model_name"))
mtcars_tibble

mtcars_tibble$cyl

pull(mtcars_tibble, cyl)

select(mtcars_tibble, 3)

slice(mtcars_tibble, 2)

unique(pull(mtcars_tibble, cyl))

max(pull(mtcars_tibble, mpg))
min(pull(mtcars_tibble, mpg))
sort(pull(mtcars_tibble, mpg))

students <- c("Gail", "Henry", "Irwin", "Joan")
chemistry <- c( 99, 98, 80, 92 )
spanish <- c(87, 85, 90, 88)

class <- tibble(students = students, 
                chemistry_grades = chemistry, 
                spanish_grades = spanish)
class

pull(class, chemistry_grades) # all grades in chemistry
min(pull(class, chemistry_grades))  # minimum chemistry score

class <- tribble(~student,~chemistry_grades,~spanish_grades,
        "Gail",   99, 87,
        "Henry",  98, 85,
        "Irwin",  80, 90,
        "Joan",   92, 88)
class

tibble(x=1:5, 
       y=x*x, 
       z = 1.5*x - 0.2)

tibble(x = seq(1,4,0.5), 
       y = sin(x), 
       z = cos(x), 
       w = x^3 - 10*x^2 + x - 2)

## path <- str_c("https://data.bloomington.in.gov/",
##               "dataset/117733fb-31cb-480a-8b30-fbf425a690cd/",
##               "resource/2b2a4280-964c-4845-b397-3105e227a1ae/",
##               "download/pedestrian-and-bicyclist-counts.csv")
## bloom <- read_csv(path)

bloom <- read_csv("data/sneaky_bloom.csv")

slice_head(bloom, n = 3)

## pull(bloom, `N College and RR`)

## write_csv(bloom, "bloom.csv")

library(tidyverse)

mtcars_tibble <- tibble(rownames_to_column(mtcars, "model_name"))
mtcars_tibble

knitr::include_graphics('images/dplyr_matrix.png')

select(mtcars_tibble, model_name, mpg, cyl, vs)

select(mtcars_tibble, model_name, mpg:wt)

select(mtcars_tibble, cyl | !starts_with("m") & contains("a"))

select(mtcars_tibble, matches("^[a-z]{3,5}$"))

filter(mtcars_tibble, cyl == 8)

filter(mtcars_tibble, cyl == 8, am == 1, hp > 300)

filter(mtcars_tibble, model_name == "Datsun 710")

arrange(mtcars_tibble, hp)

arrange(mtcars_tibble, desc(hp), mpg)

slice(mtcars_tibble, (n()-10) : (n()-2))

slice_head(mtcars_tibble, n = 2)
slice_tail(mtcars_tibble, n = 2)

slice(mtcars_tibble, 3)

slice_sample(mtcars_tibble)

rename(mtcars_tibble, weight = wt, cylinder = cyl)

relocate(mtcars_tibble, am, .before = mpg)

mtcars_with_ratio <- mutate(mtcars_tibble, 
                            cyl_gear_ratio = cyl / gear)
mtcars_with_ratio

mtcars_with_ratio <- mutate(mtcars_tibble, 
                            cyl_gear_ratio = cyl / gear,
                            .before = mpg)
mtcars_with_ratio

mtcars_mutated <- mutate(mtcars_tibble, 
                         cyl_gear_ratio = cyl / gear,
                         make = str_replace(model_name, " .*", ""),
                         .before = mpg)
mtcars_mutated

mtcars_tibble

unique(pull(mtcars_mutated, make))

mutate(mtcars_tibble, wt = round(wt))

mutate(mtcars_tibble, 
       across(c(mpg, wt, qsec), round))

only_the_new_stuff <- transmute(mtcars_tibble, 
                          cyl_gear_ratio = cyl / gear,
                          make = str_replace(model_name, " .*", ""))
only_the_new_stuff

grouped_by_cl <- group_by(mtcars_tibble, cyl)
slice_head(grouped_by_cl, n=2) # show 2 rows per group

grouped_by_cl <- group_by(mtcars_tibble, cyl)
summarized <- summarize(grouped_by_cl, 
          count = n(),
          avg_mpg = mean(mpg))
summarized

# step 1
grouped_by_cl <- group_by(mtcars_tibble, cyl, am)
# step 2
summarized <- summarize(grouped_by_cl, 
          count = n(),
          avg_mpg = mean(mpg))
# step 3
avg_mpg_counts <- filter(summarized, count > 2)
avg_mpg_counts

avg_mpg_counts <- mtcars |>
  group_by(cyl, am) |>
  summarize(count = n(), 
            avg_mpg = mean(mpg)) |>
  filter(count > 2)

mtcars_mutated |>
  pull(make) |>
  unique()

library(tidyverse)

forecast_sales <- tibble(
  week          = c(1, 2, 3),
  forecast = c("200-300", "300-400", "200-500")
)
forecast_sales

forecast_sales |>
  separate(forecast, c("low", "high"), "-", convert = TRUE)

table5

table5 |>
  unite("year", century:year, sep="")

exams <- tibble(name = c("Adriana", "Beth", "Candy", "Emily"), 
                midterm = c(90, 80, 95, 87), 
                final = c(99, 50, 70, 78))
assignments <- tibble(name = c("Adriana", "Beth", "Candy", "Florence"), 
                assign1 = c(80, 88, 93, 88), 
                assign2 = c(91, 61, 73, 83))
exams
assignments

scores_left <- left_join(assignments, exams, by="name")
scores_left
scores_right <- right_join(assignments, exams, by="name")
scores_right
scores_inner <- inner_join(assignments, exams, by = "name")
scores_inner
scores_full <- full_join(assignments, exams, by="name")
scores_full

bind_rows(assignments, exams)

scores_inner

scores_long <- scores_inner |>
  pivot_longer(c(assign1, assign2, midterm, final), 
               names_to = "assessment", values_to = "score")
scores_long

knitr::include_graphics('images/pivot_longer.png')

scores_long |>
  pivot_wider(names_from = assessment, values_from = score)

knitr::include_graphics('images/pivot_wider.png')

scores_long |>
  pivot_wider(names_from = assessment, 
              values_from = score, names_prefix = "assess_")

slice_head(table2, n = 5)

sales <- tibble(
  year     = c(2020, 2021, 2020, 2021),
  quarter  = c(1, 2, 1, 2),
  sale     = c(70, 80, 62, 100)
)

library(tidyverse)

mtcars_tibble <- mtcars |>
  rownames_to_column("model_name") 

one_to_ten <- function() { 
  print(1:10) 
}

one_to_ten()

one_to_ten <- function() { 
  print(10:1) 
}

one_to_ten()

my_family <- function() {
  c("Amy", "Billie", "Casey", "Debbie", "Eddie", "Freddie", "Gary",
    "Hary", "Ivy", "Jackie", "Lily", "Mikey", "Nellie", "Odie",
    "Paulie", "Quincy", "Ruby", "Stacey", "Tiffany")
}

a <- my_family()
a

passes_100 <- function(x) {
  max(100, x)
}

passes_100(50)  # a value smaller than 100
passes_100(2021)  # a value larger than 100

mtcars_tibble <- mtcars |>
  as_tibble()

mtcars_tibble

wt_conversion <- function(x) {
  x * 0.454
}

wt_conversion(100:105)

mtcars_transformed <- mtcars_tibble |>
  mutate(wt = wt_conversion(wt))
mtcars_transformed

cutoff_400 <- function(x) {
  min(400, x)
}

mtcars_transformed <- mtcars_tibble |>
  mutate(disp = cutoff_400(disp))
mtcars_transformed

wt_conversion(399:405)

cutoff_400(398:405)

map_dbl(399:405, wt_conversion)

map_int(1:5, function(x) x)

map_int(1:5, \(x) x)

map_dbl(1:5, \(x) x ** 2)

map_dbl(1:6, \(x) 5)

mtcars_transformed <- mtcars_tibble |>
  mutate(disp = map_dbl(disp, cutoff_400))
mtcars_transformed

knitr::include_graphics('images/csc100-Page-2.drawio.png')

library(tidyverse)

trouble_temps <- tibble(city = c("Miami", "Boston", 
                                 "Seattle", "Arlington"),
                        week1 = c(89, 88, 87, NA), 
                        week2 = c(91, NA, 86, 75), 
                        week3 = c(88, 85, 88, NA))
trouble_temps

temps_clean <- trouble_temps |> 
  drop_na()
temps_clean

trouble_temps |>
  fill(week1:week3, .direction = "up")

trouble_temps |>
  fill(week1:week3, .direction = "updown")

trouble_temps |>
  mutate(week1 = replace_na(week1, 70))

trouble_temps |>
  mutate(across(week1:week3, function(x) replace_na(x, 70)))

trouble_temps |> 
  replace_na(list(week1 = 89, week2 = 91, week3 = 88))

library(tidyverse)
library(edsdata)

csc_course_lab
csc_course_hw
csc_course_qz

csc_course_merged <- csc_course_hw |>
  inner_join(csc_course_lab, by = "number") |>
  inner_join(csc_course_qz, by = "number")
csc_course_merged

csc_course_merged <- csc_course_merged |>
  mutate(across(everything(), \(x) replace_na(x, 0)))
csc_course_merged

convert_to_100_points <- function(x) {
  return((x / 10) * 100)
}

csc_course_merged <- csc_course_merged |>
  mutate(across(matches("lab"), convert_to_100_points),
         across(matches("hw"), convert_to_100_points))
csc_course_merged

csc_course_merged <- csc_course_merged |>
  pivot_longer(starts_with("hw") | 
                 starts_with("lab") | starts_with("qz"), 
               names_to = "assessment", values_to = "score")
csc_course_merged

str_view_all("hw05", "[0-9]+")

str_replace("hw05", "[0-9]+", "")

csc_course_merged <- csc_course_merged |>
  mutate(assessment = str_replace(assessment, "[0-9]+", ""))
csc_course_merged

csc_course_merged |>
  group_by(number, assessment) |>
  summarize(mean_score = mean(score))

csc_course_merged |> 
  group_by(assessment) |>
  summarize(mean_score = mean(score))

csc_summary <- csc_course_merged |> 
  group_by(number) |>
  summarize(mean_overall_score = mean(score))
csc_summary

csc_summary |>
  filter(mean_overall_score < 70)

library(tidyverse)
library(edsdata)

scorecard_fl

scorecard_relevant <- scorecard_fl |>
  select(INSTNM:NPT4_PRIV) 
scorecard_relevant

scorecard_relevant |>
  filter(!is.na(NPT4_PUB) | !is.na(NPT4_PRIV)) 

## scorecard_relevant |>
##   filter(INSTNM == "Florida Institute of Ultrasound Inc")

scorecard_relevant |>
  filter(INSTNM == "Florida Institute of Ultrasound Inc") |>
  relocate(c(NPT4_PUB, NPT4_PRIV), .before = CITY)

scorecard_relevant <- scorecard_relevant |>
  mutate(across(everything(), \(x) na_if(x, "NULL")))

scorecard_relevant |>
  filter(!is.na(NPT4_PUB) | !is.na(NPT4_PRIV)) 

with_net_tuition <- scorecard_relevant |>
  mutate(across(everything(), \(x) na_if(x, "NULL"))) |> 
  filter(!is.na(NPT4_PUB) | !is.na(NPT4_PRIV)) |>
  mutate(NPT4_PUB = as.double(NPT4_PUB),
         NPT4_PRIV = as.double(NPT4_PRIV))
with_net_tuition

with_net_tuition |>
  select(ZIP)

str_replace(c("33172-2209", "34474"), "\\-[:number:]+", "")

with_net_tuition |> 
  mutate(ZIP5 = str_replace(ZIP, "\\-[:number:]+", "")) |>
  relocate(ZIP5, .before = CITY)

with_clean_zip <- with_net_tuition |>
  mutate(ZIP = str_replace(ZIP, "\\-[:number:]+", "")) |>
  relocate(ZIP, .before = CITY) |>
  filter(str_starts(ZIP, "331")) 
with_clean_zip

with_clean_zip |>
  unite("NPT4", NPT4_PUB:NPT4_PRIV, na.rm = TRUE) 

tuition_tidy <- with_clean_zip |>
  mutate(ISPUB = !(is.na(NPT4_PUB))) |>
  unite("NPT4", NPT4_PUB:NPT4_PRIV, na.rm = TRUE) |>
  mutate(NPT4 = as.integer(NPT4))
tuition_tidy

tuition_tidy |>
  group_by(ZIP, ISPUB) |>
  summarize(NPT4_AVE = mean(NPT4)) |>
  ungroup() |>
  arrange(desc(NPT4_AVE)) 

tuition_tidy |>
  filter(ZIP == "33146")

tuition_tidy |>
  group_by(ZIP) |>
  mutate(NPT4_avg = mean(NPT4),
         tuition_diff = NPT4 - NPT4_avg) |>
  ungroup() |>
  arrange(tuition_diff) |>
  relocate(ISPUB, .before = ZIP)

## library(tidyverse)
## library(edsdata)
## library(gapminder)

is_it_tidy <- list(table5, table1, table3, table2)

## is_it_tidy[[1]]  # Table 1

## is_it_tidy[[2]] # Table 2

## is_it_tidy[[3]] # Table 3

## is_it_tidy[[4]] # Table 4

## gap <- gapminder
## gap

## gap <- gapminder

## first(c(10, 4, 9, 42, -2))

affordable

## to_percent <- function(prop) {
##   scale <- 100
## 
## }

fruit_basket <- c("lychee", "banana", "mango")

nysalary

## nysalary |>
##   summarize(avg_compensation = mean(`Total Compensation`))

knitr::include_graphics("images/abs.png")

abs_partp2017

abs_resp2017
abs_census2016

## unemp_by_state2008 <- election_unemp2008 |>
##   group_by(state) |>
##   summarize(avg_unemp_rate = mean(Unemployment_rate_2008,
##                                   na.rm = TRUE))
## unemp_by_state2008

pluto

## scorecard_fl

relevant_cols <- c("INSTNM", "CITY", "ZIP", "UGDS", 
                   "NPT4_PUB", "NPT4_PRIV", 
                   "UGDS_WHITE", "UGDS_BLACK", 
                   "UGDS_HISP", "UGDS_ASIAN", "UGDS_AIAN")

## embraced <- function(column_label) {
##   student_counts_others |>
##     summarize(mean_num =
##                 mean({{ column_label }}, na.rm=TRUE))
## }
## embraced(n_white)
## embraced(n_others)

## examine_by_zip <- function(column_label) {
## 
## 
## }
## 
## examine_by_zip(n_white)

## t(mtcars)

## ddd <- tribble(~Name,~English,~Mathematics,
##                "Johnny",100,90,
##                "Katy",90,100,
##                "Lauren",89,94,
##                "Manuel",95,79,
##                "Nancy",80,80)
## ddd

## dddT <- t(ddd)
## dddT

## ddd1 <- t(ddd)
## dddT <- as.data.frame(ddd1)
## dddT

## ddd1 <- column_to_rownames(ddd, "Name")
## dddT <- t(ddd1)
## dddT

## na_data |> filter(B <= 90)

## na_data |> filter(B <= 90, B >= 80)

## na_data |> filter(C <= min(B, na.rm = TRUE))

## # Load data and replace all NA's with 0
## # Save it as `csc0`
## read_csv("data/csc_course.csv") |>
##   replace(is.na(.), 0) -> csc0
## # Create a lab part `csc_lab` by:
## # (a) selecting "number" and the ten lab scores,
## # (b) pivoting the lab scores into an attribute pair (lab_index,lab_score)
## #     while removing "lab" from the attribute names,
## #     where selection of the columns for pivoting is by way of specifying all the columns other than "number",
## # (c) creating a new attribute "temp_id" by connecting the value appearing in "number",
## #     an underscore, and the value appearing in "lab_index"
## csc0 |> select(number, lab01:lab10) |>
##   pivot_longer(cols = !number, names_to = "lab_index", values_to = "lab_score", names_prefix="lab" ) |>
##   mutate(temp_id = str_c(number, "_", lab_index)) -> csc_lab
## # Create a homework part `hw_lab` by:
## # (a) selecting "number" and the first ten homework scores,
## # (b) pivoting the homework scores into an attribute pair (hw_index,hw_score)
## #     while removing "hw" from the attribute names,
## #     where selection of the columns for pivoting is by way of specifying all the columns other than "number",
## # (c) creating a new attribute "temp_id" by connecting the value appearing in "number",
## #     an underscore, and the value appearing in "hw_index"
## csc0 |> select(number, hw01:hw10) |>
##   pivot_longer(cols = !number, names_to = "hw_index", values_to = "hw_score", names_prefix="hw" ) |>
##   mutate(temp_id = str_c(number, "_", hw_index)) -> csc_hw
## # check the two sub parts
## csc_lab
## csc_hw
## # Create a merger `csc_new` by:
## # (a) call `full_join` using "temp_id" as the attribute for connecting
## # (b) compute the difference as a new attribute "diff"
## # (c) create a new attribute "index" whose values are identical to those of "lab_index"
## # (d) select five columns "number", "index", "lab_socre", "hw_score", and "diff" in this order.
## full_join(csc_lab, csc_hw, name = temp_id) |> mutate(diff = hw_score - lab_score) |>
##   mutate(index = lab_index) |>
##   select(number,index,lab_score,hw_score,diff) -> csc_new
## csc_new
