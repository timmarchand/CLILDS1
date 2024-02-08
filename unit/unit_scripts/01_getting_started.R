#' Unit 1 Getting started
#' 
#' This is a raw script file for the same unit from the course textbook
#' You can follow the output of the code chunks in the text in this file
#' Feel free to experiment with the code as well
#' Use the "#" sign to add your own comments to the code!

install.packages("tidyverse")
install.packages("devtools")
devtools::install_github("jerrybonnell/edsdata")

library(tidyverse)

## WorldPhones

as_tibble(WorldPhones)

transformed <- WorldPhones |>
  as_tibble() |>
  rownames_to_column("Year") |> 
  pivot_longer(cols = 2:8, names_to = "Region", values_to = "Count")

transformed

ggplot(transformed, aes(x = Year, y = Count)) +
  geom_point(aes(color = Region))

ggplot(transformed) +
  geom_bar(aes(x = Year, y = Count, fill = as.factor(Region)),
           stat = "identity", position = "fill") +
  labs(fill = "Region")

library(tidyverse)
library(readtext)

url <- "https://www.gutenberg.org/files/2701/2701-0.txt"
moby <- readtext(url) |>
  str_sub(27780)

moby_chapters <- unlist(str_split(moby, fixed("CHAPTER"))) |>
  str_trim() |>
  str_replace_all("\\s+", " ") |>
  str_to_lower()

moby_df <- tibble("chapters" = moby_chapters) |> 
  slice(-1)

slice_head(moby_df, n = 5)

library(kableExtra)

moby_chapters_tmp <- unlist(str_split(moby, fixed("CHAPTER"))) |>
  str_trim() |>
  str_replace_all("\\s+", " ") |>
  str_to_lower() |>
  str_sub(end = 200) 
  

disp_df <- tibble("chapters" = moby_chapters_tmp) |>
  slice(-1)

knitr::kable(
  head(disp_df, 5), booktabs = TRUE) |>
  kable_styling(bootstrap_options = "striped", full_width = F) |>
  column_spec(1, width = "30em")

moby_df <- moby_df |>
  mutate(
    chapter_num = row_number(),
    ship = str_count(chapters, "ship"),
    sea = str_count(chapters, "sea"),
    god = str_count(chapters, "god")
  ) 

moby_df |>
  select(chapter_num, ship, sea, god)

ggplot(moby_df) +
  geom_point(aes(x = sea, y = ship, color = "ship")) + 
  geom_point(aes(x = sea, y = god, color = "god")) + 
  ylab("count")

