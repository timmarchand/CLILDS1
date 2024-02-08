## ----echo=FALSE, message=FALSE, warning=FALSE---------------
library(webshot)
options(tinytex.clean = FALSE)


## ----eval=FALSE---------------------------------------------
## install.packages("tidyverse")


## ----eval=FALSE---------------------------------------------
## install.packages("devtools")


## ----eval=FALSE---------------------------------------------
## devtools::install_github("jerrybonnell/edsdata")


## ----message = FALSE, warning = FALSE-----------------------
library(tidyverse)


## ----eval = FALSE-------------------------------------------
## WorldPhones


## ----echo = FALSE-------------------------------------------
as_tibble(WorldPhones)


## -----------------------------------------------------------
transformed <- WorldPhones |>
  as_tibble() |>
  rownames_to_column("Year") |> 
  pivot_longer(cols = 2:8, names_to = "Region", values_to = "Count")


## -----------------------------------------------------------
transformed


## -----------------------------------------------------------
ggplot(transformed, aes(x = Year, y = Count)) +
  geom_point(aes(color = Region))


## -----------------------------------------------------------
ggplot(transformed) +
  geom_bar(aes(x = Year, y = Count, fill = as.factor(Region)),
           stat = "identity", position = "fill") +
  labs(fill = "Region")


## ----message=FALSE, warning = FALSE-------------------------
library(tidyverse)
library(readtext)


## -----------------------------------------------------------
url <- "https://www.gutenberg.org/files/2701/2701-0.txt"
moby <- readtext(url) |>
  str_sub(27780)


## -----------------------------------------------------------
moby_chapters <- unlist(str_split(moby, fixed("CHAPTER"))) |>
  str_trim() |>
  str_replace_all("\\s+", " ") |>
  str_to_lower()


## ---- results='hide'----------------------------------------
moby_df <- tibble("chapters" = moby_chapters) |> 
  slice(-1)


## ----eval = FALSE-------------------------------------------
## slice_head(moby_df, n = 5)


## ----warning = FALSE, message = FALSE, echo = FALSE---------
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


## -----------------------------------------------------------
moby_df <- moby_df |>
  mutate(
    chapter_num = row_number(),
    ship = str_count(chapters, "ship"),
    sea = str_count(chapters, "sea"),
    god = str_count(chapters, "god")
  ) 


## -----------------------------------------------------------
moby_df |>
  select(chapter_num, ship, sea, god)


## ----message = FALSE----------------------------------------
ggplot(moby_df) +
  geom_point(aes(x = sea, y = ship, color = "ship")) + 
  geom_point(aes(x = sea, y = god, color = "god")) + 
  ylab("count")


## ---- echo=FALSE--------------------------------------------
## Directory path as an example
library(downloadthis)
download_dir(
  path = "/Users/timmarchand/Downloads/ISS_DATASCI",
  output_name = "ISS_DATASCI",
  button_label = "Folder structure",
  button_type = "success",
  has_icon = TRUE,
  icon = "fa fa-save",
  self_contained = FALSE
)


## ---- echo=FALSE--------------------------------------------
# download_file(
#   path = "../downloads/weekly_notes.Rmd",
#   button_label = "Download Weekly Notes",
#   button_type = "info",
#   has_icon = TRUE,
#   icon = "fa fa-save",
#   self_contained = FALSE
# )


## ---- echo=FALSE--------------------------------------------
# download_file(
#   path = "../downloads/R_Markdown_Template.Rmd",
#   button_label = "Download R_Markdown_Template",
#   button_type = "info",
#   has_icon = TRUE,
#   icon = "fa fa-save",
#   self_contained = FALSE
# )


## ---- echo=FALSE--------------------------------------------
# download_file(
#   path = "../downloads/data_exploration_template.R",
#   button_label = "Download Exporing data R template",
#   button_type = "info",
#   has_icon = TRUE,
#   icon = "fa fa-save",
#   self_contained = FALSE
# )


## ---- echo=FALSE--------------------------------------------
## Directory path as an example
download_dir(
  path = "../prob_sets",
  output_name = "prob_sets",
  button_label = "Download Problem Sets",
  button_type = "success",
  has_icon = TRUE,
  icon = "fa fa-save",
  self_contained = FALSE
)


## ---- echo=FALSE--------------------------------------------
# download_file(
#   path = "../downloads/example_project.Rmd",
#   button_label = "Example Project 1",
#   button_type = "info",
#   has_icon = TRUE,
#   icon = "fa fa-save",
#   self_contained = FALSE
# )


## ---- echo=FALSE--------------------------------------------
# download_file(
#   path = "../downloads/example_project_2.Rmd",
#   button_label = "Example Project 2",
#   button_type = "info",
#   has_icon = TRUE,
#   icon = "fa fa-save",
#   self_contained = FALSE
# )


## ---- echo=FALSE, fig.align="center", out.width='60%', fig.asp=1/2----
library(here)
knitr::include_graphics(here("images", "project_wizard.png"))


## ---- echo=FALSE, fig.align="center", out.width='60%', fig.asp=1/2----
knitr::include_graphics(here("images", "project_wizard2.png"))


## ---- echo=FALSE, fig.align="center", out.width='80%', fig.asp=1/2----
knitr::include_graphics(here('images','rstudio_environment.png'))


## -----------------------------------------------------------
my_value <- 5


## -----------------------------------------------------------
my_value <- "hello data science!"


## ----eval = FALSE-------------------------------------------
## plot(cyl ~ mpg, mtcars)


## ---- echo=FALSE, fig.align="center", out.width='60%', fig.asp=1/2----
knitr::include_graphics(here('images','file_directory.png'))


## ---- echo=FALSE, fig.align="center", out.width='60%', fig.asp=1/2----
knitr::include_graphics(here('images','notebook_example.png'))


## ----eval=FALSE---------------------------------------------
## library(tidyverse)
## library(edsdata)
## library(gapminder)
## library(emojifont)


## ----eval=FALSE---------------------------------------------
## emoji("star")


## ----eval=FALSE---------------------------------------------
## print("what is missing?


## ----eval=FALSE---------------------------------------------
## bubble
## tea


## ----eval=FALSE---------------------------------------------
## library(edsdata)
## ggplot(moby_dick_counts) +
##   geom_point(aes(x = whale, y = sea, color = "sea")) +
##   geom_point(aes(x = whale, y = ahab, color = "ahab")) +
##   ylab("count")

