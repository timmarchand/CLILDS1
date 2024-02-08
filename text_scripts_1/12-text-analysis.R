## ----message = FALSE, warning = FALSE-----------------------
library(tidyverse)
library(tidytext)
library(gutenbergr)


## -----------------------------------------------------------
gutenberg_works(title == "Moby Dick")


## ----message=FALSE------------------------------------------
moby_dick <- gutenberg_download(gutenberg_id = 15, 
                  mirror = "http://mirrors.xmission.com/gutenberg/")
moby_dick


## -----------------------------------------------------------
by_chapter <- moby_dick |>
  select(-gutenberg_id) |>
  mutate(document = 'Moby Dick', 
         linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex('^CHAPTER '))))
by_chapter


## -----------------------------------------------------------
regex <- "^CHAPTER "


## ----message=FALSE------------------------------------------
str_view_all("CHAPTER CXVI. THE DYING WHALE", regex) # match


## ----message=FALSE------------------------------------------
str_view_all("Beloved shipmates, clinch the last verse of the
first chapter of Jonah—", regex)  # no match


## -----------------------------------------------------------
by_chapter <- by_chapter |>
  filter(chapter > 0)
by_chapter


## -----------------------------------------------------------
some_moby_df <- by_chapter |>
  slice(4:6)
some_moby_df


## -----------------------------------------------------------
tokenized <- some_moby_df |>
  pull(text) |>
  str_split(" ")
tokenized


## -----------------------------------------------------------
tokenized[[1]][3]


## -----------------------------------------------------------
tidy_df <- some_moby_df |>
  unnest_tokens(word, text)
tidy_df


## -----------------------------------------------------------
tidy_moby <- by_chapter |>
  unnest_tokens(word, text)
tidy_moby


## -----------------------------------------------------------
tidy_moby |>
  count(word, sort = TRUE)


## -----------------------------------------------------------
stop_words


## ----message=FALSE------------------------------------------
tidy_moby_filtered <- tidy_moby |>
  anti_join(stop_words)
tidy_moby_filtered


## -----------------------------------------------------------
tidy_moby_filtered |>
  count(word, sort = TRUE)


## -----------------------------------------------------------
tidy_moby_filtered |>
  count(word, sort = TRUE) |>
  filter(n > 200) |>
  mutate(word = reorder(word, n)) |>
  ggplot() +
  geom_bar(aes(x=n, y=word), stat="identity")


## ----echo=FALSE---------------------------------------------
some_moby_df


## ----eval=FALSE, message=FALSE, warning=FALSE---------------
## some_moby_df |>
##   unnest_tokens(word, text) |>
##   count(document, word) |>
##   cast_dfm(document, word, n)


## ----message=FALSE------------------------------------------
tidy_moby_filtered |>
  count(document, word) |>
  cast_dtm(document, word, n)


## ----message = FALSE, warning = FALSE-----------------------
library(tidyverse)
library(tidytext)
library(gutenbergr)


## ----message=FALSE------------------------------------------
melville <- gutenberg_download(c(11231, 15, 10712, 1900), 
              mirror = "http://mirrors.xmission.com/gutenberg/")


## ----message=FALSE------------------------------------------
tidy_melville <- melville |>
  unnest_tokens(word, text) |>
  anti_join(stop_words) |>
  mutate(word = str_extract(word, "[a-z]+"))


## -----------------------------------------------------------
tidy_melville <- tidy_melville |>
  mutate(title = recode(gutenberg_id, 
                    '15' = 'Moby Dick', 
                    '11231' = 'Bartleby, the Scrivener',
                    '10712' = 'White Jacket',
                    '1900' = 'Typee: A Romance of the South Seas'))


## -----------------------------------------------------------
tidy_melville |>
  group_by(title) |>
  count(word, sort = TRUE) |>
  summarize(num_words = sum(n)) |>
  arrange(desc(num_words))


## -----------------------------------------------------------
tidy_melville <- tidy_melville |>
  group_by(title) |>
  count(word, sort = TRUE) |>
  ungroup()
tidy_melville


## -----------------------------------------------------------
tidy_melville |>
  filter(n > 250) |>
  mutate(word = reorder(word, n)) |>
  ggplot() +
  geom_bar(aes(x=n, y=word, fill=title), stat="identity")


## -----------------------------------------------------------
tidy_melville_prop <- tidy_melville |>
  group_by(title) |>
  mutate(proportion = n / sum(n)) |>
  ungroup()
tidy_melville_prop


## -----------------------------------------------------------
tidy_melville_prop |>
  filter(proportion > 0.005) |>
  mutate(word = reorder(word, proportion)) |>
  ggplot() +
  geom_bar(aes(x=proportion, y=word, fill=title), 
           stat="identity")


## -----------------------------------------------------------
top_moby <- tidy_melville |>
  filter(title == "Moby Dick") |>
  mutate(proportion = n / sum(n)) |>
  arrange(desc(proportion)) |>
  slice(1:10) |>
  select(word)
top_moby


## -----------------------------------------------------------
top_moby_words_other_texts <- tidy_melville |>
  group_by(title) |>
  mutate(proportion = n / sum(n)) |>
  inner_join(top_moby, by="word") |>
  ungroup()
top_moby_words_other_texts


## -----------------------------------------------------------
ggplot(top_moby_words_other_texts) + 
  geom_bar(aes(x=proportion, 
               y=factor(word, level=pull(top_moby, word)), 
               fill=title), 
           position="dodge",stat="identity") +
  labs(y="word")


## ----message = FALSE, warning = FALSE-----------------------
library(tidyverse)
library(tidytext)
library(gutenbergr)
library(topicmodels)


## ----message=FALSE------------------------------------------
melville <- gutenberg_download(15, 
              mirror = "http://mirrors.xmission.com/gutenberg/")


## -----------------------------------------------------------
melville <- melville |>
  select(-gutenberg_id) |>
  mutate(title = 'Moby Dick', 
         linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex('^CHAPTER ')))) |>
  filter(chapter > 0)


## -----------------------------------------------------------
whitman <- gutenberg_works(title == "Leaves of Grass") |>
  gutenberg_download(meta_fields = "title", 
    mirror = "http://mirrors.xmission.com/gutenberg/")


## -----------------------------------------------------------
whitman <- whitman |>
  select(-gutenberg_id) |>
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex('^BOOK ')))) |>
  filter(chapter > 0)


## -----------------------------------------------------------
hawthorne <- gutenberg_works(title == "The Scarlet Letter") |>
  gutenberg_download(meta_fields = "title", 
    mirror = "http://mirrors.xmission.com/gutenberg/")


## -----------------------------------------------------------
hawthorne <- hawthorne |>
  select(-gutenberg_id) |>
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex('^[XIV]+\\.')))) |>
  filter(chapter > 0)


## -----------------------------------------------------------
books <- bind_rows(whitman, melville, hawthorne) 
books


## -----------------------------------------------------------
books |>
  group_by(title) |>
  summarize(num_chapters = max(chapter))


## -----------------------------------------------------------
chapter_documents <- books |>
  unite(document, title, chapter)


## -----------------------------------------------------------
documents_tokenized <- chapter_documents |>
  unnest_tokens(word, text)
documents_tokenized


## ----message=FALSE------------------------------------------
document_counts <- documents_tokenized |>
  anti_join(stop_words) |>
  count(document, word, sort = TRUE) |>
  ungroup()
document_counts


## -----------------------------------------------------------
chapters_dtm <- document_counts |>
  cast_dtm(document, word, n)
chapters_dtm


## -----------------------------------------------------------
lda_model <- LDA(chapters_dtm, k = 3, control = list(seed = 50))


## -----------------------------------------------------------
chapter_beta <- tidy(lda_model, matrix = "beta")
chapter_beta


## -----------------------------------------------------------
top_terms <- chapter_beta |>
  group_by(topic) |>
  arrange(topic, -beta) |>
  slice(1:5) |>
  ungroup() 
top_terms


## -----------------------------------------------------------
top_terms |>
  mutate(term = reorder_within(term, beta, topic)) |>
  ggplot() +
  geom_bar(aes(beta, term, fill = factor(topic)), 
           stat="identity", show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  scale_y_reordered()


## -----------------------------------------------------------
chapters_gamma <- tidy(lda_model, matrix = "gamma")
chapters_gamma


## -----------------------------------------------------------
chapters_gamma <- chapters_gamma |>
  separate(document, c("title", "chapter"), sep = "_", 
           convert = TRUE)
chapters_gamma


## -----------------------------------------------------------
chapters_gamma |>
  filter(title == "The Scarlet Letter", chapter == 17)


## -----------------------------------------------------------
chapter_label <- chapters_gamma |>
  group_by(title, chapter) |>
  slice_max(gamma) |>
  ungroup()
chapter_label


## ----message=FALSE------------------------------------------
labels_summarized <- chapter_label |>
  group_by(title, topic) |>
  summarize(num_chapters = n()) |>
  ungroup()
labels_summarized


## ----message=FALSE------------------------------------------
ggplot(labels_summarized) + 
  geom_bar(aes(x = num_chapters, 
               y = title, fill = factor(topic)), 
           stat = "identity") +
  labs(fill = "topic")


## -----------------------------------------------------------
book_labels <- labels_summarized |>
  group_by(title) |>
  slice_max(num_chapters) |>
  ungroup() |> 
  transmute(label = title, topic)
book_labels


## -----------------------------------------------------------
chapter_label |>
  inner_join(book_labels, by = "topic") |>
  filter(title != label) 


## ----eval=FALSE, message=FALSE, warning=FALSE---------------
## library(tidytext)
## library(gutenbergr)
## library(topicmodels)


## ----eval=FALSE, message=FALSE, warning=FALSE---------------
## gutenberg_works(author == "Austen, Jane")


## ----eval=FALSE, message=FALSE, warning=FALSE---------------
## austen_letters <- gutenberg_download(gutenberg_id = 42078,
##   "http://mirrors.xmission.com/gutenberg/")
## austen_letters


## ----eval=FALSE, message=FALSE, warning=FALSE---------------
## (austen_letters |> pull(text))[241:250]


## ----eval=FALSE, message=FALSE, warning=FALSE---------------
## (austen_letters |> pull(text))[6445:6454]


## ----eval=FALSE, message=FALSE, warning=FALSE---------------
## (austen_letters |> pull(text))[3216:3225]


## ----eval=FALSE, message=FALSE, warning=FALSE---------------
## (austen_letters |> pull(text))[8531:8540]


## ----eval=FALSE, message=FALSE, warning=FALSE---------------
## test_vector <- c( "I.", "    I.", "VII.",
##                   "THE END.", "FOOTNOTE:",
##                   "FOOTNOTES:",
##                   "ds", "    world")


## ----eval=FALSE, message=FALSE, warning=FALSE---------------
## letters_clean <- letters |>
##   slice(start_no:end_no)
## letters_clean


## ----eval=FALSE, message=FALSE, warning=FALSE---------------
## regex_index <- regex('^[IVXLCDM]++\\.')
## str_which(letters$text, regex_index)


## ----eval=FALSE, message=FALSE, warning=FALSE---------------
## letter_tokens_nostop_ranked


## ----eval=FALSE, message=FALSE, warning=FALSE---------------
## ggplot(freq_by_author_prop_long,
##        aes(x = `Jane Austen`, y = `G. K. Chesterton`,
##            color = abs(`G. K. Chesterton` - `Jane Austen`))) +
##   geom_abline(color = "gray", lty = 2) +
##   geom_text(aes(label = word), check_overlap = TRUE) +
##   theme(legend.position="none")
## 


## ----eval=FALSE---------------------------------------------
## tidy_melville |>
##   count(word, sort=TRUE)


## ----eval=FALSE---------------------------------------------
## tidy_hawthorne |>
##   count(word, sort = TRUE) |>
##   filter(word == "lord")


## ----eval=FALSE---------------------------------------------
## hawthorne <- gutenberg_download(c(33, 77, 1900, 508))


## ----eval=FALSE, message=FALSE------------------------------
## tidy_hawthorne <- hawthorne |>
##   unnest_tokens(word, text) |>
##   anti_join(stop_words) |>
##   mutate(word = str_extract(word, "[a-z]+"))


## ----eval=FALSE---------------------------------------------
## gutenberg_works(author == "Whitman, Walt")


## ----eval=FALSE---------------------------------------------
## whitman <- gutenberg_download(c(1322, 8801, 27494))


## ----eval=FALSE, message=FALSE------------------------------
## tidy_whitman <- whitman |>
##   unnest_tokens(word, text) |>
##   anti_join(stop_words) |>
##   mutate(word = str_extract(word, "[a-z]+"))


## ----eval=FALSE---------------------------------------------
## frequency <- bind_rows(mutate(tidy_whitman, author = "Walt Whitman"),
##                        mutate(tidy_hawthorne, author = "Nathaniel Hawthorne"),
##                        mutate(tidy_melville, author = "Herman Melville")) |>
##   count(author, word) |>
##   group_by(author) |>
##   mutate(proportion = n / sum(n)) |>
##   select(-n) |>
##   pivot_wider(names_from = author, values_from = proportion) |>
##   pivot_longer(`Walt Whitman`:`Nathaniel Hawthorne`,
##                names_to = "author", values_to = "proportion")
## 


## ----eval=FALSE---------------------------------------------
## frequency


## ----eval=FALSE---------------------------------------------
## library(scales)


## ----eval=FALSE---------------------------------------------
## # expect a warning about rows with missing values being removed
## ggplot(frequency, aes(x = proportion, y = `Herman Melville`,
##                       color = abs(`Herman Melville` - proportion))) +
##   geom_abline(color = "gray40", lty = 2) +
##   geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
##   geom_text(aes(label = word), check_overlap = TRUE) +
##   scale_x_log10(labels = percent_format()) +
##   scale_y_log10(labels = percent_format()) +
##   scale_color_gradient(limits = c(0, max(pull(frequency, proportion))),
##                        low = "darkslategray4", high = "deeppink") +
##   facet_wrap(~author, ncol = 2) +
##   theme(legend.position="top") +
##   labs(y = "Herman Melville", x = NULL, color='')
## 


## ----eval=FALSE---------------------------------------------
## cor.test(data = frequency[frequency$author == "Nathaniel Hawthorne",],
##          ~ proportion + `Herman Melville`)


## ----eval=FALSE---------------------------------------------
## cor.test(data = frequency[frequency$author == "Walt Whitman",],
##          ~ proportion + `Herman Melville`)


## ----eval=FALSE---------------------------------------------
## gutenberg_works(author == "Whitman, Walt")


## ----eval=FALSE---------------------------------------------
## gutenberg_works(title == "The Scarlet Letter")

