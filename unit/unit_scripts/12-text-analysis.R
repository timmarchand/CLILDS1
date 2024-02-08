library(tidyverse)
library(tidytext)
library(gutenbergr)

gutenberg_works(title == "Moby Dick")

moby_dick <- gutenberg_download(gutenberg_id = 15, 
                  mirror = "http://mirrors.xmission.com/gutenberg/")
moby_dick

by_chapter <- moby_dick |>
  select(-gutenberg_id) |>
  mutate(document = 'Moby Dick', 
         linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex('^CHAPTER '))))
by_chapter

regex <- "^CHAPTER "

str_view_all("CHAPTER CXVI. THE DYING WHALE", regex) # match

str_view_all("Beloved shipmates, clinch the last verse of the
first chapter of Jonah—", regex)  # no match

by_chapter <- by_chapter |>
  filter(chapter > 0)
by_chapter

some_moby_df <- by_chapter |>
  slice(4:6)
some_moby_df

tokenized <- some_moby_df |>
  pull(text) |>
  str_split(" ")
tokenized

tokenized[[1]][3]

tidy_df <- some_moby_df |>
  unnest_tokens(word, text)
tidy_df

tidy_moby <- by_chapter |>
  unnest_tokens(word, text)
tidy_moby

tidy_moby |>
  count(word, sort = TRUE)

stop_words

tidy_moby_filtered <- tidy_moby |>
  anti_join(stop_words)
tidy_moby_filtered

tidy_moby_filtered |>
  count(word, sort = TRUE)

tidy_moby_filtered |>
  count(word, sort = TRUE) |>
  filter(n > 200) |>
  mutate(word = reorder(word, n)) |>
  ggplot() +
  geom_bar(aes(x=n, y=word), stat="identity")

some_moby_df

## some_moby_df |>
##   unnest_tokens(word, text) |>
##   count(document, word) |>
##   cast_dfm(document, word, n)

tidy_moby_filtered |>
  count(document, word) |>
  cast_dtm(document, word, n)

library(tidyverse)
library(tidytext)
library(gutenbergr)

melville <- gutenberg_download(c(11231, 15, 10712, 1900), 
              mirror = "http://mirrors.xmission.com/gutenberg/")

tidy_melville <- melville |>
  unnest_tokens(word, text) |>
  anti_join(stop_words) |>
  mutate(word = str_extract(word, "[a-z]+"))

tidy_melville <- tidy_melville |>
  mutate(title = recode(gutenberg_id, 
                    '15' = 'Moby Dick', 
                    '11231' = 'Bartleby, the Scrivener',
                    '10712' = 'White Jacket',
                    '1900' = 'Typee: A Romance of the South Seas'))

tidy_melville |>
  group_by(title) |>
  count(word, sort = TRUE) |>
  summarize(num_words = sum(n)) |>
  arrange(desc(num_words))

tidy_melville <- tidy_melville |>
  group_by(title) |>
  count(word, sort = TRUE) |>
  ungroup()
tidy_melville

tidy_melville |>
  filter(n > 250) |>
  mutate(word = reorder(word, n)) |>
  ggplot() +
  geom_bar(aes(x=n, y=word, fill=title), stat="identity")

tidy_melville_prop <- tidy_melville |>
  group_by(title) |>
  mutate(proportion = n / sum(n)) |>
  ungroup()
tidy_melville_prop

tidy_melville_prop |>
  filter(proportion > 0.005) |>
  mutate(word = reorder(word, proportion)) |>
  ggplot() +
  geom_bar(aes(x=proportion, y=word, fill=title), 
           stat="identity")

top_moby <- tidy_melville |>
  filter(title == "Moby Dick") |>
  mutate(proportion = n / sum(n)) |>
  arrange(desc(proportion)) |>
  slice(1:10) |>
  select(word)
top_moby

top_moby_words_other_texts <- tidy_melville |>
  group_by(title) |>
  mutate(proportion = n / sum(n)) |>
  inner_join(top_moby, by="word") |>
  ungroup()
top_moby_words_other_texts

ggplot(top_moby_words_other_texts) + 
  geom_bar(aes(x=proportion, 
               y=factor(word, level=pull(top_moby, word)), 
               fill=title), 
           position="dodge",stat="identity") +
  labs(y="word")

library(tidyverse)
library(tidytext)
library(gutenbergr)
library(topicmodels)

melville <- gutenberg_download(15, 
              mirror = "http://mirrors.xmission.com/gutenberg/")

melville <- melville |>
  select(-gutenberg_id) |>
  mutate(title = 'Moby Dick', 
         linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex('^CHAPTER ')))) |>
  filter(chapter > 0)

whitman <- gutenberg_works(title == "Leaves of Grass") |>
  gutenberg_download(meta_fields = "title", 
    mirror = "http://mirrors.xmission.com/gutenberg/")

whitman <- whitman |>
  select(-gutenberg_id) |>
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex('^BOOK ')))) |>
  filter(chapter > 0)

hawthorne <- gutenberg_works(title == "The Scarlet Letter") |>
  gutenberg_download(meta_fields = "title", 
    mirror = "http://mirrors.xmission.com/gutenberg/")

hawthorne <- hawthorne |>
  select(-gutenberg_id) |>
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex('^[XIV]+\\.')))) |>
  filter(chapter > 0)

books <- bind_rows(whitman, melville, hawthorne) 
books

books |>
  group_by(title) |>
  summarize(num_chapters = max(chapter))

chapter_documents <- books |>
  unite(document, title, chapter)

documents_tokenized <- chapter_documents |>
  unnest_tokens(word, text)
documents_tokenized

document_counts <- documents_tokenized |>
  anti_join(stop_words) |>
  count(document, word, sort = TRUE) |>
  ungroup()
document_counts

chapters_dtm <- document_counts |>
  cast_dtm(document, word, n)
chapters_dtm

lda_model <- LDA(chapters_dtm, k = 3, control = list(seed = 50))

chapter_beta <- tidy(lda_model, matrix = "beta")
chapter_beta

top_terms <- chapter_beta |>
  group_by(topic) |>
  arrange(topic, -beta) |>
  slice(1:5) |>
  ungroup() 
top_terms

top_terms |>
  mutate(term = reorder_within(term, beta, topic)) |>
  ggplot() +
  geom_bar(aes(beta, term, fill = factor(topic)), 
           stat="identity", show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  scale_y_reordered()

chapters_gamma <- tidy(lda_model, matrix = "gamma")
chapters_gamma

chapters_gamma <- chapters_gamma |>
  separate(document, c("title", "chapter"), sep = "_", 
           convert = TRUE)
chapters_gamma

chapters_gamma |>
  filter(title == "The Scarlet Letter", chapter == 17)

chapter_label <- chapters_gamma |>
  group_by(title, chapter) |>
  slice_max(gamma) |>
  ungroup()
chapter_label

labels_summarized <- chapter_label |>
  group_by(title, topic) |>
  summarize(num_chapters = n()) |>
  ungroup()
labels_summarized

ggplot(labels_summarized) + 
  geom_bar(aes(x = num_chapters, 
               y = title, fill = factor(topic)), 
           stat = "identity") +
  labs(fill = "topic")

book_labels <- labels_summarized |>
  group_by(title) |>
  slice_max(num_chapters) |>
  ungroup() |> 
  transmute(label = title, topic)
book_labels

chapter_label |>
  inner_join(book_labels, by = "topic") |>
  filter(title != label) 

## library(tidytext)
## library(gutenbergr)
## library(topicmodels)

## gutenberg_works(author == "Austen, Jane")

## austen_letters <- gutenberg_download(gutenberg_id = 42078,
##   "http://mirrors.xmission.com/gutenberg/")
## austen_letters

## (austen_letters |> pull(text))[241:250]

## (austen_letters |> pull(text))[6445:6454]

## (austen_letters |> pull(text))[3216:3225]

## (austen_letters |> pull(text))[8531:8540]

## test_vector <- c( "I.", "    I.", "VII.",
##                   "THE END.", "FOOTNOTE:",
##                   "FOOTNOTES:",
##                   "ds", "    world")

## letters_clean <- letters |>
##   slice(start_no:end_no)
## letters_clean

## regex_index <- regex('^[IVXLCDM]++\\.')
## str_which(letters$text, regex_index)

## letter_tokens_nostop_ranked

## ggplot(freq_by_author_prop_long,
##        aes(x = `Jane Austen`, y = `G. K. Chesterton`,
##            color = abs(`G. K. Chesterton` - `Jane Austen`))) +
##   geom_abline(color = "gray", lty = 2) +
##   geom_text(aes(label = word), check_overlap = TRUE) +
##   theme(legend.position="none")
## 

## tidy_melville |>
##   count(word, sort=TRUE)

## tidy_hawthorne |>
##   count(word, sort = TRUE) |>
##   filter(word == "lord")

## hawthorne <- gutenberg_download(c(33, 77, 1900, 508))

## tidy_hawthorne <- hawthorne |>
##   unnest_tokens(word, text) |>
##   anti_join(stop_words) |>
##   mutate(word = str_extract(word, "[a-z]+"))

## gutenberg_works(author == "Whitman, Walt")

## whitman <- gutenberg_download(c(1322, 8801, 27494))

## tidy_whitman <- whitman |>
##   unnest_tokens(word, text) |>
##   anti_join(stop_words) |>
##   mutate(word = str_extract(word, "[a-z]+"))

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

## frequency

## library(scales)

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

## cor.test(data = frequency[frequency$author == "Nathaniel Hawthorne",],
##          ~ proportion + `Herman Melville`)

## cor.test(data = frequency[frequency$author == "Walt Whitman",],
##          ~ proportion + `Herman Melville`)

## gutenberg_works(author == "Whitman, Walt")

## gutenberg_works(title == "The Scarlet Letter")
