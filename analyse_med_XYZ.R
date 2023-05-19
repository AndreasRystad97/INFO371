library(readtext)
library(tidyverse)
library(quanteda)
library(stopwords)
library(dplyr)
library(ggplot2)
library(tidytext)
library(quanteda.textmodels)
library(quanteda.textplots)

### Read PDFs
dat_raw <- readtext("Commission consultation all/*")

table(dat_raw$doc_id)

dat <- dat_raw |> 
  mutate(text = str_squish(text)) |> # remove unnecessary whitespace
  separate(doc_id, into = c("id", "actor", "type_actor"),
           sep = "_") # create docvars based on file names
# remove PDF ending in document-level variable
dat <- dat |> 
  mutate(type_actor = str_remove_all(type_actor, "\\.pdf"))
# save as RDS file
saveRDS(dat, "data_corpus_aiact.rds")

ai_act <- readRDS("data_corpus_aiact.rds")

corp_aiact <- corpus(ai_act)
corp_aiact


### Do some thing og thang

ntoken(corp_aiact, remove_punct = TRUE)

toks_act <- tokens(corp_aiact, remove_punct = TRUE) %>%
  tokens_remove(stopwords(language = "en"))

dfm_act <- dfm(toks_act)
dfm_act

topfeatures(dfm_act, n = 10)


### --------Wordcloud(?)----
library(wordcloud)
library(RColorBrewer)

corp_aiact %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 15))
### ------------------------


### LSD sentiment ting

dat_dict <- corp_aiact %>%
  tokens() %>%
  tokens_lookup(dictionary = data_dictionary_LSD2015,
                nested_scope = "dictionary") %>%
  dfm() %>%
  convert(to = "data.frame")

dat_dict


### Plot sentiment ting

dat_dict_pl <- dat_dict %>%
  mutate(sentiment = log((positive + neg_positive + 0.5) /
                           (negative + neg_positive + 0.5)))

pl_senti <- ggplot(data = dat_dict_pl,
                   aes(x = sentiment,
                       y = reorder(doc_id, sentiment))) +
  geom_point() +
  labs(x = "Est sentiment",
       y = "Document")

pl_senti


### Positive & negative words

# Positive
corp_aiact %>%
  tokens() %>%
  tokens_keep(pattern = data_dictionary_LSD2015$positive) %>%
  dfm() %>%
  topfeatures(n = 10)

# Negative
corp_aiact %>%
  tokens() %>%
  tokens_keep(pattern = data_dictionary_LSD2015$negative) %>%
  dfm() %>%
  topfeatures(n = 10)

# Positive ord blir oftere brukt. (???)


### Wordfish (?)

tmod_wf <- textmodel_wordfish(dfm_act, dir = c(1,2))
summary(tmod_wf)

textplot_scale1d(tmod_wf, groups = dfm_act$doc_id)
