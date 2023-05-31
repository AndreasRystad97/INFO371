library(readtext)
library(tidyverse)
library(quanteda)
library(stopwords)
library(dplyr)
library(ggplot2)
library(tidytext)
library(quanteda.textmodels)
library(quanteda.textplots)
library(keyATM)
library(seededlda)
library(topicmodels)
library(tidyr)
#install.packages("syuzhet")
library(syuzhet)
library(tibble)

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


## Del opp til 2 forskjellige variabler
text1_act <- ai_act[1,]
text1_act

text2_act <- ai_act[2,]
text2_act

## Lag corpuser, av hele, og begge hver for seg

corp_aiact <- corpus(ai_act)
corp_aiact

corp_txt1 <- corpus(text1_act)
corp_txt2 <- corpus(text2_act)


### Tokenize dokumentet og gjør til dfm

ntoken(corp_aiact, remove_punct = TRUE)

toks_act <- tokens(corp_aiact, remove_punct = TRUE) %>%
  tokens_remove(stopwords(language = "en"))

dfm_act <- dfm(toks_act)
dfm_act

topfeatures(dfm_act, n = 10)

# Repeter på de delte dokumentene

txt1_act <- tokens(corp_txt1, remove_punct = TRUE) %>%
  tokens_remove(stopwords(language = "en"))
dfm_txt1 <- dfm(txt1_act)

txt2_act <- tokens(corp_txt2, remove_punct = TRUE) %>%
  tokens_remove(stopwords(language = "en"))
dfm_txt2 <- dfm(txt2_act)


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
dfm_pos <- corp_aiact %>%
  tokens() %>%
  tokens_keep(pattern = data_dictionary_LSD2015$positive) %>%
  dfm()
  
# Negative
dfm_neg <- corp_aiact %>%
  tokens() %>%
  tokens_keep(pattern = data_dictionary_LSD2015$negative) %>%
  dfm()

top_pos <- dfm_pos %>%
  topfeatures(n = 10)

top_neg <- dfm_neg %>%
  topfeatures(n = 10)

# Combined data
combined_results <- list(Positive = as.data.frame(top_pos), Negative = as.data.frame(top_neg))

# Convert combined data to a data frame
df <- bind_rows(lapply(names(combined_results), function(sentiment) {
  tibble(Word = combined_results[[sentiment]]$feature, Count = combined_results[[sentiment]]$frequency, Sentiment = sentiment)
}))

# Bar plot
ggplot(df, aes(x = reorder(Word, Count), y = Count, fill = Sentiment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Word", y = "Count", fill = "Sentiment") +
  theme_minimal() +
  theme(legend.position = "top")

x# Positive ord blir oftere brukt. (???)

bing_pos_neg <- ai_act %>%
  cross_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

### BARPLOT KANSKJE
faens_tekst1 <- corp_txt1 %>%
  tokens() %>%
  tokens_keep(pattern = data_dictionary_LSD2015) %>%
  dfm()

df_tekst1 <- convert(faens_tekst1, to = "data.frame")
df_tekst1


ggplot(df_tekst1, aes(x = words, y = count)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Words", y = "Count", fill = "Sentiment") +
  scale_fill_manual(values = c("positive" = "green", "negative" = "red")) +
  theme_minimal() +
  theme(legend.position = "top")


### faenskap
bing_pos_neg %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>%
  ungroup() %>%
  filter(row_number() <= 20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = n, y = word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment", y = NULL)


### Wordfish

tmod_wf <- textmodel_wordfish(dfm_act, dir = c(1,2))
summary(tmod_wf)

textplot_scale1d(tmod_wf, groups = dfm_act$doc_id)

textplot_scale1d(tmod_wf, margin = "features",
                 highlighted = c("ai", "risk", "criminal", "harm",
                                 "rights", "compliance", "ensure",
                                 "world", "appropriations", "budget",
                                 "deleted", "year", "regulation",
                                 "multiannual", "sensetive", "paper",
                                 "0.760", "machine", "prospective",
                                 "22"))

# Estimated theta = estimated document positions
# Estimated beta = estimated feature marginal effects
# Estimated psi = estimated word fixed effects.



### Keywords in context (?) / multiword expressions


### Clustering

