library(readtext)
library(quanteda)
library(tidyverse)

# load PDF files (I had to adjust five filenames manually)
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
