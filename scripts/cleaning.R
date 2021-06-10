# script to read .conllu and metadata files from the ParlaMint.ana 2.0 data for Poland for 2019 and 2020


# 1. Setup -----------
library(tidyverse) # for manipulating data
library(tidytext) # for tidy text analysis
library(udpipe) # for reading conllu files


# 2. Metadata files ----------------

## 2020

# path to folder
path <- "data/ParlaMint-PL.conllu"
temp <- list.files(path = path, pattern = ".tsv$", recursive = TRUE)
temp <- temp[grepl("2020", temp)]

f <- file.path(path, temp)

meta <- lapply(f, read.delim, stringsAsFactors = FALSE, colClasses = "character", encoding = "UTF-8")

meta2020 <- bind_rows(meta) %>%
  # https://pl.wikipedia.org/wiki/Waldemar_Bonkowski
  mutate(Speaker_birth = ifelse(Speaker_name == "Bonkowski, Waldemar", 1959, Speaker_birth))

saveRDS(meta2020, "data/ParlaMint-PL.conllu/cleaned/metadata2020.rds")


## 2019

# path to folder
temp <- list.files(path = path, pattern = ".tsv$", recursive = TRUE)
temp <- temp[grepl("2019", temp)]

f <- file.path(path, temp)

meta <- lapply(f, read.delim, stringsAsFactors = FALSE, colClasses = "character", encoding = "UTF-8")

meta2019 <- bind_rows(meta)

saveRDS(meta2019, "data/ParlaMint-PL.conllu/cleaned/metadata2019.rds")


# 2. conllu files ----------------

# 2020

temp <- list.files(path = path, pattern = ".conllu$", recursive = TRUE)
temp <- temp[grepl("2020", temp)]

f <- file.path(path, temp)

pl_conllu <- lapply(f, udpipe_read_conllu)

conllu2020 <- bind_rows(pl_conllu) %>% 
  left_join(meta2020, by = c("doc_id" = "ID"))

saveRDS(conllu2020, "data/ParlaMint-PL.conllu/cleaned/conllu2020.rds")


# 2019

temp <- list.files(path = path, pattern = ".conllu$", recursive = TRUE)
temp <- temp[grepl("2019", temp)]

f <- file.path(path, temp)

pl_conllu <- lapply(f, udpipe_read_conllu)

conllu2019 <- bind_rows(pl_conllu) %>% 
  left_join(meta2019, by = c("doc_id" = "ID"))

saveRDS(conllu2019, "data/ParlaMint-PL.conllu/cleaned/conllu2019.rds")



# 3. Subsets -----------------

# only regular MPs (excluding chairpersons and guests) and without punctuation
conllu2020_subset <- conllu2020 %>%
  filter(Speaker_role == "Regular",
         Speaker_type == "MP",
         upos != "PUNCT")

# this reduced the number of tokens by 47%
1 - nrow(conllu2020_subset) / nrow(conllu2020)
