# script to read .conllu and metadata files from the ParlaMint.ana 2.0 data


# 1. Setup -----------
library(tidyverse) # for manipulating data
library(tidytext) # for tidy text analysis
library(udpipe) # for reading conllu files


# set country used in the data file names (ISO2-character country code)
country <- "dk"

# set language to get stopwords list
# pl = Polish; lt = Lithuanian; sl = Slovene; cs = Czech; nl = Dutch, Flemish; da = Danish
language <- "da"

# path to folder (may need adjusting)
path <- paste0("data/ParlaMint-", country, ".conllu")

# 2. Metadata files ----------------

## 2020

# list files from path and subfolders
temp <- list.files(path = path, pattern = ".tsv$", recursive = TRUE)
#keep only files where names include 2020
temp <- temp[grepl("2020", temp)]
# create file paths
f <- file.path(path, temp)

# read the from all files to a list
meta <- lapply(f, read.delim, stringsAsFactors = FALSE, colClasses = "character", encoding = "UTF-8")

# combine rows of the list into a data frame
meta2020 <- bind_rows(meta) %>%
  # in Poland, one MP has no birth year https://pl.wikipedia.org/wiki/Waldemar_Bonkowski
  mutate(Speaker_birth = ifelse(Speaker_name == "Bonkowski, Waldemar", 1959, Speaker_birth))

# save to file
saveRDS(meta2020, paste0("data/cleaned/metadata2020-", country, ".rds"))


## 2019

# path to folder
temp <- list.files(path = path, pattern = ".tsv$", recursive = TRUE)
temp <- temp[grepl("2019", temp)]

f <- file.path(path, temp)
meta <- lapply(f, read.delim, stringsAsFactors = FALSE, colClasses = "character", encoding = "UTF-8")
meta2019 <- bind_rows(meta)

saveRDS(meta2019, paste0("data/cleaned/metadata2019-", country, ".rds"))




# 2. conllu files ----------------

# 2020

temp <- list.files(path = path, pattern = ".conllu$", recursive = TRUE)
temp <- temp[grepl("2020", temp)]
f <- file.path(path, temp)

conllu <- lapply(f, udpipe_read_conllu)
conllu2020 <- bind_rows(conllu) %>% 
  left_join(meta2020, by = c("doc_id" = "ID"))

saveRDS(conllu2020, paste0("data/cleaned/conllu2020-", country, ".rds"))


# 2019

temp <- list.files(path = path, pattern = ".conllu$", recursive = TRUE)
temp <- temp[grepl("2019", temp)]
f <- file.path(path, temp)

conllu <- lapply(f, udpipe_read_conllu)
conllu2019 <- bind_rows(conllu) %>% 
  left_join(meta2019, by = c("doc_id" = "ID"))

saveRDS(conllu2019, paste0("data/cleaned/conllu2019-", country, ".rds"))



# 3. Subset examples -----------------

# only regular MPs (excluding chairpersons and guests) and without punctuation
conllu2020_subset <- conllu2020 %>%
  filter(Speaker_role == "Regular",
         Speaker_type == "MP",
         upos != "PUNCT")



# stopwords
stopwords <- read.delim(paste0("https://raw.githubusercontent.com/stopwords-iso/stopwords-", 
                               language, 
                               "/master/stopwords-", 
                               language, 
                               ".txt"), 
                        header = FALSE,
                        stringsAsFactors = FALSE,
                        col.names = "word",
                        encoding = "UTF-8")

# keep only regular MPs, exclude punctuation, symbols, numbers, and stopwords
conllu2020_subset2 <- conllu2020 %>%
  mutate(token_lc = tolower(token)) %>%
  filter(Speaker_role == "Regular",
         Speaker_type == "MP",
         !upos %in% c("PUNCT", "SYM", "X", "NUM"),
         !token_lc %in% stopwords$word)


nrow(conllu2020)
nrow(conllu2020_subset)
nrow(conllu2020_subset2)
