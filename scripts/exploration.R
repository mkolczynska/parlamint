# Exploration ofParlaMint.ana 2.0 data for Poland


# 1. Setup -----------
library(tidyverse) # for manipulating data
library(tidytext) # for tidy text analysis
library(udpipe) # for reading conllu files

# network graphs:
library(igraph) 
library(ggraph)
library(tidygraph)

# 2. Reading in the data ----------

conllu <- bind_rows(readRDS("data/ParlaMint-PL.conllu/cleaned/conllu2019.rds"), 
                    readRDS("data/ParlaMint-PL.conllu/cleaned/conllu2020.rds")) %>%
  # correct the lemmatization of "koronawirus"
  mutate(lemma = ifelse( grepl("^koronawirus", lemma), "koronawirus", lemma),
         # create lower case lemmas and tokens
         lemma_lc = tolower(lemma),
         token_lc = tolower(token))


## stop words

stopwords <- read.csv("https://raw.githubusercontent.com/rubenros1795/ParlaMintCase/main/resources/stopwords/pl.csv", 
                      header = FALSE,
                      stringsAsFactors = FALSE,
                      col.names = "word",
                      encoding = "UTF-8")

# 3. Counts ---------------

conllu %>%
  distinct(Speaker_name, Subcorpus) %>%
  count(Subcorpus)

conllu %>%
  distinct(Speaker_name, Term) %>%
  count(Term)

conllu %>%
  distinct(Speaker_name, Speaker_role, Speaker_type, Term) %>%
  count(Term, Speaker_role, Speaker_type)


# regular MPs only

conllu_subset1 <- conllu %>%
  filter(Speaker_role == "Regular",
         Speaker_type == "MP")

conllu_subset1 %>%
  count(doc_id, Speaker_name, Speaker_party, Party_status, Term) %>%
  group_by(Term) %>%
  filter(n == max(n))


conllu_subset1 %>%
  filter( !grepl(";|Prawo", Speaker_party),
          !Speaker_party %in% c("-", "Teraz!")) %>%
  mutate(month = substr(From, 6, 7),
         year = substr(From, 1,4)) %>%
  count(year, month, doc_id, Speaker_name, Speaker_party, Party_status, Term) %>%
  group_by(year, month, Speaker_party, Party_status, Term) %>%
  summarise(ntokens = sum(n)) %>%
  ggplot(., aes(x = month, y = ntokens, fill = Speaker_party)) +
  geom_bar(position="stack", stat="identity") +
  theme_minimal() +
  facet_wrap("year", ncol = 1)

conllu_subset1 %>%
  count(Term, Speaker_party)

# 4. Subset for analysis -----------------

# only regular MPs (excluding chairpersons and guests), without punctuation, 
# and excluding stopwords, symbols, and numbers

conllu_subset2 <- conllu %>%
  filter(Speaker_role == "Regular",
         Speaker_type == "MP",
         !upos %in% c("PUNCT", "SYM", "X"),
         !token_lc %in% stopwords$word)


# 5. Keywords -----------------

count_covid <- conllu_subset2 %>%
  filter(Subcorpus == "COVID") %>%
  count(lemma_lc) %>%
  mutate(nrel = n / nrow(filter(conllu_subset2, Subcorpus == "COVID")) * 1000000)

count_ref <- conllu_subset2 %>%
  filter(Subcorpus == "Reference") %>%
  count(lemma_lc) %>%
  mutate(nrel = n / nrow(filter(conllu_subset2, Subcorpus == "Reference")) * 1000000)

count <- full_join(count_covid, count_ref, by = c("lemma_lc"), suffix = c("_covid", "_ref")) %>%
  mutate(nrel_ref = ifelse(is.na(nrel_ref), 0, nrel_ref),
         nrel_covid = ifelse(is.na(nrel_covid), 0, nrel_covid),
         keyness_ref = (nrel_ref + 1) / (nrel_covid + 1) ,
         keyness_covid = (nrel_covid + 1) / (nrel_ref + 1) )

count %>%
  arrange(desc(keyness_covid)) %>%
  select(lemma_lc, keyness_covid) %>%
  head(10)

count %>%
  arrange(desc(keyness_ref)) %>%
  select(lemma_lc, keyness_ref) %>%
  head(10)



# 6. Collocation networks --------------

collocations <- collocation( filter(conllu_subset2, Subcorpus == "COVID"), 
                            term = "lemma_lc", 
                            group = c("doc_id", "sentence_id"), 
                            ngram_max = 2, n_min = 5, sep = " ")


# graph 1: collocates of "koronawirus"

collocations_koronawirus <- collocations %>%
  filter(freq > 2) %>%
  filter(left == "koronawirus" | right == "koronawirus")

edges_data <- collocations_koronawirus %>% 
  filter(right == "koronawirus") %>%
  select(from = left, to = right, MI = pmi)

v.attr <- collocations_koronawirus %>%
  filter(right == "koronawirus") %>%
  select(word_left = left, word_right = right, freq_left, freq_right) %>%
  pivot_longer(cols = everything(),
               names_to = c(".value", "set"),
               names_sep = "_") %>%
  group_by(set, word) %>%
  summarise(freq = mean(freq)) %>%
  group_by(word) %>%
  summarise(coll_freq = sum(freq) / 100)

ig <- graph_from_data_frame(d=edges_data, vertices=v.attr, directed = FALSE)

tg <- tidygraph::as_tbl_graph(ig) %>% activate(nodes) %>% mutate(label=name)

v.size <- V(tg)$coll_freq

E(tg)$weight <- E(tg)$MI

eigenCent <- evcent(tg)$vector
bins <- unique(quantile(eigenCent, seq(0,1,length.out=30)))
vals <- cut(eigenCent, bins, labels=FALSE, include.lowest=TRUE)
colorVals <- rev(heat.colors(length(bins)))[vals]

tg %>%
  ggraph(layout="stress") +
  geom_edge_diagonal(alpha = .2, color='white') +
  geom_node_point(size=log(v.size)*2, color=colorVals) +
  geom_node_text(aes(label = name, size=log(v.size)+2), repel = TRUE, 
                 point.padding  = unit(0.2, "lines"), 
                 colour='white') +
  theme_graph(background = 'grey20') +
  theme(legend.position = "none")



# graph 2" collocates of "koronawirus" and collocates of collocates

collocations_koronawirus1 <- collocations %>%
  filter(freq > 8) %>%
  filter( left == "koronawirus" | right == "koronawirus")

collocations_koronawirus2 <- collocations %>%
  filter(freq > 70) %>%
  filter(left %in% c(collocations_koronawirus1$left, collocations_koronawirus1$right) |
           right %in% c(collocations_koronawirus1$left, collocations_koronawirus1$right),
         freq_left < 8000,
         freq_right < 8000)

collocations_koronawirus <- bind_rows(collocations_koronawirus1, collocations_koronawirus2)  


edges_data <- collocations_koronawirus %>% 
  select(from = left, to = right, MI = pmi)

v.attr <- collocations_koronawirus %>%
  select(word_left = left, word_right = right, freq_left, freq_right) %>%
  pivot_longer(cols = everything(),
               names_to = c(".value", "set"),
               names_sep = "_") %>%
  group_by(set, word) %>%
  summarise(freq = mean(freq)) %>%
  group_by(word) %>%
  summarise(coll_freq = sum(freq) / 100)

ig <- graph_from_data_frame(d=edges_data, vertices=v.attr, directed = FALSE)

tg <- tidygraph::as_tbl_graph(ig) %>% activate(nodes) %>% mutate(label=name)

v.size <- V(tg)$coll_freq

E(tg)$weight <- E(tg)$MI

eigenCent <- evcent(tg)$vector
bins <- unique(quantile(eigenCent, seq(0,1,length.out=30)))
vals <- cut(eigenCent, bins, labels=FALSE, include.lowest=TRUE)
colorVals <- rev(heat.colors(length(bins)))[vals]

tg %>%
  ggraph(layout="stress") +
  geom_edge_diagonal(alpha = .2, color='white') +
  geom_node_point(size=log(v.size)*2, color=colorVals) +
  geom_node_text(aes(label = name, size=log(v.size+1)), repel = TRUE, 
                 point.padding  = unit(0.2, "lines"), colour='white') +
  theme_graph(background = 'grey20') +
  theme(legend.position = "none")



# 7. Collocations over time ------------

monthly_data_list <- conllu_subset2 %>%
  filter(Subcorpus == "COVID") %>%
  mutate(month = substr(From, 6, 7),
         year = substr(From, 1,4),
         year_month = paste0(year, month)) %>%
  split(., .$year_month)

coll_monthly <- lapply(monthly_data_list, function(x) 
  collocation(x, term = "lemma_lc", group = c("doc_id", "sentence_id"), ngram_max = 2, n_min = 1, sep = " ") 
  )

coll_monthly_df <- bind_rows(coll_monthly, .id = "yearmonth")
                       
coll_monthly_df_koronawirus <- coll_monthly_df %>%
  filter( left == "koronawirus" | right == "koronawirus")

coll_monthly_df_koronawirus %>%
  mutate(keyword = gsub("^koronawirus | koronawirus$", "", keyword)) %>%
  group_by(yearmonth) %>%
  slice_max(order_by = freq, n = 10, with_ties = FALSE) %>%
  select(yearmonth, keyword) %>%
  mutate(rown = row_number()) %>%
  spread(yearmonth, keyword)
