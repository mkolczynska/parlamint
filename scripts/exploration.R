# Exploration of ParlaMint.ana 2.0 data from Poland


# 1. Setup -----------
library(tidyverse) # for manipulating data
library(tidytext) # for tidy text analysis
library(udpipe) # for reading conllu files
library(knitr) # for creating report
library(kableExtra) # for creating custom tables
library(lubridate) # for working with dates

# network graphs:
library(igraph) 
library(ggraph)
library(tidygraph)

# set country used in the data file names (ISO2-character country code)
country <- "pl"

# set language to get stopwords list (https://github.com/stopwords-iso/stopwords-iso)
# pl = Polish; lt = Lithuanian; sl = Slovene; cs = Czech; nl = Dutch, Flemish; da = Danish

language <- case_when(
  country == "cz" ~ "cs",
  country == "dk" ~ "da",
  country == "gb" ~ "en",
  country == "nl" ~ "nl",
  country == "lt" ~ "lt",
  country == "pl" ~ "pl",
  country == "si" ~ "sl"
)

# 2. Reading in the data ----------

conllu <- bind_rows(readRDS(paste0("data/cleaned/conllu2019-", country, ".rds")), 
                    readRDS(paste0("data/cleaned/conllu2020-", country, ".rds"))) %>%
  # correct the lemmatization of "koronawirus" in the Polish corpus
  mutate(lemma = ifelse(country == "pl" & grepl("^koronawirus", lemma), "koronawirus", lemma),
         # create lower case lemmas and tokens
         lemma_lc = tolower(lemma),
         token_lc = tolower(token))


## stop words (https://github.com/stopwords-iso/stopwords-iso)

stopwords <- read.delim(paste0("https://raw.githubusercontent.com/stopwords-iso/stopwords-", 
                               language, 
                               "/master/stopwords-", 
                               language, 
                               ".txt"), 
                        header = FALSE,
                        stringsAsFactors = FALSE,
                        col.names = "word",
                        encoding = "UTF-8")

# 3. Counts ---------------

conllu %>%
  summarise(`N tokens` = n(),
            `N sentences` = n_distinct(sentence_id),
            `N speeches` = n_distinct(doc_id),
            `N sittings` = n_distinct(Sitting),
            `N sessions` = n_distinct(Session),
            `N terms` = n_distinct(Term),
            `N speakers` = n_distinct(Speaker_name),
            `min date` = min(From),
            `max date` = max(From)) %>%
  kable(caption = "Summary of data from 2019-2020.") %>% 
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width = F) %>%
  as_image(., file = "plots/summary_n.png")

conllu %>%
  count(upos) %>%
  arrange(desc(n)) %>%
  kable(caption = "Frequency of tokens by part of speech.") %>% 
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width = F) %>%
  as_image(., file = "plots/pos_table.png")

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
  filter(n == max(n)) %>%
  mutate(Term = factor(Term, levels = c("8-lower", "9-upper", "9-lower", "10-upper"))) %>%
  arrange(Term) %>%
  kable(caption = "Longest speeches (according to the number of tokens) by parliamentary house and term.") %>% 
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width = F) %>%
  as_image(., file = "plots/speeches_longest.png")


conllu_subset1 %>%
  distinct(doc_id, Speaker_name, Speaker_party, Party_status, Term) %>%
  count(Term, Speaker_name) %>%
  group_by(Term) %>%
  filter(n == max(n)) %>%
  mutate(Term = factor(Term, levels = c("8-lower", "9-upper", "9-lower", "10-upper"))) %>%
  arrange(Term) %>%
  kable(caption = "Highest numbers of speeches by parliamentary house and term.") %>%
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width = F) %>%
  as_image(., file = "plots/speeches_most.png")



conllu_subset1 %>%
  filter( !upos %in% c("PUNCT", "SYM", "X", "NUM"),
          !grepl(";|Prawo", Speaker_party),
          !Speaker_party %in% c("-", "Teraz!")) %>%
  mutate(month = substr(From, 6, 7),
         month = sub("^0", "", month),
         month = as.numeric(month),
         month = month(ymd(010101) + months(month-1), label=TRUE, abbr=TRUE),
         year = substr(From, 1,4),
         Speaker_party = factor(Speaker_party,
                                c("PiS", "KO", "Lewica", "KP-PSL", "Kukiz'15", 
                                  "Konfederacja", "UPR", "Niezrzeszeni"))) %>%
  count(year, month, Speaker_party, Party_status, Term) %>%
  ggplot(., aes(x = month, y = n / 100000, fill = Speaker_party)) +
  geom_bar(position="stack", stat="identity") +
  theme_minimal() +
  scale_fill_brewer(name = "Party", palette = "Paired") +
  ylab("Words (in 100 thousand)") + xlab("") +
  labs(title = "Number of words spoken by regular MPs per month by party",
       caption = "Note: Excludes punctuation and numbers.
       Data source: Linguistically annotated multilingual comparable corpora of parliamentary debates ParlaMint.ana 2.0. http://hdl.handle.net/11356/1405") +
  facet_wrap("year", ncol = 1)

ggsave("plots/parties_words.png", width = 10, height = 5, units = "in")



conllu_subset1 %>%
  filter( !grepl(";|Prawo", Speaker_party),
          !Speaker_party %in% c("-", "Teraz!")) %>%
  count(From, doc_id, Speaker_name, Speaker_party, Party_status, Term) %>%
  mutate(month = substr(From, 6, 7),
         month = sub("^0", "", month),
         month = as.numeric(month),
         month = month(ymd(010101) + months(month-1), label=TRUE, abbr=TRUE),
         year = substr(From, 1,4),
         Speaker_party = factor(Speaker_party,
                                c("PiS", "KO", "Lewica", "KP-PSL", "Kukiz'15", 
                                  "Konfederacja", "UPR", "Niezrzeszeni"))) %>%
  count(year, month, Speaker_party, Party_status, Term) %>%
  ggplot(., aes(x = month, y = n, fill = Speaker_party)) +
  geom_bar(position="stack", stat="identity") +
  theme_minimal() +
  scale_fill_brewer(name = "Party", palette = "Paired") +
  ylab("Speeches") + xlab("") +
  labs(title = "Number of speeches by regular MPs per month by party",
       caption = "Data source: Linguistically annotated multilingual comparable corpora of parliamentary debates ParlaMint.ana 2.0. http://hdl.handle.net/11356/1405") +
  facet_wrap("year", ncol = 1)

ggsave("plots/parties_speeches.png", width = 10, height = 5, units = "in")


conllu_subset1 %>%
  filter( !grepl(";|Prawo", Speaker_party),
          !Speaker_party %in% c("-", "Teraz!")) %>%
  mutate(month = substr(From, 6, 7),
         month = sub("^0", "", month),
         month = as.numeric(month),
         month = month(ymd(010101) + months(month-1), label=TRUE, abbr=TRUE),
         year = substr(From, 1,4),
         Speaker_party = factor(Speaker_party,
                                c("PiS", "KO", "Lewica", "KP-PSL", "Kukiz'15", 
                                  "Konfederacja", "UPR", "Niezrzeszeni"))) %>%
  group_by(year, month, Speaker_party, Party_status, Term) %>%
  summarise(nspeakers = n_distinct(Speaker_name)) %>%
  ggplot(., aes(x = month, y = nspeakers, fill = Speaker_party)) +
  geom_bar(position="stack", stat="identity") +
  theme_minimal() +
  scale_fill_brewer(name = "Party", palette = "Paired") +
  ylab("Speakers") + xlab("") +
  labs(title = "Number of speakers (regular MPs) per month by party",
       caption = "Data source: Linguistically annotated multilingual comparable corpora of parliamentary debates ParlaMint.ana 2.0. http://hdl.handle.net/11356/1405") +
  facet_wrap("year", ncol = 1)

ggsave("plots/parties_speakers.png", width = 10, height = 5, units = "in")


# 4. Subset for analysis -----------------

# only regular MPs (excluding chairpersons and guests), without punctuation, 
# and excluding stopwords, symbols, and numbers

conllu_subset2 <- conllu %>%
  filter(Speaker_role == "Regular",
         Speaker_type == "MP",
         !upos %in% c("PUNCT", "SYM", "X", "NUM"),
         !token_lc %in% stopwords$word)


# 5. Keywords -----------------

# keyness score formula: https://www.sketchengine.eu/documentation/simple-maths/

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
  # filter out dzwonek (bell), which records the bell sound during speeches
  filter(lemma_lc != "dzwonek") %>%
  mutate(keyness_covid = round(keyness_covid, 1)) %>%
  head(10) %>%
  kable(caption = "Top 10 keywords distinguishing the COVID subcorpus from the Reference subcorpus.") %>%
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width = F) %>%
  as_image(., file = "plots/top10_covid_keywords.png")

count %>%
  arrange(desc(keyness_ref)) %>%
  select(lemma_lc, keyness_ref) %>%
  head(10)



# 6. Collocation networks --------------

collocations <- collocation(filter(conllu_subset2, Subcorpus == "COVID"), 
                            term = "lemma_lc", 
                            group = c("doc_id", "sentence_id"), 
                            ngram_max = 2, n_min = 2, sep = " ")


# graph 1: collocates of "koronawirus"

collocations_koronawirus <- collocations %>%
  filter(freq > 5) %>%
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

tg %>%
  ggraph(layout="stress") +
  geom_edge_diagonal(alpha = .2, color='black') +
  geom_node_point(col = "gray70",size=log(v.size)*2) +
  geom_node_text(aes(label = name), size=log(v.size)+3, repel = TRUE, 
                 point.padding  = unit(0.2, "lines"), 
                 colour='black') +
  theme_graph(background = 'white') +
  theme(legend.position = "none") +
  labs(title = "Collocations for 'koronawirus' in the COVID subcorpus",
       subtitle = "Poland, 2020",
       caption = "Data source: Linguistically annotated multilingual comparable corpora of parliamentary debates ParlaMint.ana 2.0. http://hdl.handle.net/11356/1405")

ggsave("plots/network_koronawirus1.png", width = 10, height = 7, units = "in")



# graph 2" collocates of "koronawirus" and collocates of collocates

collocations_koronawirus1 <- collocations %>%
  filter(freq > 8) %>%
  filter( left == "koronawirus" | right == "koronawirus")

collocations_koronawirus2 <- collocations %>%
  filter(freq > 70) %>%
  filter(left %in% c(collocations_koronawirus1$left, collocations_koronawirus1$right) |
           right %in% c(collocations_koronawirus1$left, collocations_koronawirus1$right),
         freq_left < 7000,
         freq_right < 7000)

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

tg %>%
  ggraph(layout="stress") +
  geom_edge_diagonal(alpha = .2, color='black') +
  geom_node_point(col = "gray70",size=log(v.size)*2) +
  geom_node_text(aes(label = name), size=log(v.size)+3, repel = TRUE, 
                 point.padding  = unit(0.2, "lines"), 
                 colour='black') +
  theme_graph(background = 'white') +
  theme(legend.position = "none") +
  labs(title = "Collocation networks around 'koronawirus' in the COVID subcorpus",
       subtitle = "Poland, 2020",
       caption = "Data source: Linguistically annotated multilingual comparable corpora of parliamentary debates ParlaMint.ana 2.0. http://hdl.handle.net/11356/1405")

ggsave("plots/network_koronawirus2.png", width = 10, height = 7, units = "in")



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
  mutate(rank = row_number()) %>%
  spread(yearmonth, keyword) %>%
  mutate_at(vars(-rank), function(x) cell_spec(x, 
                                               color = ifelse(x == "epidemia", "red", 
                                                              ifelse(x == "pandemia", "blue", 
                                                                     ifelse(x == "walka", "green",
                                                                            ifelse(x == "kryzys", "orange", "black")))))) %>%
  kbl(format = "html", escape = F) %>%
  kable_paper("striped", full_width = F) %>%
  as_image(.,  file = "plots/collocations_months.png")


# 8. Timeline graphs ----------------

conllu_subset1 %>%
  filter(Party_status != "-",
         as.numeric(substr(From, 1,4)) == 2020) %>%
  mutate(covid_terms = ifelse(lemma_lc %in% c("covid", "koronawirus", "epidemia", "pandemia"), 1, 0),
         week = week(From),
         date = ymd( "2020-01-01" ) + weeks(week - 1)) %>%
  group_by(Party_status, date) %>%
  summarise(sum_covid = sum(covid_terms),
            sum_all = n()) %>%
  group_by(date) %>%
  mutate(prop_covid = sum_covid / sum(sum_all)) %>%
  ggplot(., aes(x = date, y = prop_covid, fill = Party_status)) +
  geom_bar(stat = "Identity", position = "dodge") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b-%d") +
  scale_fill_brewer(name = "", palette = "Paired") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  ylab("Relative frequency") + xlab("") +
  labs(title = "Relative frequency of 'koronawirus', 'covid', 'pandemia' and 'epidemia' 
       in speeches by coalition and opposition parties",
       subtitle = "Poland, 2020",
       caption = "Data source: Linguistically annotated multilingual comparable corpora of parliamentary debates ParlaMint.ana 2.0. http://hdl.handle.net/11356/1405")

ggsave("plots/timeline_gov_opp.png", width = 10, height = 4, units = "in")




conllu_subset1 %>%
  filter(Party_status != "-",
         as.numeric(substr(From, 1,4)) == 2020) %>%
  mutate(covid_term = ifelse(lemma_lc %in% c("covid", "koronawirus", "epidemia", "pandemia"), lemma_lc, NA),
         covid_term = factor(covid_term, levels = c("covid", "koronawirus", "epidemia", "pandemia")),
         week = week(From),
         date = ymd( "2020-01-01" ) + weeks(week - 1)) %>%
  group_by(date, covid_term) %>%
  summarise(sum_covid = n()) %>%
  group_by(date) %>%
  mutate(prop_term = sum_covid / sum(sum_covid)) %>%
  drop_na(covid_term) %>%
  ggplot(., aes(x = date, y = prop_term, fill = covid_term)) +
  geom_bar(stat = "Identity", position = "stack") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b-%d") +
  scale_fill_brewer(name = "", palette = "Paired") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  ylab("Relative frequency") + xlab("") +
  labs(title = "Relative frequency of 'koronawirus', 'covid', 'pandemia' and 'epidemia'",
       subtitle = "Poland, 2020",
       caption = "Data source: Linguistically annotated multilingual comparable corpora of parliamentary debates ParlaMint.ana 2.0. http://hdl.handle.net/11356/1405")

ggsave("plots/timeline_4words.png", width = 10, height = 4, units = "in")

