library(dplyr)
library(tidyr)
library(tidytext)
library(igraph)
library(ggraph)
library(stringr)
library(ggplot2)
library(stringr)
source("https://pastebin.com/raw/3ZKsa7gZ") #multiplot function
#library(jsonlite)

#William Giani
# Wed Oct 10 19:19:11 2018 ------------------------------

#_,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,_

#This functions does all the dirty job.
#And you just gonna need to inform the corpus at the end.
count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}

visualize_bigrams <- function(bigrams) {
  set.seed(2018)
  a <- grid::arrow(type = "closed", length = unit(.09, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "kk") +
    geom_edge_link(aes(color = -n), show.legend = FALSE, arrow = a,) +
    geom_node_point(color = "dodgerblue3", size = 3) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}

#_,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,_

#Importing Corpus
json_data <- fromJSON("../Downloads/bipolar_posts_2012.txt")

Corpus <- VCorpus(VectorSource(json_data$selftext))

#_,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,_

#Converting Corpus to tidy (It's like a data.frame)
CorpusV2 <- tidy(Corpus, collapse = "\n")

#_,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,_

#Generating graphs

Grafo <- CorpusV2 %>%
  count_bigrams()

Grafo %>%
  filter(n > 20, #choose the frequency filter
         !str_detect(word1, "\\d"),
         !str_detect(word2, "\\d")) %>%
  visualize_bigrams()

#_,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,_

#Sentimental analysis
AFINN <- get_sentiments("afinn")

#Splitting the Corpus
bigramas <- CorpusV2 %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigramas_separados <- bigramas %>%
  separate(bigram, c("word1", "word2"), sep = " ")

#_,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,_

not_words <- bigramas_separados %>%
  filter(word1 == "feeling") %>% #change to word of your prefference
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE) %>%
  ungroup()

p1 <- not_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Palavras precedidas de \"feeling\"") + #change here too
  ylab("Sentiment score * numero de ocorrencias") +
  coord_flip()

not_words <- bigramas_separados %>%
  filter(word1 == "like") %>% #change to word of your prefference
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE) %>%
  ungroup()

p2 <- not_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Palavras precedidas de \"like\"") +  #change here too
  ylab("Sentiment score * numero de ocorrencias") +
  coord_flip()

#_,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,_

#Plotting the results

multiplot(p1,p2, cols = 2)

#_,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,_
