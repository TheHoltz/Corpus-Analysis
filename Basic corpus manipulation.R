library(jsonlite)
library(dplyr)
library(tm)
library(tidytext)
library(wordcloud)
library(ggplot2)
library(reshape2)


# William Giani
# Wed Oct 10 18:27:52 2018 ------------------------------

json_data <- fromJSON("../Downloads/bipolar_posts_2012.txt")
#_,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,_

#VCORPUS has more compatibility than just Corpus. 
Corpus <- VCorpus(VectorSource(json_data$selftext))

#_,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,_

#Transforming in tidy (It's like a data.frame)
CorpusV2 <- tidy(Corpus, collapse = "\n")

#_,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,_

# Converting phrases to words
CorpusV2 <- CorpusV2 %>%
  unnest_tokens(word, text)

#_,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,_

#importing stop words data
data(stop_words)

#_,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,_

#removing stopwords
CorpusV2 <- CorpusV2 %>%
  anti_join(stop_words)

#_,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,_

#Making a histogram about the more common words

CorpusV2 %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() + labs(title="Palavras mais comuns.")

#_,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,_

#Wordcloud

CorpusV2 %>% anti_join(stop_words) %>% count(word) %>% 
  with(wordcloud(word, n, max.words = 100))

#_,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,_

#Wordcloud by sentimental analysis

CorpusV2 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100,scale=c(3,.5))