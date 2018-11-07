library(jsonlite)
library(tm)
library(dplyr)
library(tidyverse)
library(stringi)
library(tidytext)
library(foreach)

#_,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,_

json_data <- fromJSON("../Downloads/bipolar_posts_2012.txt")
json_data2 <- fromJSON("../Downloads/bipolar_comments_2012.txt")

#_,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,_

#Função extraída do livro:
#Julia Silge, David Robinson - Text Mining with R_ A Tidy Approach (2017, O’Reilly Media)

count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort=T)
}

#_,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,_

BigramasCorpusPosts <- VCorpus(VectorSource(json_data$selftext)) %>% 
  tidy(., collapse = "\n") %>% count_bigrams()

BigramasCorpusComentarios <- VCorpus(VectorSource(json_data2$body)) %>% 
  tidy(., collapse = "\n") %>% count_bigrams()

#_,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,_  

dim(BigramasCorpusPosts) #30668 
dim(BigramasCorpusComentarios) #93673

Posts <- BigramasCorpusPosts %>% filter(n > 5)
Comentarios <- BigramasCorpusComentarios %>% filter(n > 20)

dim(Posts) #71
dim(Comentarios) #299

#_,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,_

degree_ <- function(Novos) {

  #Esse for irá calcular a ocorrencia de cada palavra nos bigramas,
  #dessa forma posso extrair o grau do nó.
  n <- length(unique(Novos$word1))
  vec1 <- integer(length = n)
  
  cat("Parte [1/2] Iniciando cálculos..\n")
  foreach(i=1:n, .combine='c') %do% {
    vec1[i] <- Novos[Novos$word1 == unique(Novos$word1)[i],] %>% dim() %>% .[1]-1
    cat('Progresso',i/n,"\n")}
  
  cat("Parte [2/2] Iniciando cálculos..\n")
  Sys.sleep(0.5)
  vec2 <- character(length = n)
  
  foreach(i=1:n, .combine='c') %do% {
          vec2[i] <- unique(Novos$word1)[i]
          cat('Progresso',i/n,"\n")
          }
  
  out <- drop_na(tibble("Grau"=vec1, "Nó"=vec2))
  
  return(out)
  
}

#_,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,_

GrausP <- degree_(BigramasCorpusPosts)
GrausC <- degree_(BigramasCorpusComentarios)

hist(GrausP$Grau, breaks = 20)
hist(GrausC$Grau, breaks = 20)

qplot(y=GrausP$Grau, x=c(1:length(GrausP$Grau))) + geom_smooth() +
  labs(x="", y="Grau", title="Distribuição graus dos nós [POSTS]")

qplot(y=GrausC$Grau, x=c(1:length(GrausC$Grau))) + geom_smooth() +
  labs(x="", y="Grau", title="Distribuição graus dos nós [COMM.]")



