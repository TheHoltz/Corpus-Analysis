Grafo <- Corpus %>%
  count_bigrams()

dados <- Grafo %>% filter(n > 30)

require(foreach);

degree_ <- function(Novos) {

  #Esse for irá calcular a ocorrencia de cada palavra nos bigramas,
  #dessa forma posso extrair o grau do nó.
  out <- foreach(i=1:length(unique(Novos$word1)), .combine='rbind') %do% {
    data.frame("Grau"=stri_count_regex(Novos, unique(Novos$word1)[i]) %>% sum(),
          "Nó"=unique(Novos$word1)[i])
  } %>% filter(complete.cases(.)) #Esse filtro é para remover qualquer NA.
  
  return(out);
  
}

Graus <- degree_(Novos)



