options(warn=-1)

GerarNgram<-function(texto,lingua,ngram){
source("Analise_texto.R")
#source("PreProcessTexto.R")
require(tidytext)
require(dplyr)
require(ggplot2)





Preprocessing=Palavras(texto,lingua)
Preprocessing=Preprocessing[!(Preprocessing  %in% stop_words$word)]
Preprocessing=PreProcessWords(Preprocessing)
frame=data.frame(txt=Preprocessing)
NGrams=frame %>% unnest_tokens(word, txt, token = "ngrams", n = ngram) 
NGrams=NGrams %>%count(word, sort = TRUE)
return(NGrams)
}

PreProcessWords<-function(texto){
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
texto=gsub("/"," ",texto)
texto=gsub("\\|"," ",texto)
texto=gsub("@"," ",texto)
#docs <- Corpus(VectorSource(texto))
#toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
#docs <- tm_map(docs, toSpace, "/")
#docs <- tm_map(docs, toSpace, "@")
#docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
#docs <- tm_map(docs, content_transformer(tolower))
texto=tolower(texto)
# Remove numbers
texto=removeNumbers(texto)

# Remove english common stopwords
#docs <- tm_map(docs, removeWords, stopwords(linguas))
texto=removePunctuation(texto)
# Remove punctuations
#docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
#docs <- tm_map(docs, stripWhitespace)
texto=stripWhitespace(texto)
texto=texto[texto!=""]
texto=texto[texto!=" "]
#texto=stemDocument(texto)
return(texto)

}



GerarGraficoNGram<-function(texto,ngram,lingua,Quantidade){
source("Analise_texto.R")

require(tidytext)
require(dplyr)
require(ggplot2)





Preprocessing=Palavras(texto,lingua)
Preprocessing=Preprocessing[!(Preprocessing  %in% stop_words$word)]
Preprocessing=PreProcessWords(Preprocessing)
frame=data.frame(txt=Preprocessing)
NGrams=frame %>% unnest_tokens(word, txt, token = "ngrams", n = ngram) 
NGrams=NGrams %>%count(word, sort = TRUE)
if(nrow(NGrams)<Quantidade)
	Quantidade=nrow(NGrams)

return(NGrams %>%
slice(1:Quantidade) %>% 
  ggplot() + geom_bar(aes(word, n), stat = "identity", fill = "#de5833") +
  theme_minimal() +
  coord_flip() +
  labs(y="Frequency") )


}

GraphFromWords<-function(texto,lingua,MinFreq){
  
  Frame=GerarNgram(texto,lingua,2)
  
  aux=data.frame(Reduce(rbind,strsplit(Frame$word,split=" ")),Frame$n)
  names(aux)=c('term1','term2','cooc')
  require(igraph)
  require(ggraph)
  
  aux=aux[aux$cooc>=MinFreq,]
  wordnetwork=graph_from_data_frame(aux)
  
  p<-ggraph(wordnetwork, layout = "fr") +
    geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
    geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
    theme_graph(base_family = "Arial Narrow") +
    theme(legend.position = "none") 
  return(list(Viz=p,Graph=aux)  )
  
}

