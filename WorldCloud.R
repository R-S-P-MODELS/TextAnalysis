NuvemPalavras<-function(texto,linguas){
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

docs <- Corpus(VectorSource(texto))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords(linguas))
docs <- tm_map(docs, removeWords, c('https','tco'))

# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
x <- data.frame(word = names(v),freq=v)

wordcloud(words = x[,1], freq = x[,2], min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

}



NuvemComparativa<-function(texto,texto2,linguas){
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

docs <- Corpus(VectorSource(texto))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords(linguas))
docs <- tm_map(docs, removeWords, c('https','tco'))

# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
x <- data.frame(word = names(v),freq=v)


docs <- Corpus(VectorSource(texto2))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords(linguas))
docs <- tm_map(docs, removeWords, c('https','tco'))

# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
x1 <- data.frame(word = names(v),freq=v)

print(x$word)
nomes=c(as.character(x$word),as.character(x1$word) )
print(nomes)
nomes=unique(nomes)



MatrizComp=matrix(0,length(nomes),2)
rownames(MatrizComp)=nomes
for(i in nomes){
Indice1=which(x$word==i)
Indice2=which(x1$word==i)
if(length(Indice1)>0){
	Indice1=max(Indice1)
	MatrizComp[i,1]=x[Indice1,2]
}
if(length(Indice2)>0){
	Indice2=max(Indice2)
	MatrizComp[i,2]=x[Indice2,2]
	}
}
print(MatrizComp)
print("Vai rodar a cloud")

Lista=apply(apply(MatrizComp,2,is.na),2,which)
if(length(Lista)>0){

	for(i in 1:length(Lista)){
		MatrizComp[Lista[[i]],i]=0
	}
}
comparison.cloud(MatrizComp,max.words=100,random.order=FALSE,
		match.colors=TRUE)

}



Sentimento<-function(a,block=1,linguagem="en"){
#library(janeaustenr)
library(dplyr)
library(stringr)
library(tidytext)

tidy_books <-data.frame(a,a,1:length(a))
names(tidy_books)=c("book","word","linenumber")

library(tidyr)

if(linguagem=="en")
  Lexicon=get_sentiments("bing")
else if(linguagem=="pt")
  Lexicon=read.csv("LexiconPortuguesPositivevsNegative.csv",header=TRUE)

jane_austen_sentiment <- tidy_books %>%
  inner_join(Lexicon) %>%
  count(book, index = linenumber %/% block, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

return(jane_austen_sentiment)
}

NuvemSentimentos<-function(texto,lingua){
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
Palavras=stripWhitespace(removePunctuation(removeNumbers(removeWords(texto,stopwords(lingua)))))
Sentimentos=Sentimento(Palavras,1,lingua)
Frequencias=plyr::count(Sentimentos$book)
vec=c()
for(i in 1:length(Frequencias$x) )
	vec[i]=Sentimentos$sentiment[min(which(Frequencias$x[i]==Sentimentos$book) ) ] * Frequencias$freq[i]
PalavrasNegativas=data.frame(Frequencias$x[which(vec<0)],Frequencias$freq[which(vec<0)],0 )

PalavrasPositivas=data.frame(Frequencias$x[which(vec>0)],0,Frequencias$freq[which(vec>0)] )
names(PalavrasPositivas)=names(PalavrasNegativas)=c('Palavra','Negative','Positive')
PalavrasFinais=rbind(PalavrasNegativas,PalavrasPositivas)
MatrizRepresentativa=as.matrix(PalavrasFinais[,2:3])
rownames(MatrizRepresentativa)=PalavrasFinais$Palavra
comparison.cloud(MatrizRepresentativa,max.words=100,random.order=FALSE,
		match.colors=TRUE)

}
