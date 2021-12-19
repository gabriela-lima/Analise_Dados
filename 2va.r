---
title: "VADeaths"
author: "Gabriela Lima"
date: "23/10/2020"
output: html_document
---
# Questões {.tabset}

## VADeaths

```{r}
deaths <- VADeaths

barplot(deaths, beside = T, col = c("blue", "black", "red", "orange", "gray"), main = "VADeaths", xlab = "Tipo", ylab = "Idade", ylim = c(0,75))
legend("topleft", col = c("blue", "black", "red", "orange", "gray"), legend = c("50-54", "55-59", "60-64", "65-69", "70-74"), pch = c(15,15,15,15), cex = 0.8)
```

## ClassificaçãoDoença

```{r}
class <- c("moderado", "leve", "leve", "severo", "leve", "moderado", "moderado", "moderado", "leve", "leve", "severo","leve", "moderado", "moderado", "leve", "severo", "moderado", "moderado", "moderado","leve")

severo <-length(grep("severo", class))
moderado <-length(grep("moderado", class))
leve <-length(grep("leve", class))

classificacao <- c(leve, moderado,severo)
  
percent <- round(classificacao/sum(classificacao)*100)
label <- paste(percent,"%",sep="")

pie(classificacao,labels=label, main="Porcentagem da Gravidade", col = rainbow(3))

legend("topleft",
        legend=c("Leve", "Moderado", "Severo"), fill= rainbow(3))
```

## Twitters

```{r include=FALSE}
consumer_key <- c("EFJch2gsby5h47QKuScPve87I")
consumer_secret <- c("iZWcntBsu65Dd6nMPXZ2yBJRWLvxdMffxte1pAU2XNeHkK7APJ")
access_token <- c("1312481919284776960-ucbunlYUsKZSxlnpLKAu27toEahSgT")
access_secret <-c("goxXZ9wtlrg48VCi9VzNAvHHLocGCshqKM3vqV7EzyOGb")
```


```{r}
library("twitteR")
library("wordcloud")
library("RColorBrewer")
library(tm)
library("syuzhet")
setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)
tweets <- searchTwitter("#racismo", n = 500, lang = "pt-br")
tweets<-twListToDF(tweets)
tweets_t <- paste(tweets$text,collapse= " ")
tweets_S <- VectorSource(tweets_t)
corpus <- Corpus(tweets_S)
corpus <- tm_map(corpus,tolower)
corpus <- tm_map(corpus,removePunctuation)
corpus <- tm_map(corpus,stripWhitespace)
corpus <- tm_map(corpus,removeWords,stopwords('portuguese'))
removeURL<-function(x)    gsub("http[^[:space:]]", "",  x)
corpus <- tm_map(corpus,removeURL)
removeNumPunct <- function(  x)  gsub("[^[:alpha:][:space:]]", "",  x)
corpus <- tm_map(corpus,content_transformer(removeNumPunct))

dtm <- TermDocumentMatrix(corpus)
dtm <- as.matrix(dtm)

fre <-sort(rowSums(dtm),decreasing=TRUE)
wordcloud(corpus, min.freq = 3, max.words = 30, random.order = F,rot.per = 0.1,colors = brewer.pal(8,"Dark2"))


s<- get_nrc_sentiment(tweets_t)
barplot(colSums(s), las=2,col = rainbow(10), ylab = "Quantidade", main = "Analise de Sentimentos ao racismo")


```

## Teorema

```{r}

library(nortest)
flu <- read.csv("D:\\Projetos\\R\\flu.csv")

hist(flu$age, col = rainbow(30), freq = F)
densityPont<-density(flu$age)
lines(densityPont)

n <- 200
tamMedio <- 35
xbar <- rep(NA, n)
for(i in 1:n){
  amostra <- sample(flu$age,size = tamMedio)
  xbar[i] <- mean(amostra)
}
hist(amostra, col = rainbow(30), freq = F)
ad.test(amostra)
densidade2<-density(amostra)
lines(densidade2)


```


```{r}
response <- load("D:\\Projetos\\R\\")
```

