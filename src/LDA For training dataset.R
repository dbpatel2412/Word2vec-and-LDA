
# Course: Advanced Data Mining

# set output length on R console to print more information.
options(max.print=999999) 


#Problem 1.a
# set working directory 
setwd("E://study metirial//adm//20news-18828")


# Importing all library which will be used in this code

library(e1071)
library(NbClust)
library(cluster)
library(ggplot2)
library(FunCluster)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(factoextra)
library(lsa)
library(topicmodels)
library(caret)

count_files_words <-function(n)
{
  data <-c(n);  
  news <- Corpus(DirSource(data,  encoding = "UTF-8"),readerControl = list(reader=readPlain))  
  news <- tm_map(news, removeWords,"Subject")
  news <- tm_map(news, removeWords,"Organization")   
  news <- tm_map(news, removeWords,"writes")   
  news <- tm_map(news, removeWords,"From")   
  news <- tm_map(news, removeWords,"lines")  
  news <- tm_map(news, removeWords," NNTP-Posting-Host")   
  news <- tm_map(news, removeWords,"article")   
  news <- tm_map(news, tolower)   
  news <- tm_map(news, removeWords, stopwords("english"))   
  news <- tm_map(news, removePunctuation)   
  news <- tm_map(news, stemDocument)   
  news <- tm_map(news, removeNumbers)   
  news <- tm_map(news, stripWhitespace)      
  dtm <- DocumentTermMatrix(news,control=list(wordLengths=c(4,Inf)))  
  Documents <-dim(dtm)[1]   
  Words <-dim(dtm)[2]  
  print(paste(basename(n),Documents,Words,sep = " ")) 
} 
files <- list.files(path="E://study metirial//admproject//TXT", full.names=T, recursive=FALSE)
for(file in files)
{   count_files_words(file) } 


dataset<- c("E://study metirial//admproject//TXT//Chinese",
            "E://study metirial//admproject//TXT//French",
            "E://study metirial//admproject//TXT//India",
            "E://study metirial//admproject//TXT//Italian",
            "E://study metirial//admproject//TXT//Maxican"
          )



# news <- Corpus(DirSource(dataset, recursive=TRUE), readerControl =list(reader=readPlain))
dataset
news <- Corpus(DirSource(dataset, encoding = "UTF-8"), readerControl=list(reader=readPlain,language="en"))




dtmpreproc <- DocumentTermMatrix(news,control=list(wordLengths=c(4,Inf)))
dtmpreproc

#processing the data
news<-tm_map (news, content_transformer(tolower))
news<-tm_map (news, removePunctuation)
news<-tm_map (news, stripWhitespace)
news<-tm_map (news, removeNumbers)


#Transforming data by performing basic actions like removing white spaces , stop words etc.
mystopwords =data(Stopwords_en)
myStopwords<-stopwords('english')
news<-tm_map (news, removeWords,myStopwords)
news<-tm_map (news, stemDocument)

news<-tm_map(news,removeWords,"Subject")
news<-tm_map(news,removeWords,"subject")
news<-tm_map(news,removeWords,"Organization")
news<-tm_map(news,removeWords,"writes")
news<-tm_map(news,removeWords,"Restaurant")
news<-tm_map(news,removeWords,"place")
news<-tm_map(news,removeWords,"good")
news<-tm_map(news,removeWords,"service")
news<-tm_map(news,removeWords,"time")
news<-tm_map(news,removeWords,"From")
news<-tm_map(news,removeWords,"lines")
news<-tm_map(news,removeWords,"NNTP-Posting-Host")
news<-tm_map(news,removeWords,"article")


news<-tm_map (news, content_transformer(tolower))
news<-tm_map (news, removePunctuation)
news<-tm_map (news, stripWhitespace)
news<-tm_map (news, removeNumbers)

#Stop Words: words which do not contain important significance to be used in Search Queries. 
#Usually these words are filtered out from search queries because they return vast amount of unnecessary information

myStopwords<-stopwords('english')
news<-tm_map (news, removeWords,myStopwords)

#stemming : Reduce the count of terms occurring in Document Term matrix which helps to delete the sparse items
#Simplifying them int to single words.
news<-tm_map (news, stemDocument)


#Document Term Matrix
dtmpostproc <- DocumentTermMatrix(news,control=list(wordLengths=c(4,Inf)))
dtmpostproc

# Term Document Matrix
tdmpostproc <- TermDocumentMatrix(news,control=list(wordLengths=c(4,Inf)))
tdmpostproc



#Using TDM to find frequency of words:

tdmspostproc <- removeSparseTerms(tdmpostproc, 0.98)
tdmspostproc
m <- as.matrix(tdmspostproc)
v <- sort(rowSums(m), decreasing=TRUE) 
d <- data.frame(word = names(v),freq=v) 
head(d, 10)


dms <- as.matrix(dtmspostproc)
rownames(dms) <- 1:nrow(dms)

dtmspostproc <- removeSparseTerms(dtmpostproc, 0.98)
dtmspostproc

dtm_tfidf <- weightTfIdf(dtmspostproc)
dtm_tfidf1 <- as.matrix((dtm_tfidf))



freq <- sort(colSums(dms),decreasing = TRUE)
dark2 <- brewer.pal(8, "Dark2")
wordcloud(names(freq), freq, max.words=150, rot.per=0.15,colors=dark2,scale=c(1.5,.3))



#LDA
burnin <- 4000
iter <- 1000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

#Number of Topics 10
k<-10


ldaOut <-LDA(dtmspostproc,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
#write out results
#docs to topics
ldaOut.topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste("E://study metirial//admproject",k,"DocsToTopics10.csv"))
#top 10 terms in each topic
ldaOut.terms <- as.matrix(terms(ldaOut,10))
write.csv(ldaOut.terms,file=paste("E://study metirial//admproject",k,"TopicsToTerms10.csv"))
ldaOut.terms

