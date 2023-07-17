#task1
library(tm)
docs<-Corpus(DirSource("C:/Users/93217/Desktop/materials"))

#Data preprocessing

docs <- tm_map(docs,content_transformer(tolower)) #Transform to lower case
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))}) #remove potentially problematic symbols


docs <- tm_map(docs, removePunctuation) #remove punctuation
docs <- tm_map(docs, removeNumbers) #Strip digits
docs <- tm_map(docs, removeWords, stopwords("english")) #remove stopwords
docs <- tm_map(docs, stripWhitespace) #remove whitespace

myStopwords <- c("said","will","can","must","also")
docs <- tm_map(docs, removeWords, myStopwords) #remove custom stopwords

tdm <- DocumentTermMatrix(docs) #Create document term matrix
tdm

set.seed(1234)
library(tidytext)
library(topicmodels)
library(tidyr)
library(ggplot2)
library(dplyr)
ap_lda<-LDA(tdm,k=4,control=list(seed=1234)) #create two-topic LDA model
ap_topics<-tidy(ap_lda,matrix="beta") 
ap_top_terms <- ap_topics %>% group_by(topic) %>% top_n(10,beta) %>% ungroup () %>% arrange (topic, -beta)
ap_top_terms%>% mutate(term=reorder(term,beta))%>% 
  ggplot(aes(term,beta,fill=factor(topic)))+geom_col(show.legend=FALSE)+
  facet_wrap(~topic,scales="free")+coord_flip() 
beta_spread <- ap_topics %>% mutate (topic=paste0("topic",topic)) %>% spread(topic,beta) %>%
  filter (topic1>0.005 | topic2 > 0.005) %>% mutate(log_ratio = log2(topic2/topic1))
beta_spread%>% mutate(term=reorder(term,log_ratio))%>% 
  ggplot(aes(term,log_ratio))+geom_col(show.legend=FALSE)+coord_flip()

ap_documents<-tidy(ap_lda,matrix="gamma") #Extract the per-document-per-topic-probabilities
ap_documents

findAssocs(tdm, terms = c("success"), corlimit = 0.75)
