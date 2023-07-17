#task2
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
#
tdm.tfidf <- weightTfIdf(tdm)
tdm.tfidf <- removeSparseTerms(tdm.tfidf, 0.999)
tfidf.matrix <- as.matrix(tdm.tfidf)
#
library(proxy)
dist.matrix <- dist(tfidf.matrix, method = "cosine")

truth.K=5
#Perform clustering
library(dbscan)
clustering.kmeans <- kmeans(tfidf.matrix, truth.K) 
clustering.hierarchical <- hclust(dist.matrix, method = "ward.D2") 
clustering.dbscan <- hdbscan(dist.matrix, minPts = 10)

library(cluster)
clusplot(as.matrix(dist.matrix),clustering.kmeans$cluster,color=T,shade=T,labels=2,lines=0)
plot(clustering.hierarchical)
rect.hclust(clustering.hierarchical,2)
plot(as.matrix(dist.matrix),col=clustering.dbscan$cluster+1L)
#d
library(colorspace)
points <- cmdscale(dist.matrix, k = 5) 
palette <- diverge_hcl(truth.K) # Creating a color palette, need library(colorspace)
#layout(matrix(1:3,ncol=1))

plot(points, main = 'K-Means clustering', col = as.factor(master.cluster), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
plot(points, main = 'Hierarchical clustering', col = as.factor(slave.hierarchical), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),  
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
plot(points, main = 'Density-based clustering', col = as.factor(slave.dbscan), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 

table(master.cluster)
table(slave.hierarchical)
table(slave.dbscan)
