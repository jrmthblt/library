#Cleanup
#rm(list = ls())

#getwd()

## INIT

#columns2keep
#c2k <- c("volumeInfo.title",
#          "volumeInfo.publisher",
#          "volumeInfo.description",
#          "volumeInfo.categories",
#          "volumeInfo.language",
#          "volumeInfo.subtitles",
#          "volumeInfo.authors")

infile <- "dataset.csv"
dataset_df <- read.csv2(infile, header=TRUE, sep=";")

dataset_df <- dataset_df[-c(29,32,37,39,171,1158,1182,1597,1623),] # Nettoyage de certains enregistrements problématiques

## Prepare DTM
#install.packages('tm')
#install.packages('stringi')
#install.packages('stringr')
#install.packages('wordcloud')
library(tm)
library(stringi)
library(stringr)
library(wordcloud)

#Initial corpus
documents <- Corpus(VectorSource(dataset_df$volumeInfo.description))

#Remove chars with accents
accent <- function(x) stri_trans_general(x, "Latin-ASCII") # cela signifie qu'on remplace un caractère encodé en Latin1 par son équivalent le plus proche en ASCII, il n'y a par exemple pas de caractères accentués en ASCII
documents <- tm_map(documents, content_transformer(accent))

#Remove numbers and punctuation
documents <- tm_map(documents, content_transformer(gsub), pattern = "[^a-zA-Z]", replacement = " ")

#Put in lowercase
documents <- tm_map(documents, content_transformer(tolower))

#Stopwords removal
#stopwords_fr <- stopwords("french")
stopwords_fr <- sapply(stopwords("french"),accent)
documents <- tm_map(documents, removeWords, stopwords_fr)

stopwords_fr2 = c(stopwords_fr,'a','h','lundi','mardi','mercredi','jeudi','vendredi','samedi','dimanche',
                 'etre','apres','selon','comme','alors','tout','tous','faire','depuis','encore',
                 'peut','doit','mieux',"un","deux","trois","quatre","cinq","six","sept","huit","neuf","dix",
                 'tant','ainsi','livre','oeuvre','ouvrage','aussi','autre','fait','entre','plus','tout',
                 'toute','auteur','bien','dont','roman','comment','petit','petite','grand','grande',
                 'etc','annee','aujourd','hui','tres','seul','seule','autre','autres','celle','dont','donc',
                 'donne','sous','jusqu','quelqu','nombreux','propose','part','parti','partir','jamais',
                 'grands','grandes','question','fois','france','travers','jour'
                 )
stopwords_fr2 = setdiff(stopwords_fr2, c("pas")) # 'pas' est inclus dans les stopword, on trouve que c'est un peu dommage alors on le retire de la liste
documents <- tm_map(documents, removeWords, stopwords_fr2)


#Stemming
#install.packages('SnowballC')
library('SnowballC')
documents_nonstem <- documents
documents <- tm_map(documents, stemDocument, "french")

#Strip whitespaces
documents <- tm_map(documents, stripWhitespace) #n'enleve pas le tout premier espace
documents <- tm_map(documents, content_transformer(gsub), pattern = "^\\s+", replacement = "")

#DTM
dtm <- DocumentTermMatrix(documents)

#Elagage (minimum 20 mots)
minfreq <- findFreqTerms(dtm, 20)
length(minfreq)

dtm <- removeSparseTerms(
  DocumentTermMatrix(documents, control=list(dictionary= minfreq, weighting = function(x) weightTfIdf(x, normalize = TRUE)))
  ,
  0.89)

dtm2 <- removeSparseTerms(
  DocumentTermMatrix(documents, control=list(dictionary= minfreq, weighting = function(x) weightTf(x)))
  ,
  0.89)


### Statistics
#inspect(dtm)
#dim(dtm)
#documents[1]$content
#documents_nonstem[942]$content


rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ] #remove all docs without words

rowTotals2 <- apply(dtm2 , 1, sum) #Find the sum of words in each Document
dtm.new2   <- dtm2[rowTotals> 0, ] #remove all docs without words


findMostFreqTerms(dtm.new, n=20L)

m <- as.matrix(as.TermDocumentMatrix(dtm.new))
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 30)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE,  
          colors=brewer.pal(8, "Dark2"))



### Clustering
#install.packages('skmeans')
library(skmeans)
library(cluster)
#install.packages('proxy')
library(proxy)


#http://michael.hahsler.net/SMU/CSE7337/ et http://michael.hahsler.net/SMU/CSE7337/install/tm.R
#http://edutechwiki.unige.ch/fr/Tutoriel_tm_text_mining_package
#https://stackoverflow.com/questions/13944252/remove-empty-documents-from-documenttermmatrix-in-r-topicmodels
#https://stackoverflow.com/questions/24459246/text-analysis-using-lda-and-tm-in-r
#

### don't forget to normalize the vectors so Euclidean makes sense
norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
m_norm <- norm_eucl(as.matrix(dtm.new))

set.seed(123)

#Silouhette method for finding the optimal number of clusters

# Compute the average silhouette width for 
# k = 2 to k = 20 (skmeans)
k.max <- 20
sil <- rep(0,k.max)
for(i in 2:k.max){
  set.seed(123)
  k.res <- skmeans(dtm.new, i)
  ss <- silhouette(k.res$cluster, proxy::dist(as.matrix(dtm.new), method="cosine"))
  sil[i] <- mean(ss[, 3])
}

# Plot the average silhouette width (skmeans)
plot(1:k.max, sil, type = "b", pch = 19, 
     frame = FALSE, xlab = "Number of clusters k")
abline(v = which.max(sil), lty = 2)


# Compute the average silhouette width for 
# k = 2 to k = 20 (kmeans)
k.max <- 20
sil2 <- rep(0,k.max)
for(i in 2:k.max){
  set.seed(123)
  k.res2 <- kmeans(m_norm, i, nstart=50,iter.max = 15)
  ss2 <- silhouette(k.res2$cluster, dist(m_norm))
  sil2[i] <- mean(ss2[, 3])
}

# Plot the average silhouette width (kmeans)
plot(1:k.max, sil2, type = "b", pch = 19, 
     frame = FALSE, xlab = "Number of clusters k")
abline(v = which.max(sil2), lty = 2)


# cluster into x (optimal) clusters - skmeans
set.seed(123)
cl <- skmeans(dtm.new, 13)
table(cl$cluster)
# show clusters using the first 2 principal components
plot(prcomp(m_norm)$x, col=cl$cluster)


# cluster into x (optimal) clusters - kmeans
set.seed(123)
cl2 <- kmeans(m_norm, 13)
table(cl2$cluster)
# show clusters using the first 2 principal components
plot(prcomp(m_norm)$x, col=cl2$cluster)


## Self organizing map (som) - Kohonen map
# https://www.r-bloggers.com/self-organising-maps-for-customer-segmentation-using-r/
# https://www.shanelynn.ie/self-organising-maps-for-customer-segmentation-using-r/
# https://clarkdatalabs.github.io/soms/SOM_Shakespeare_Part_1
#install.packages('kohonen')
library(kohonen)

set.seed(123)
data_train_matrix <- as.matrix(scale(m_norm))
# Create the SOM Grid - you generally have to specify the size of the 
# training grid prior to training the SOM. Hexagonal and Circular 
# topologies are possible
som_grid <- somgrid(xdim = 6, ydim=6, topo="hexagonal")
# Finally, train the SOM, options for the number of iterations,
# the learning rates, and the neighbourhood are available
som_model <- som(data_train_matrix, 
                 grid=som_grid, 
                 rlen=2000, 
                 alpha=c(0.05,0.01), 
                 keep.data = TRUE)

plot(som_model, type="changes")
plot(som_model, type="count", main="Node Counts")
#plot(som_model, type="dist.neighbours", main = "SOM neighbour distances")


### it seems the optimal number of cluster is more or less the number of remaining words in DTM... ###
### Instability of the clustering with many tries ###



### Classification

#install.packages('caret')
library(caret)



## Write final dataset csv
#write.csv2(tidy_df, file=paste0("dataset",".csv"), quote = TRUE)



