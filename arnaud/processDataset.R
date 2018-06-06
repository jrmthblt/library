#Cleanup
#rm(list = ls())

#getwd()

## INIT

#install.packages('tm')
#install.packages('stringi')
#install.packages('stringr')
#install.packages('wordcloud')
library(tm)
library(stringi)
library(stringr)
library(wordcloud)
#install.packages('SnowballC')
library('SnowballC')

#install.packages('skmeans')
library(skmeans)
library(cluster)
#install.packages('proxy')
library(proxy)

#install.packages('kohonen')
library(kohonen)

#columns2keep
#c2k <- c("volumeInfo.title",
#          "volumeInfo.publisher",
#          "volumeInfo.description",
#          "volumeInfo.categories",
#          "volumeInfo.language",
#          "volumeInfo.subtitles",
#          "volumeInfo.authors")

infile <- "dataset.csv"
dataset_df <- read.csv2(infile, header=TRUE, sep=";", stringsAsFactors = FALSE)

dataset_df <- dataset_df[-c(29,32,37,39,171,1158,1182,1597,1623),] # suppression de certains enregistrements de mauvaise qualité
dataset_df <- dataset_df[-c(1139,116,1179,1181,14,1577,1651,206,23,86)] # suppression des enregistrements contenant des mots anglais
#dataset_df2 <- dataset_df[,c(1,4)]
#names(dataset_df2)[1]<-"doc_id"
#names(dataset_df2)[2]<-"text"

## Prepare DTM

#Initial corpus
documents <- Corpus(VectorSource(dataset_df$volumeInfo.description))
#documents <- Corpus(DataframeSource(dataset_df2))


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
                  'oui','non','apres','selon','comme','alors','tout','tous','faire','depuis','encore',
                  'peut','doit','mieux','un','deux','trois','quatre','cinq','six','sept','huit','neuf','dix',
                  'tant','ainsi','livre','oeuvre','aussi','fait','entre','plus','moins','toute','donc',
                  'toutes','auteur','bien','roman','comment','petit','petite','grand','grande','ceux',
                  'etc','annee','aujourd','hui','tres','seul','seule','autre','autres','celle','donc','dont',
                  'donne','sous','sur','jusqu','quelqu','nombreux','propose','part','parti','partir','jamais',
                  'car','grands','grandes','question','fois','france','travers','jour','avant','apres',
                  'il','elle','ils','elles','tel','tels','telle','telles','quelque','quelques','toujours',
                  'souvent','chez','celui','chaque','mis','mise','texte','moindre','peu','ouvrage','rien',
                  'pourtant','texte','the','certains','certaines','lecteur','vers','parfois','grace','aime',
                  'aimer','enfin','chacun','chacune','pourquoi','propos','plusieurs','avoir','etre','contre',
                  'mais','faut','puis','notre','votre','seule','seules','seulement','note','noter','edition',
                  'les','d','l','c','resume','science-fiction','ou','tome','serie','roman','romans','collection',
                  'livre','gerard','dirigee','prix','king','auteur','stephen','histoire','etait','meme',
                  'recueil','hugo','quinze'
)


stopwords_fr2 = setdiff(stopwords_fr2, c("pas")) # 'pas' est inclus dans les stopword, on trouve que c'est un peu dommage alors on le retire de la liste
documents <- tm_map(documents, removeWords, stopwords_fr2)


#Stemming
documents_nonstem <- documents
documents <- tm_map(documents, stemDocument, "french")

#Strip whitespaces
documents <- tm_map(documents, stripWhitespace) # n'enleve pas le tout premier espace
documents <- tm_map(documents, content_transformer(gsub), pattern = "^\\s+", replacement = "")


#DTM
dtm.prev <- DocumentTermMatrix(documents)
clength <- apply(dtm.prev, 1, sum) # somme des mots dans chaque document

row2keep <- dtm.prev[clength >= 15, ]$dimnames[1][[1]] # on cible les lignes à garder dans le corpus (plus de 15 mots par document)
dtm.prev <- dtm.prev[clength >= 15, ] # on supprime les lignes dans dtm.prev pour ne pas fausser les stats

documents2 <- documents[as.numeric(row2keep)] # puis on supprime les mêmes enregistrements dans le Corpus


#Terms frequency statistics
freqr <- slam::col_sums(dtm.prev)
#length should be total number of terms
length(freqr)
#length should be total number of terms
ord <- order(freqr,decreasing=TRUE)
#inspect most frequently occurring terms
freqr[head(ord,200L)]
#inspect least frequently occurring terms
freqr[tail(ord,100L)]
#View(freqr)


# On travaille maintenant sur le Corpus réduit
# 'Bounds' (limites haute et basse) sur la répétition des mots
# Ex. dtm <-DocumentTermMatrix(docs, control=list(bounds = list(global = c(1,300))))

dtm.tfidf <- DocumentTermMatrix(documents2, control=list(bounds = list(global = c(1,200)), weighting = function(x) weightTfIdf(x, normalize = TRUE)))
dtm.tf <- DocumentTermMatrix(documents2, control=list(bounds = list(global = c(1,200)), weighting = function(x) weightTf(x)))


dtm.tfidf.nosparse <- removeSparseTerms(dtm.tfidf, 0.98)
dtm.tf.nosparse <- removeSparseTerms(dtm.tf, 0.98)


### DTM statistics

#inspect(dtm)
#inspect(dtm.nosparse)
#inspect(dtm2)
#inspect(dtm2.nosparse)
#inspect(dtm.used)
#inspect(dtm.used[,'withdrawal'])
#dim(dtm)
#dim(dtm2)
#dataset_df[965,]
#documents_nonstem[14]$content


rowTotals <- apply(dtm.tfidf.nosparse , 1, sum) #Find the sum of words in each Document
if (length(rowTotals) >0 ) {
  dtm.new <- dtm.tfidf.nosparse[rowTotals> 0, ] #remove all docs with less than x words
  print(sprintf("dtm tfidf - Nombre de ligne(s) supprimée(s) après removeSparseTerms : %s",length(dtm.tfidf.nosparse[rowTotals == 0, ])))
}else{
  dtm.new <- dtm.tfidf.nosparse
}

rowTotals2 <- apply(dtm.tf.nosparse , 1, sum) #Find the sum of words in each Document
if (length(rowTotals2) > 0) {
  dtm.new2   <- dtm.tf.nosparse[rowTotals2> 0, ] #remove all docs with less than x words
  print(sprintf("dtm tf - Nombre de ligne(s) supprimée(s) après removeSparseTerms : %s",length(dtm.tf.nosparse[rowTotals == 0, ])))
}else{
  dtm.new2 <- dtm.tf.nosparse
}

dtm.used <- dtm.new


# wordcloud
m <- as.matrix(as.TermDocumentMatrix(dtm.used))
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 30)

wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE,  
          colors=brewer.pal(8, "Dark2"))


set.seed(1234)
### Clustering

#http://michael.hahsler.net/SMU/CSE7337/ et http://michael.hahsler.net/SMU/CSE7337/install/tm.R
#http://edutechwiki.unige.ch/fr/Tutoriel_tm_text_mining_package
#https://stackoverflow.com/questions/13944252/remove-empty-documents-from-documenttermmatrix-in-r-topicmodels
#https://stackoverflow.com/questions/24459246/text-analysis-using-lda-and-tm-in-r
#

### Normalize the vectors so Euclidean makes sense with kmeans
norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
m_norm <- norm_eucl(as.matrix(dtm.used))


set.seed(123)

# Finding the optimal number of clusters

# Compute the WSS for 
# k = 2 to k.max (kmeans)
k.max <- 50
sil2 <- (nrow(m_norm)-1)*sum(apply(m_norm,2,var))
for(i in 2:k.max){
  set.seed(123)
  sil2[i] <- sum(kmeans(m_norm, centers = i, iter.max = 20)$withinss)
  #ss2 <- silhouette(k.res2$cluster, dist(m_norm))
  #sil2[i] <- mean(ss2[, 3])
}

# Plot the elbow (kmeans)
plot(1:k.max, sil2, type = "b", pch = 19, 
     frame = FALSE, xlab = "Number of clusters k")
#abline(v = which.min(sil2), lty = 2)


# cluster into x (optimal) clusters - kmeans
set.seed(123)
cl2 <- kmeans(m_norm, 8, iter.max = 20)
table(cl2$cluster)
# show clusters using the first 2 principal components
plot(prcomp(m_norm)$x, col=cl2$cluster)

# palette() - Liste des couleurs (Modulo 8 par défaut) - pour correspondance avec les clusters
# 1-black, 2-red, 3-green3, 4-blue, 5-cyan, 6-magenta, 7-yellow, 8-gray, 9-black, ...   


## Self organizing map (som) - Kohonen map
# https://www.r-bloggers.com/self-organising-maps-for-customer-segmentation-using-r/
# https://www.shanelynn.ie/self-organising-maps-for-customer-segmentation-using-r/
# https://clarkdatalabs.github.io/soms/SOM_Shakespeare_Part_1

set.seed(123)
data_train_matrix <- as.matrix(dtm.used)
# Create the SOM Grid - you generally have to specify the size of the 
# training grid prior to training the SOM. Hexagonal and Circular 
# topologies are possible
som_grid <- somgrid(xdim = 8, ydim=8, topo="hexagonal")
# Finally, train the SOM, options for the number of iterations,
# the learning rates, and the neighbourhood are available
som_model <- som(data_train_matrix, 
                 rlen = 300,
                 grid=som_grid,  
                 alpha=c(0.05,0.01), 
                 keep.data = TRUE,
                 dist.fcts = "sumofsquares")


plot(som_model, type="changes")
plot(som_model, type="count", main="Node Counts")

#plot(som_model, type="dist.neighbours", main = "SOM neighbour distances")
#plot(som_model, type="codes", main="codes plot")
#plot(som_model, type = "property", property = getCodes(som_model)[,1], main=colnames(getCodes(som_model))[1])

plot(som_model,
     type = "mapping",
     bgcol = "lightgray",
     labels = dataset_df$volumeInfo.categories[as.numeric(Docs(dtm.used))])


### it seems the optimal number of cluster is more or less the number of remaining words in DTM... ###
### Instability of the clustering with many tries ###



### Classification

#install.packages('caret')
library(caret)

# https://datascience.stackexchange.com/questions/15670/which-classifier-should-i-use-for-sparse-boolean-features
# Will try lasso logistic regression and random forest...

# TODO : ajout classe à la dtm


## Write final dataset csv
#write.csv2(tidy_df, file=paste0("dataset",".csv"), quote = TRUE)



