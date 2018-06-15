#Cleanup
#rm(list = ls())

#tempdir()
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

## Import files
dtm.infile <- 'dtm-dataset.csv'
df.dtm <- read.csv2(dtm.infile, header=TRUE, sep=";", stringsAsFactors = FALSE)

pred.infile <- 'pred-dataset.csv'
df.pred <- read.csv2(pred.infile, header=TRUE, sep=";", stringsAsFactors = FALSE)

## Load models
load(file = 'models')

# Check
print(rf_gridsearch)
#plot(rf_gridsearch)
print(nnetFit)
#plot(nnetFit)


## Prepare prediction dataset

#Initial corpus
names(df.pred)[1]<-"doc_id"
names(df.pred)[2]<-"text"

docs.pred <- Corpus(DataframeSource(df.pred))
docids.pred <- meta(docs.pred,'id') # Backup doc ids to avoid losses during tm_map operations


#Remove chars with accents
accent <- function(x) stri_trans_general(x, "Latin-ASCII") # cela signifie qu'on remplace un caractère encodé en Latin1 par son équivalent le plus proche en ASCII, il n'y a par exemple pas de caractères accentués en ASCII
docs.pred2 <- tm_map(docs.pred, content_transformer(accent))

#Remove URLs
removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T, ignore.case = T))
docs.pred2 <- tm_map(docs.pred2, removeURL)
removeURL2 <- content_transformer(function(x) gsub("www\.\\S+", "", x, perl=T, ignore.case = T))
docs.pred2 <- tm_map(docs.pred2, removeURL2)

#Put in lowercase
docs.pred2 <- tm_map(docs.pred2, content_transformer(tolower))

#Stopwords removal
stopwords_fr <- sapply(stopwords("french"),accent)

#Remove numbers and punctuation
docs.pred2 <- tm_map(docs.pred2, content_transformer(gsub), pattern = "[^a-zA-Z]", replacement = " ")


stopwords_fr2 <- union(stopwords_fr,c('a','h','lundi','mardi','mercredi','jeudi','vendredi','samedi','dimanche',
                                     'oui','non','apres','selon','comme','alors','tout','tous','faire','depuis','encore',
                                     'peut','doit','mieux','un','deux','trois','quatre','cinq','six','sept','huit','neuf','dix',
                                     'tant','ainsi','livre','livres','oeuvre','aussi','fait','entre','plus','moins','toute','donc',
                                     'toutes','auteur','bien','roman','comment','petit','petite','grand','grande','ceux','lorsque',
                                     'etc','annee','aujourd','hui','tres','seul','seule','autre','autres','celle','donc','dont',
                                     'donne','sous','sur','jusqu','quelqu','nombreux','propose','part','parti','partir','jamais',
                                     'car','grands','grandes','question','fois','france','travers','jour','avant','apres','lorsqu',
                                     'il','elle','ils','elles','tel','tels','telle','telles','quelque','quelques','toujours',
                                     'souvent','chez','celui','chaque','mis','mise','texte','moindre','peu','ouvrage','ouvrages',
                                     'rien','pourtant','texte','the','certains','certaines','lecteur','vers','parfois','grace','aime',
                                     'aimer','enfin','chacun','chacune','pourquoi','propos','plusieurs','avoir','etre','contre',
                                     'mais','faut','puis','notre','votre','seule','seules','seulement','note','noter','edition',
                                     'les','d','l','c','resume','science-fiction','ou','tome','serie','roman','romans','collection',
                                     'des','livre','gerard','dirigee','prix','king','auteur','stephen','histoire','etait','meme',
                                     'recueil','hugo','quinze','chose','jean','nombreuses','trop','peuvent','mot','mots'
))

docs.pred2 <- tm_map(docs.pred2, content_transformer(removeWords), stopwords_fr2)


#Stemming
docs.pred2_nonstem <- docs.pred2
docs.pred2 <- tm_map(docs.pred2, stemDocument, "french")

#Strip whitespaces
docs.pred2 <- tm_map(docs.pred2, stripWhitespace) # n'enleve pas le tout premier espace
docs.pred2 <- tm_map(docs.pred2, content_transformer(gsub), pattern = "^\\s+", replacement = "")


# Restore the corpus - Fix metadata corruption of the corpus due to tm_map
a <- list()
for (i in seq_along(docs.pred2)) {
  a[i] <- gettext(docs.pred2[[i]][[1]]) #Do not use $content here!
}
df <- data.frame(doc_id = docids.pred, text = matrix(NA, nrow = length(a), ncol = 1), stringsAsFactors = FALSE)
df$text <- unlist(a)

docs.pred2 <- Corpus(DataframeSource(df)) #This action restores the corpus.
rm(list = c('a','df'))


#DTM

#dtm.pred.tfidf <- DocumentTermMatrix(docs.pred2.bis, control=list(bounds = list(global = c(1,260)), weighting = function(x) weightTfIdf(x, normalize = TRUE)))
dtm.pred.tf <- DocumentTermMatrix(docs.pred2, control=list(bounds = list(global = c(1,260)), weighting = function(x) weightTf(x)))

# 'Bounds' (limites haute et basse) sur la répétition des mots
# Ex. dtm <-DocumentTermMatrix(docs, control=list(bounds = list(global = c(1,300))))


#Terms frequency statistics
freqr.pred <- slam::col_sums(dtm.pred.tf)
#length should be total number of terms
length(freqr.pred)
#length should be total number of terms
ord.pred <- order(freqr.pred,decreasing=TRUE)
#inspect most frequently occurring terms
freqr.pred[head(ord.pred,300L)]
#inspect least frequently occurring terms
freqr.pred[tail(ord.pred,100L)]
#View(freqr)


#dtm.pred.tfidf.nosparse <- removeSparseTerms(dtm.pred.tfidf, 0.98)
dtm.pred.tf.nosparse <- removeSparseTerms(dtm.pred.tf, 0.98)


### DTM statistics

#inspect(dtm.pred.tf.nosparse[,'musical'])
#inspect(dtm)
#inspect(dtm.pred.tf.nosparse)
#inspect(dtm.used)
#inspect(dtm.used[1:10,])
#dim(dtm)
#dim(dtm2)
#dataset_df2['doc_id' == 12,]
#documents_nonstem[12]$content


### On garde les mots déjà présents dans le jeu d'entrainement
words.train <- names(df.dtm)
words.pred <- Terms(dtm.pred.tf.nosparse)
words.used <- intersect(words.train, words.pred)

dtm.pred.used <- dtm.pred.tf.nosparse[,words.used]

#View(as.data.frame(as.matrix(dtm.pred.used)))

wc.train <- apply(df.dtm, 2, FUN = function(x) length(x[x != 0])) #Find the frequency of words in train corpus
totalDocs.train <- nrow(df.dtm)
wc.bydoc.pred <- apply(as.matrix(dtm.pred.used), 1, FUN = function(x) length(x[x != 0]))

fun.mytfidf <- function(x,y,yy,z) {
  # http://www.tfidf.com/
  #
  # x : document count of a term
  # y : overall count of words in a document (normalization term)
  # yy: number of docs where term appears
  # z : total nb. of documents
  tfn.x <- x/y
  idf.x <- 1 + log(z/yy)
  tfidf.x <- tfn.x * idf.x
  return(tfidf.x)
}


### TODO : calcul TFIDF par rapport avec jeu de données d'entrainement
### TODO : élagage / merge des mots
### https://stackoverflow.com/questions/10956873/how-to-print-the-name-of-current-row-when-using-apply-in-r


# Preparation de la dtm en vue de la classification supervisée
# Obtention de la catégorie présumée pour chaque enregistrement present dans la DTM 

row2keep2 <- (rowTotals2 > 0)
documents3 <- documents2[row2keep2]
ids <- as.numeric(meta(documents3,'id'))



set.seed(123)



### it seems the optimal number of cluster is more or less the number of remaining words in DTM... ###
### Instability of the clustering with many tries ###



### Predictions
# rf: https://stackoverflow.com/questions/47521841/r-randomforest-caret-seeing-predictions
# rf: http://rstudio-pubs-static.s3.amazonaws.com/27155_519e7e23601048d08eb8a74d2a01ad2f.html
# nnet : 


#install.packages('caret', dependencies = c('Depends','Imports'))
#install.packages("MLmetrics")
#install.packages('randomForest')
#install.packages('nnet')
#install.packages('doParallel')
#install.packages('tictoc')

library(caret)
library(randomForest)
library(nnet)
library(parallel)
library(doParallel)
library(tictoc)



## Random Forest

tic()
toc()


## Multinomial logistic regression w/ nnet (neural network)

tic()
toc()



