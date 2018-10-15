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

docs.pred <- VCorpus(DataframeSource(df.pred))
docids.pred <- unname(unlist(meta(docs.pred,'id'))) # Backup doc ids to avoid losses during tm_map operations (workaround before tm 0.7-3.2)


#Remove chars with accents
accent <- function(x) stri_trans_general(x, "Latin-ASCII") # cela signifie qu'on remplace un caractère encodé en Latin1 par son équivalent le plus proche en ASCII, il n'y a par exemple pas de caractères accentués en ASCII
docs.pred2 <- tm_map(docs.pred, content_transformer(accent))

#Remove URLs
removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T, ignore.case = T))
docs.pred2 <- tm_map(docs.pred2, removeURL)
removeURL2 <- content_transformer(function(x) gsub("www\\.\\S+", "", x, perl=T, ignore.case = T))
docs.pred2 <- tm_map(docs.pred2, removeURL2)

#Put in lowercase
docs.pred2 <- tm_map(docs.pred2, content_transformer(tolower))

#Remove numbers and punctuation
docs.pred2 <- tm_map(docs.pred2, content_transformer(gsub), pattern = "[^a-zA-Z]", replacement = " ")

#Stopwords removal
stopwords_fr <- sapply(stopwords("french"),accent)
stopwords_jlt <- unlist(read.csv(file='jltStopwords_UTF8.txt', encoding = 'UTF-8', stringsAsFactors = F))
stopwords_jlt <- unique(sapply(stopwords_jlt, accent))

stopwords_fr2 <- union(stopwords_fr, stopwords_jlt)
stopwords_fr2 <- union(stopwords_fr2,
                       c('a','h','lundi','mardi','mercredi','jeudi','vendredi','samedi','dimanche',
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

docs.pred2 <- VCorpus(DataframeSource(df)) #This action restores the corpus.
rm(list = c('a','df'))


### DTM
# 'Bounds' (limites haute et basse) sur la répétition des mots
# Ex. dtm <-DocumentTermMatrix(docs, control=list(bounds = list(global = c(1,300))))

#dtm.pred.tfidf <- DocumentTermMatrix(docs.pred2.bis, control=list(bounds = list(global = c(1,260)), weighting = function(x) weightTfIdf(x, normalize = TRUE)))
dtm.pred.tf <- DocumentTermMatrix(docs.pred2, control=list(bounds = list(global = c(1,260)), weighting = function(x) weightTf(x)))


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
df.pred.used <- as.data.frame(as.matrix(dtm.pred.used))
df.merge <- as.data.frame(matrix(data = 0L, nrow(dtm.pred.used), ncol(df.dtm)))
names(df.merge) <- words.train
#df.merge
#dim(df.merge)
#dim(dtm.pred.used)

for (i in words.used) {
  df.merge[, i] <- df.pred.used[, i]
}

#View(df.merge)

wc.train <- apply(df.dtm, 2, FUN = function(x) length(x[x != 0])) # Find the frequency of words in train corpus
totalDocs.train <- nrow(df.dtm)
wc.bydoc.pred <- apply(as.matrix(dtm.pred.used), 1, FUN = function(x) length(x[x != 0])) # Normalization term

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

### Calcul TFIDF par rapport avec jeu de données d'entrainement
### Elagage / merge des mots
### https://stackoverflow.com/questions/10956873/how-to-print-the-name-of-current-row-when-using-apply-in-r
### https://stackoverflow.com/questions/15287038/r-apply-on-a-matrix-a-function-of-columns-and-row-index
### https://stackoverflow.com/questions/2545879/row-column-counter-in-apply-functions


for (i in words.used) {
  #print(i)
  for (j in 1:length(df.merge[, i])) {
    if (df.merge[j,i] != 0) {
      val <- fun.mytfidf(df.merge[j, i], wc.bydoc.pred[j], wc.train[i], nrow(df.dtm))
      df.merge[j,i] <- val
    }
  }
}

#View(df.merge)


# Preparation de la dtm en vue de la classification supervisée


#install.packages('caret', dependencies = c('Depends','Imports'))
#install.packages('MLmetrics')
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

#tic()
caret::predict.train(rf_gridsearch, newdata = df.merge, type = 'raw')
caret::predict.train(rf_gridsearch, newdata = df.merge, type = 'prob')

prediction1 <- caret::predict.train(rf_gridsearch, newdata = df.merge, type = 'raw')

confmat1 <- confusionMatrix(prediction1, factor(df.pred$Categorie, levels = levels(prediction1)))
confmat1

#toc()


## Penalized multinomial logistic regression w/ nnet (neural network)

#tic()
caret::predict.train(nnetFit, newdata = df.merge, type ='raw')
caret::predict.train(nnetFit, newdata = df.merge, type ='prob')

prediction2 <- caret::predict.train(nnetFit, newdata = df.merge, type = 'raw')

confmat2 <- confusionMatrix(prediction2, factor(df.pred$Categorie, levels = levels(prediction2)))
confmat2

#toc()




