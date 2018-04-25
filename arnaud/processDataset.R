#Cleanup
#rm(list = ls())

#getwd()

## INIT

#columns2keep
c2k <- c("volumeInfo.title",
         "volumeInfo.publisher",
         "volumeInfo.description",
         "volumeInfo.categories",
         "volumeInfo.language",
         "volumeInfo.subtitles",
         "volumeInfo.authors")

infile <- "dataset.csv"
dataset_df <- read.csv2(infile, header=TRUE, sep=";")

dataset_df <- dataset_df[-c(29,32,37,39,171,1158,1182,1597,1623),]

## Prepare DTM
#install.packages('tm')
#install.packages('stringi')
#install.packages('stringr')
library(tm)
library(stringi)
library(stringr)

#Corpus initial
documents <- Corpus(VectorSource(dataset_df$volumeInfo.description))

#Désaccentuation
accent <- function(x) stri_trans_general(x, "Latin-ASCII") # cela signifie qu'on remplace un caractère encodé en Latin1 par son équivalent le plus proche en ASCII, il n'y a par exemple pas de caractères accentués en ASCII
documents <- tm_map(documents, content_transformer(accent))

#filtrage ponctuation et nombres
documents <- tm_map(documents, content_transformer(gsub), pattern = "[^a-zA-Z]", replacement = " ")

#Minuscules
documents <- tm_map(documents, content_transformer(tolower))

#Suppression des stopwords (mots-outils)
#stopwords_fr <- stopwords("french")
stopwords_fr <- sapply(stopwords("french"),accent)
documents <- tm_map(documents, removeWords, stopwords_fr)

stopwords_fr2 = c(stopwords_fr,'a','h','lundi','mardi','mercredi','jeudi','vendredi','samedi','dimanche',
                 'etre','apres','selon','comme','alors','tout','tous','faire','depuis','encore',
                 'peut','doit','mieux',"un","deux","trois","quatre","cinq","six","sept","huit","neuf","dix",
                 'tant','ainsi')
stopwords_fr2 = setdiff(stopwords_fr2, c("pas")) # 'pas' est inclus dans les stopword, on trouve que c'est un peu dommage alors on le retire de la liste
documents <- tm_map(documents, removeWords, stopwords_fr2)


#Lemmatisation
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
minfreq <- findFreqTerms(dtm, 30)
length(minfreq)

dtm <- removeSparseTerms(
  DocumentTermMatrix(documents, control=list(dictionary= minfreq, weighting = function(x) weightTfIdf(x, normalize = FALSE)))
  ,
  0.9)

#inspect(dtm)
#dim(dtm)
#documents[1]$content

df <- data.frame(as.matrix(dtm), stringsAsFactors=FALSE)
m <- as.matrix(dtm)

rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ]           #remove all docs without words

##Classification
#install.packages('caret')
library(caret)
#install.packages('skmeans')
library(skmeans)

#Non-supervisé
#http://michael.hahsler.net/SMU/CSE7337/ et http://michael.hahsler.net/SMU/CSE7337/install/tm.R
#http://edutechwiki.unige.ch/fr/Tutoriel_tm_text_mining_package
#https://stackoverflow.com/questions/13944252/remove-empty-documents-from-documenttermmatrix-in-r-topicmodels
#https://stackoverflow.com/questions/24459246/text-analysis-using-lda-and-tm-in-r
## do document clustering
sk <- skmeans(x=dtm.new, 6)


table(sk$cluster)

### show clusters using the first 2 principal components
plot(prcomp(m_norm)$x, col=cl$cl)

findFreqTerms(dtm[cl$cluster==1], 50)
#inspect(reuters[which(cl$cluster==1)])


## Write final dataset csv
#write.csv2(tidy_df, file=paste0("dataset",".csv"), quote = TRUE)



