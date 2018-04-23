#Cleanup
#rm(list = ls())

### Arguments

# args = commandArgs(trailingOnly=TRUE)
# if (length(args)==0) {
#   stop("At least one argument must be supplied (category).\n", call.=FALSE)
# } else if (length(args)>=1) {
#   
#   # category
#   category <- args[1]
# }

### Packages
.libPaths( c( .libPaths(), "~/Documents/R/win-library/3.4"))
print(paste0("Using libPath : ",.libPaths()))
#install.packages("httr")
#install.packages("jsonlite")
library("jsonlite")
library("httr")


##############################################################################################
### Helper functions
### function getCategoryRequest

getCategoryRequest <- function(p_baseURL, p_baseURLParams, p_category, p_startIndex=0) {
  
  url <- p_baseURL
  startIndex <- p_startIndex
  urlParams <- c(p_baseURLParams, list(startIndex=startIndex, q=sprintf("subject:%s",p_category)))
  
  q <- GET(url, query=urlParams, user_agent("Mozilla/5.0 (Windows NT 10.0; WOW64; rv:56.0) Gecko/20100101 Firefox/56.0"))
  
  return(q)
  
}

### function normalizeDFCol

normalizeDFCol <- function(p_data, p_cname) {
  add <- p_cname[!p_cname %in% names(p_data)]
  
  if(length(add)!=0) {
    print(add)
    p_data[add] <- NA
    print(sprintf("%s column(s) added...", length(add)))
  }
  
  tryCatch(
    if(ncol(p_data) > length(p_cname)) {
      delta <- ncol(p_data)-length(p_cname)
      p_data <- p_data[p_cname]
      print(sprintf("%s column(s) dropped...", delta))
    },
    error = function(e) {head(p_data)})
  
  return(p_data)  
}


##############################################################################################

### Init
baseURL <- "https://www.googleapis.com/books/v1/volumes"
baseURLParams <- list(langRestrict="fr",
                      printType="books",
                      maxResults="40")

# The target dataframe
tColnames <- c("volumeInfo.title", "volumeInfo.subtitle", "volumeInfo.authors", 
               "volumeInfo.publisher", "volumeInfo.publishedDate", "volumeInfo.description", 
               "volumeInfo.industryIdentifiers", "volumeInfo.pageCount", "volumeInfo.printType", 
               "volumeInfo.categories", "volumeInfo.maturityRating", "volumeInfo.allowAnonLogging", 
               "volumeInfo.contentVersion", "volumeInfo.language", "volumeInfo.previewLink", 
               "volumeInfo.infoLink", "volumeInfo.canonicalVolumeLink", "volumeInfo.averageRating", 
               "volumeInfo.ratingsCount", "volumeInfo.readingModes.text", "volumeInfo.readingModes.image", 
               "volumeInfo.imageLinks.smallThumbnail", "volumeInfo.imageLinks.thumbnail", 
               "volumeInfo.panelizationSummary.containsEpubBubbles", "volumeInfo.panelizationSummary.containsImageBubbles"
)
df_volInfos <- data.frame(matrix(ncol = 25, nrow = 0))
colnames(df_volInfos) <- tColnames


##############################################################################################
### The dirty mechanic...

dumpCategory <- function (p_category) {
  bookCategory <- c("Science","Fiction","Architecture","Nature","Plantes","Agriculture","Psychologie","Sociologie")
  #category <- bookCategory[2]
  category <- p_category
  startIndex <- 0
  currentIndex <- 0
  
  response <- getCategoryRequest(baseURL, baseURLParams, category)
  print(sprintf("URL : %s",response$url))
  
  body <- content(response,"text")
  df <- fromJSON(body, flatten = TRUE, simplifyVector = FALSE, simplifyDataFrame = TRUE, simplifyMatrix = FALSE)
  
  totalItems <- df$totalItems
  df_volInfos <- df$items[,grep("volumeInfo",colnames(df$items))]
  
  #dput(colnames(df_volInfos))
  #View(df_volInfos)
  #View(df)
  
  while (TRUE) {
    
    if ((totalItems > 40) && (currentIndex < totalItems) && (startIndex+40 < totalItems)) {
      
      Sys.sleep(2+abs(rnorm(1,0.5,0.5))) # in second
      
      startIndex <- startIndex + 40
      response <- getCategoryRequest(baseURL, baseURLParams, category, startIndex)
      print(response$url)
      
      body <- content(response,"text")
      df_t <- fromJSON(body, flatten = TRUE, simplifyVector = FALSE, simplifyDataFrame = TRUE, simplifyMatrix = FALSE)
      totalItems <- df_t$totalItems
      
      
      df_volInfos_t <- df_t$items[,grep("volumeInfo",colnames(df_t$items))]
      print(sprintf("Number of field(s) : %s",ncol(df_volInfos_t)))
      
      df_volInfos_t <- normalizeDFCol(df_volInfos_t, tColnames)
      df_volInfos <- rbind(df_volInfos, df_volInfos_t)
      #View(df_volInfos_t)
      
      currentIndex <- currentIndex + nrow(df_volInfos_t)
      
      print(sprintf("Current index : %s, total : %s",currentIndex, totalItems))
      
    }else{
      break
    }
    
  }
  
  print(sprintf("Writing \"%s\" category file to disk...", category))
  write_json(df_volInfos, paste0(category,".json"), pretty = TRUE) #JSON
  
  # Transformation avant csv : ex. list() to comma separated strings
  df_volInfos$volumeInfo.authors <- sapply(df_volInfos$volumeInfo.authors, simplify = TRUE, FUN = function(x) {paste0(x, collapse=",")})
  df_volInfos$volumeInfo.categories <- sapply(df_volInfos$volumeInfo.categories, simplify = TRUE, FUN = function(x) {paste0(x, collapse=",")})
  df_volInfos$volumeInfo.industryIdentifiers<-NULL #Le garde-t-on au final ?
  #lapply(df_volInfos, FUN = function(x) {is.list(x)})
  
  write.csv2(df_volInfos, file=paste0(category,".csv"), quote = TRUE) #CSV
  #View(df_volInfos)
  
}