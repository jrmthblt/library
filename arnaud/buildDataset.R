#Cleanup
#rm(list = ls())

#getwd()

## INIT
infiles <- list.files(pattern = ".csv$")
#print(infiles)

#columns2keep
c2k <- c("volumeInfo.title",
         "volumeInfo.publisher",
         "volumeInfo.description",
         "volumeInfo.categories",
         "volumeInfo.language",
         "volumeInfo.subtitles",
         "volumeInfo.authors"
)
merge_df <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(merge_df) <- c2k

## CORE
for(file in infiles) {
  
  #file <- "Agriculture.csv"
  print(sprintf("Reading %s...", file))
  temp_df <- read.csv(file, header=TRUE, sep=";")
  
  # Select columns
  temp_df <- temp_df[,names(temp_df) %in% c2k]
  # Enforce category
  temp_df$volumeInfo.categories <- gsub(pattern = "\\.csv$", "", ignore.case = TRUE , file)
  
  # Add category to dataset
  merge_df <- rbind(merge_df, temp_df)
  
}

## Cleansing and filtering
# Row selection :
# - language fr
# - description filled

tidy_df <- merge_df[merge_df$volumeInfo.language=="fr",]
tidy_df <- subset(tidy_df, (!is.na(volumeInfo.description)))

## Write final dataset csv
write.csv2(tidy_df, file=paste0("dataset",".csv"), quote = TRUE)



