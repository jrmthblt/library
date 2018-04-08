#Cleanup
#rm(list = ls())

### Import script
source(file="dumpCategory.R")


### Dump categories
bookCategory <- c("Science","Fiction","Architecture","Nature","Plantes","Agriculture","Psychologie","Sociologie")

dumpCategory("Science")
dumpCategory("Fiction")
dumpCategory("Architecture")
dumpCategory("Nature")
dumpCategory("Plantes") # Pas top
dumpCategory("Agriculture")
dumpCategory("Psychologie")
dumpCategory("Sociologie")

