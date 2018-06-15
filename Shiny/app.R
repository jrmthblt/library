#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tm)
library(wordcloud)
library(shiny)

# Liste des collections :
collections <- levels(dfBookSummaries$collection)

getTermMatrix <- function(collection) {
  m <- dtmTfIdf_m[dfBookSummaries$collection == collection,]
  wordsFreq <- sort(colSums(m),decreasing = TRUE)
  # wordsFreq <- data.frame(word = names(wordsFreq), Freq = as.vector(wordsFreq)) # mise au format pour wordcloud2
  return(wordsFreq)
}


getPrediction <- function(inputResume) {
  resume <- Corpus(VectorSource(inputResume))
  resume <- tm_map(resume, content_transformer(nfdRemoveAccents))
  resume <- tm_map(resume, content_transformer(tolower))
  resume <- tm_map(resume, content_transformer(removePonctuation))
  inspect(resume[1])
  
  resume_m <- as.data.frame(as.matrix(DocumentTermMatrix(resume)))
  
  nonEmptyDtmTfDf <- as.data.frame(as.matrix(nonEmptyDtmTf))
  nonEmptyDtmTfDf <- nonEmptyDtmTfDf[1,]
  nonEmptyDtmTfDf[1,] <- 0
  
  resume_m <- bind_rows(nonEmptyDtmTfDf, resume_m)
  
  resume_m <- resume_m[,colnames(nonEmptyDtmTfDf)]
  resume_m <- resume_m[2,]
  
  resume_m[,which(is.na(resume_m))] <- 0
  
  rpartPredictionProbability <- predict(object = rpartModel$finalModel, resume_m, type = 'prob')
  print(rpartPredictionProbability)
  return(rpartPredictionProbability)
}


# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Démonstrateur projet Library"),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      selectInput("selection", "Choisir une collection :",
                  choices = collections),
      actionButton("update", "Change"),
      hr(),
      textAreaInput("resume", "Résumé à classer : ", ""),
      actionButton("predict", "Prédire la collection")
    ),
    

    mainPanel(
      plotOutput("plot"),
      textOutput("predictedCollection")
    )
  )
)

# Define server logic required to draw a histogram
server <- 
  function(input, output, session) {
    # Define a reactive expression for the document term matrix
    terms <- reactive({
      # Change when the "update" button is pressed...
      input$update
      # ...but not for anything else
      isolate({
        withProgress({
          getTermMatrix(input$selection)
        })
      })
    })
    
    
    prediction <- reactive({
      # Change when the "predict" button is pressed...
      input$predict
      # ...but not for anything else
      
      isolate({
        withProgress({
          getPrediction(input$resume)
        })
      })
    })
    
    # Make the wordcloud drawing predictable during a session
    wordcloud_rep <- repeatable(wordcloud)
    
    output$plot <- renderPlot({
      v <- terms()
      wordcloud_rep(names(v), v, scale=c(4,0.5),
                    min.freq = 15, max.words=100,
                    colors=brewer.pal(8, "Dark2"))
    })
    
    output$predictedCollection <- renderText(prediction())
  }

# Run the application 
shinyApp(ui = ui, server = server)