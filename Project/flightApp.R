library(shiny)
library(shinycssloaders)
library(wordcloud)
library(ggplot2)
library(shinydashboard)
library(dplyr)
library(tidytext)
library(DT)
library(readxl)

#Untuk set working directory
setwd('D:/Semester IV/Prak. Data Science/Projek/Project')
Airplane <- read_excel('data/Airplane.xlsx')

source("scraper/scrapReviewAirplane.R")
source("model-dan-dataset/modelNB.R")

ui <- fluidPage(
  
  titlePanel("Sentiment Analysis of 10 Airplanes in the World"),
  
  sidebarLayout(
    
    sidebarPanel(
      uiOutput("selectAirplane")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "About",
          helpText("This app will display the sentiment classification of Airplane user review from Airlinequality
                   website. The sentiment will be split into Positive Sentiment and Negative Sentiment.")
          # conditionalPanel(condition = is.na("output$total"), 
          #                  h3(textOutput("Sorry, the restaurant you selected has no reviews"))
          #                  )
        ),
        tabPanel(
          "User Review + Sentiment Classification",
          fluidRow(
            box(
              title = "User Review",
              solidHeader = T,
              width = 12,
              collapsible = T,
              div(DT::dataTableOutput("table_review") %>% withSpinner(color="#1167b1"), style = "font-size: 70%;")
            ),
            box(title = "Sentiment Classification",
                solidHeader = T,
                width = 12,
                collapsible = T,
                plotOutput("plot") %>% withSpinner(color="#1167b1")
            )
          )
        ),
        tabPanel(
          "Wordcloud",
          fluidRow(
            box(title = "Wordcloud",
                solidHeader = T,
                width = 12,
                collapsible = T,
                plotOutput("wordcloud") %>% withSpinner(color="#1167b1")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  var <- reactive({
    setNames(Airplane$link, Airplane$airplane)
  })
  
  output$selectAirplane <- renderUI({
    selectInput(inputId  = "selectAirplane", 
                label = "Pilih Airplane",
                choices = var())
  })
  
  dataScrap <- reactive({
    result <- get_airplane_reviews(input$selectAirplane, incProgress)
    return(result)
  })
  
  data_prediction <- reactive({
    withProgress({
      setProgress(message = "Predicting", value = 0)
      
      reviews <- dataScrap()$review
      incProgress(1/2)
      prediction <- get_prediction(reviews)
      incProgress(1/2)
    })
    prediction$reviewer <- dataScrap()$reviewer
    
    return(prediction)
  })
  
  output$total_review <- renderText({
    paste0("This airplane has ", nrow(dataScrap()), " review")
  })
  
  output$table_review <- renderDataTable(datatable({
    data_prediction()
  }))
  
  output$wordcloud <- renderPlot({
    data_corpus <- clean_data(dataScrap()$review)
    wordcloud(data_corpus, min.freq = 30, max.words = 50)
  })
  
  output$plot <- renderPlot({
    Classification <- data_prediction()$sentiment
    Total <- nrow(data_prediction())
    ggplot(data_prediction(), aes(x = Classification, y = Total/100, fill = Classification)) + geom_col()
  })
      
}

shinyApp(ui, server)
