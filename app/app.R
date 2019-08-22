library(shiny)

source("haiku.R")
load("trumpData.rda")

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Trump tweet haiku maker"),
  fluidRow("Create a haiku poem out of sentences from Trump's presidential tweets.",
    radioButtons("senti", label = "What sentiment should your haiku have?", 
                        choices = list(Random = "random",
                                       Positive = "positive",
                                       Negative = "negative"
                                       ),
                        selected = "random", inline = TRUE),
           actionButton("doHaiku", label = "Make haiku!"),
           HTML("<br>"),
           htmlOutput("outhaiku")
  )
)

server <- function(input, output) {
 
  re <- eventReactive(input$doHaiku, input$senti)
  output$outhaiku <- renderUI({
    senti <- re()  
    HTML(paste(randomHaiku(sentiment = senti, doReturn = TRUE), collapse = "<br>"))
  })
}

shinyApp(ui, server)
