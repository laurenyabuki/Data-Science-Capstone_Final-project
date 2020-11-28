library(shiny)
library(shinyWidgets)
library(shinythemes)
library(stringr)


#Load the n-gram databases 
bigram<-readRDS("bigram.RData")
trigram<-readRDS("trigram.RData")
quadgram<-readRDS("quadgram.RData")

ui <- navbarPage("Coursera Data Science Capstone: Final Course Project", 
           tabPanel("Word Prediction",
                    div(style="width:2400px; padding-left:25px;",), fluidPage(
                      theme = shinytheme("superhero"),
                      tags$head(tags$style(HTML(".navbar-brand {font-size: 28px;}"))),
                      tags$head(tags$style('body {color:white;font:18pt "Arial"}')),
                      tags$head(tags$style('h1 {color:white; font:18pt "Arial"}')),
                      tags$head(tags$style('h2 {color:white; font:14pt "Arial"}')),
                      tags$head(tags$style('h3 {color:white; font:24pt "Arial"}')),
                      tags$head(tags$style(HTML('<style type="text/css"> .row-fluid { width: 50%; }.well { background-color: #696969;}</style>'))),
                      headerPanel(
                         h1("A Shiny app has been developped to take as input a phrase (multiple words) in a text box input and to output a prediction of the next word.")),
                      sidebarLayout(
                        sidebarPanel(
                          width = 5,
                          h1("𝗧𝘆𝗽𝗲 𝘆𝗼𝘂𝗿 𝘁𝗲𝘅𝘁 𝗶𝗻 𝘁𝗵𝗲 𝗯𝗼𝘅 𝘁𝗼 𝗼𝗯𝘁𝗮𝗶𝗻 𝘁𝗵𝗲 𝗽𝗿𝗲𝗱𝗶𝗰𝘁𝗲𝗱 𝘄𝗼𝗿𝗱"),
                          textInput("txt",label=NULL),
                          h1("𝗪𝗼𝗿𝗱 𝗣𝗿𝗲𝗱𝗶𝗰𝘁𝗶𝗼𝗻"),
                          verbatimTextOutput("prediction", placeholder = T)),
                        mainPanel(
                          h3(strong("Data Science Capstone: Final Course Project")),
                          h2("Author: Lauren Yabuki - 28th of November, 2020"),
                          h2("This application is a web application using R and",
                          a(href = "https://shiny.rstudio.com/", "Shiny library"),
                            "corresponding to the last assignment from,",
                          a(href = "https://www.coursera.org/learn/data-science-project", "Data Science Capstone from Coursera")),
                          hr(),
                          h2("Source code is available at",
                          a(href = "https://github.com/laurenyabuki/Data-Science-Capstone_Final-project",
                           "https://github.com/laurenyabuki/Data-Science-Capstone_Final-project")))
                        ))))
        

PNW <- function(sentence) {
  
  inputtxt <- gsub('[[:punct:]]|[[:digit:]]', "", sentence) ## Remove numbers and punctuations
  inputtxt <- unlist(strsplit(inputtxt, "\\s+")) # split the input string by white spaces
  
  req(inputtxt)
  
  if (length(inputtxt)>= 3) {
  
    outputtxt <- paste(tail(inputtxt, 3), collapse = " ")
    predictedtxt<- quadgram$word[grepl(pattern= paste("^",outputtxt, sep=""), x=quadgram$word, ignore.case = T)]
    guess_word <- word(predictedtxt[1],-1)
    
    if(length(predictedtxt)==0){
      
      outputtxt <- paste(tail(inputtxt, 2), collapse = " ")
      predictedtxt<- trigram$word[grepl(pattern= paste("^",outputtxt, sep=""), x=trigram$word, ignore.case = T)]
      guess_word <- word(predictedtxt[1],-1)
      
      if(length(predictedtxt)==0){
        
        outputtxt <- paste(tail(inputtxt, 1), collapse = " ")
        predictedtxt<- bigram$word[grepl(pattern= paste("^",outputtxt, sep=""), x=bigram$word, ignore.case = T)]
        guess_word <- word(predictedtxt[1],-1)
        
        if(length(predictedtxt)==0){
          guess_word <-"the"
        }
      }
    }
  } else if (length(inputtxt)==2) {
    
    outputtxt <- paste(tail(inputtxt, 2), collapse = " ")
    predictedtxt<- trigram$word[grepl(pattern= paste("^",outputtxt, sep=""), x=trigram$word, ignore.case = T)]
    guess_word <- word(predictedtxt[1],-1)
    
    if(length(predictedtxt)==0){
      
      outputtxt <- paste(tail(inputtxt, 1), collapse = " ")
      predictedtxt<- bigram$word[grepl(pattern= paste("^",outputtxt, sep=""), x=bigram$word, ignore.case = T)]
      guess_word <- word(predictedtxt[1],-1)
      
      if(length(predictedtxt)==0){
        guess_word <-"the"
      }
    }
  } else if (length(inputtxt)==1) {
    
    outputtxt <- paste(tail(inputtxt, 1), collapse = " ")
    predictedtxt<- bigram$word[grepl(pattern= paste("^",outputtxt, sep=""), x=bigram$word, ignore.case = T)]
    guess_word <- word(predictedtxt[1],-1)
    
    if(length(predictedtxt)==0){
      guess_word <-"the"
    }
  } 
  paste(guess_word)
}

server<-shinyServer(
  
  function(input, output) {
    output$txt <- renderPrint({input$txt})  
    output$prediction <- reactive(paste(input$txt,pred()))
    pred<- reactive({
      PNW(sentence=input$txt) 
    })
  }
)

# Run the application 
shinyApp(ui = ui, server = server)
