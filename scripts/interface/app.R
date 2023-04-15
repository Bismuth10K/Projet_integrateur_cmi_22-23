library(shiny)
library(leaflet)
library(readr)
library(dplyr)
library(shinydashboard)
library(htmlwidgets)
library(shinythemes)
library(plotly)
library(shinycssloaders)
library(ggplot2)
library(ggExtra)

ui = bootstrapPage(
  tags$link(
    rel="stylesheet", 
    href="https://fonts.googleapis.com/css2?family=Quicksand:wght@300&display=swap" 
  ),
  tags$style(HTML('
      .box {
        # border: 1px solid #3182bd;
        #border-radius: 0.25rem;
        #padding: 1rem;
        margin-bottom: 1rem;
        box-shadow: rgba(0, 0, 0, 0.35) 0px 5px 15px;
        width: 100%;
        text-align: center;
        
      }
      .boxText {
        # border: 1px solid #3182bd;
        #border-radius: 0.25rem;
        #padding: 1rem;
        margin-bottom: 2rem;
        box-shadow: rgba(0, 0, 0, 0.35) 0px 5px 15px;
        width: 100%;
        font-family: "Quicksand", sans-serif;
        font-size: 32px;
        text-align: center;
      }
      .boxPlot {
        # border: 1px solid #3182bd;
        #border-radius: 0.5rem;
        #padding: 1rem;
        margin-bottom: 1rem;
        box-shadow: rgba(0, 0, 0, 0.35) 0px 5px 15px;
        width: 100%;
        font-family: "Quicksand", sans-serif;
        font-size: 18px;
      }
      
      .box-header {
        background-color: #f05424;
        color: white;
        padding: 0.5rem 1rem;
        border-top-left-radius: 0.25rem;
        border-top-right-radius: 0.25rem;
        font-family: "Quicksand", sans-serif;
        font-size: 16px;
        font-weight: bold;
      }
    ')),
  
  navbarPage(theme = shinytheme('flatly'),"Global Warming Dashboard", id="main",
             tabPanel("Descriptive Analysis", 
                      sidebarLayout(
                        sidebarPanel(
                          # Create a selection list for choosing the dataset
                          selectInput("dataset", "Choose a dataset:", 
                                      choices = c("antartica mass" = "../../datasets_nasa/cleaned_dataset/antarctica_mass_clean.csv", 
                                                  "carbone dioxyde" = "../../datasets_nasa/cleaned_dataset/co2_clean.csv", 
                                                  "global temperature" = "../../datasets_nasa/global_temperature.txt"
                                                  #"green and land mass" = "../../datasets_nasa/2485_Sept_Arctic_extent_1979-2021.xlsx" 
                                                  #"carbone dioxyde" = "", 
                                                  #"global temperature" = "",
                                                  )),
                          selectInput("date_feature", "Choose a time feature:", 
                                      choices = NULL),
                          
                          selectInput("y_feature", "Choose a Y feature:", 
                                      choices = NULL)
                          
                        ),
                        
                        mainPanel(
                          verbatimTextOutput("selected_dataset"),
                          mainPanel(
                            plotOutput("plot")
                          )
                        )
                      )
             ),
             tabPanel("Predictor", 
             ),
             tabPanel("Data Explorer", DT::DTOutput("table") %>% withSpinner(color='#16536a')),
             
             tabPanel("Learn More",
                      #includeMarkdown('../README.md')
             )
  )
  
  
  
)






server <- function(input, output, session){
  
  dataset <- reactive({
    read.csv(input$dataset)
  })
  
  observeEvent(input$dataset, {
    updateSelectInput(session, "date_feature", 
                      choices = colnames(dataset()), 
                      selected = NULL)
  })
  
  observeEvent(input$dataset, {
    updateSelectInput(session, "y_feature", 
                      choices = colnames(dataset()), 
                      selected = NULL)
  })
  
  output$plot <- renderPlot({
    filtered_dataset <- dataset() %>% 
      select(input$date_feature, input$y_feature)
    ggplot(data = filtered_dataset)  +
      geom_point(aes(x = !!sym(input$date_feature), 
                     y = !!sym(input$y_feature)))
  })
}


shinyApp(ui, server)

