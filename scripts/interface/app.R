library(shiny)
library(readr)
library(dplyr)
library(shinydashboard)
library(htmlwidgets)
library(shinythemes)
library(plotly)
library(shinycssloaders)
library(ggplot2)
library(ggExtra)
library(DT)


COLOR = '#16536a'

merged_df = read.csv('../../datasets_nasa/cleaned_datasets/merged_df.csv')

ui = bootstrapPage(
  tags$link(
    rel="stylesheet", 
    href="https://fonts.googleapis.com/css2?family=Quicksand:wght@300&display=swap" 
  ),
  tags$style(HTML('
      .box {
        # border: 1px solid #16536a;
        #border-radius: 0.25rem;
        #padding: 1rem;
        margin-bottom: 1rem;
        box-shadow: rgba(0, 0, 0, 0.35) 0px 5px 15px;
        width: 100%;
        text-align: center;
        height:40%
        
      }
      .boxText {
        # border: 1px solid #16536a;
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
        # border: 1px solid #16536a;
        #border-radius: 0.5rem;
        #padding: 1rem;
        margin-bottom: 1rem;
        box-shadow: rgba(0, 0, 0, 0.35) 0px 5px 15px;
        width: 100%;
        height:90%;
        font-family: "Quicksand", sans-serif;
        font-size: 18px;
      }
      
      .box-header {
        background-color: #16536a;
        color: white;
        padding: 0.5rem 1rem;
        border-top-left-radius: 0.25rem;
        border-top-right-radius: 0.25rem;
        font-family: "Quicksand", sans-serif;
        font-size: 16px;
        font-weight: bold;
        text-align: left;
      }
    ')),
  
  navbarPage(theme = shinytheme('flatly'),"Global Warming Dashboard", id="main",
             tabPanel("Descriptive Analysis", 
                      fluidRow(
                        column(width = 5, 
                               sidebarPanel(width = '100%',
                                 selectInput("dataset", "Choose a dataset:", 
                                             choices = c("Antartica mass" = "../../datasets_nasa/cleaned_datasets/antarctica_mass_clean.csv", 
                                                         "Carbone dioxyde" = "../../datasets_nasa/cleaned_datasets/co2_clean.csv", 
                                                         "Global temperature" = "../../datasets_nasa/cleaned_datasets/global_temp_clean.csv",
                                                         "Greenland mass" = "../../datasets_nasa/cleaned_datasets/greenland_mass_clean.csv",
                                                         "sept_artic_extend" = "../../datasets_nasa/cleaned_datasets/sept_artic_extend_clean.csv" 
                                             )),
                                 selectInput("date_feature", "Choose a time feature:", 
                                             choices = NULL),
                                 
                                 selectInput("y_feature", "Choose a Y feature:", 
                                             choices = NULL)
                                 
                               )
                               ),
                        
                        
                        
                        column(width = 7,
                               div(class = "box", 
                                   div(class = "box-header", "Time series"),
                                   div(style="padding-left: 20px",
                                       checkboxInput('comparison_bool', value = FALSE, label = 'Comparison'),
                                       selectInput('datasetVS', 'Choose a feature to compare', 
                                                 choices = c("Antartica mass" = "../../datasets_nasa/cleaned_datasets/antarctica_mass_clean.csv", 
                                                             "Carbone dioxyde" = "../../datasets_nasa/cleaned_datasets/co2_clean.csv", 
                                                             "Global temperature" = "../../datasets_nasa/cleaned_datasets/global_temp_clean.csv",
                                                             "Greenland mass" = "../../datasets_nasa/cleaned_datasets/greenland_mass_clean.csv",
                                                             "sept_artic_extend" = "../../datasets_nasa/cleaned_datasets/sept_artic_extend_clean.csv" )),
                                   ),
                                   plotlyOutput("plot")
                               ),
                               
                               div(class = "box", #style = "max-height: 500px; overflow-y: scroll;",
                                   div(class = "box-header", "Explore"),
                                   tabsetPanel(
                                     tabPanel('Head',
                                              verbatimTextOutput("head")
                                     ),
                                     tabPanel('Boxplot',
                                              fluidRow(
                                                column(width = 5,
                                                       div(style="padding: 20px",
                                                       selectInput("variable", "Choose a variable", choices = NULL)
                                                       )),
                                                column(width = 7,
                                                       plotlyOutput("boxplot", height = "300px", width = "100%")
                                                       )

                                              )
                                              
                                     ),
                                     tabPanel('Summary',
                                              verbatimTextOutput("summary")
                                     )
                                   )
                               )
                        )
                      )

             ),
             
             tabPanel("Predictor", 
             ),
             tabPanel("Data Explorer", 
                      sidebarPanel(
                        selectInput("dataset_table", "Select Dataset", 
                                    choices = c("Antartica mass" = "../../datasets_nasa/cleaned_datasets/antarctica_mass_clean.csv", 
                                                "Carbone dioxyde" = "../../datasets_nasa/cleaned_datasets/co2_clean.csv", 
                                                "Global temperature" = "../../datasets_nasa/cleaned_datasets/global_temp_clean.csv",
                                                "Greenland mass" = "../../datasets_nasa/cleaned_datasets/greenland_mass_clean.csv",
                                                "Carbone dioxyde" = "../../datasets_nasa/cleaned_datasets/sept_artic_extend_clean.csv" 
                                    ))
                      ),
                      DT::DTOutput("table") %>% withSpinner(color='#16536a')
                      
                      ),
             
             tabPanel("Learn More",
                      includeMarkdown('../../README.md')
             )
  )
)






server <- function(input, output, session){
  
  dataset = reactive({
    read.csv(input$dataset)
  })
  
  
  datasetVS = reactive({
    read.csv(input$datasetVS)
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
  
  
  
  output$plot <- renderPlotly({
    
    
    # if(input$dataset == "../../datasets_nasa/cleaned_datasets/antarctica_mass_clean.csv"){
    #   feature_dataset1 = Antartica_mass
    # }
    
    
    
    if(input$comparison_bool[1] == FALSE){
        filtered_dataset = dataset() %>%
          select(input$date_feature, input$y_feature)
        ggplotly(ggplot(data = filtered_dataset)  +
          geom_point(aes(x = !!sym(input$date_feature),
                         y = !!sym(input$y_feature)), color = COLOR )
          )%>%
          config(displayModeBar = F)
    
    # }else{
    #   if(input$datasetVS == "../../datasets_nasa/cleaned_datasets/antarctica_mass_clean.csv"){
    #     filtered_dataset =merged_df %>%
    #       select(date, input$y_feature, monthly_average)
    #     ggplotly(ggplot(merged_df) +
    #                geom_line(aes(x = date,
    #                              y = feature_dataset1, color = "Line 1")) +
    #                geom_line(aes(x = date,
    #                              y = monthly_average, color = "Line 2")) +
    #                
    #                
    #               
    #                # scale_y_continuous(name = "Target 1", limits = c(-3000, 500),
    #                #                    sec.axis = sec_axis(~.*100, name = "Target 2")) +
    #                theme_bw())
    #     
    #     
    #     
    #   }
    #   if(input$datasetVS == "../../datasets_nasa/cleaned_datasets/co2_clean.csv"){
    #     ggplotly(ggplot(merged_df, aes(date)) +
    #                geom_line(aes(y = input$y_feature), color = "black") +
    #                geom_line(aes(y = monthly_average), color = "red") +
    #                # scale_y_continuous(name = "Target 1", limits = c(-3000, 500),
    #                #                    sec.axis = sec_axis(~.*100, name = "Target 2")) +
    #                theme_bw())
    #   }
    #   if(input$datasetVS == "../../datasets_nasa/cleaned_datasets/global_temp_clean.csv"){
    #     ggplotly(ggplot(merged_df, aes(date)) +
    #                geom_line(aes(y = input$y_feature), color = "black") +
    #                geom_line(aes(y = No_Smoothing), color = "red") +
    #                # scale_y_continuous(name = "Target 1", limits = c(-3000, 500),
    #                #                    sec.axis = sec_axis(~.*100, name = "Target 2")) +
    #                theme_bw())
    #   }
    #   if(input$datasetVS == "../../datasets_nasa/cleaned_datasets/greenland_mass_clean.csv"){
    #     ggplotly(ggplot(merged_df, aes(date)) +
    #                geom_line(aes(y = input$y_feature), color = "black") +
    #                geom_line(aes(y = Greenland_mass), color = "red") +
    #                # scale_y_continuous(name = "Target 1", limits = c(-3000, 500),
    #                #                    sec.axis = sec_axis(~.*100, name = "Target 2")) +
    #                theme_bw())
    #   }
    #   if(input$datasetVS == "../../datasets_nasa/cleaned_datasets/sept_artic_extend_clean.csv"){
    #     ggplotly(ggplot(merged_df, aes(date)) +
    #                geom_line(aes(y = input$y_feature), color = "black") +
    #                geom_line(aes(y = extent), color = "red") +
    #                # scale_y_continuous(name = "Target 1", limits = c(-3000, 500),
    #                #                    sec.axis = sec_axis(~.*100, name = "Target 2")) +
    #                theme_bw())
    #   }
    #   
      
      
      
    }
    # ggplotly(ggplot(merged_df, aes(date)) +
    #            geom_line(aes(y = Antarctic_mass), color = "black") +
    #            geom_line(aes(y = `monthly average` / 100), color = "red") +
    #            scale_y_continuous(name = "Target 1", limits = c(-3000, 500),
    #                               sec.axis = sec_axis(~.*100, name = "Target 2")) +
    #            labs(x = "Date", y = "Target", title = "Targets over time") +
    #            theme_bw())
  })
 
  

  
  
  
  
  
  # ================= 2nde box DROITE ======================#
  output$summary <- renderPrint({
    summary(dataset())
  })
  
  output$head <- renderPrint({
    head(dataset())
  })
  

  # ============ BOXPLOT ============ #
  observeEvent(input$dataset, {
    updateSelectInput(session, "variable", 
                      choices = colnames(dataset()), 
                      selected = NULL)
  })
  
  # Output boxplot
  output$boxplot <- renderPlotly({
    plot_ly(dataset(), y = as.formula(paste0("~", input$variable)), type = "box", marker = list(color = '#16536a')
            ) %>% 
      layout(title = input$variable, plot_bgcolor = '#f5f5f5',
             paper_bgcolor = '#f5f5f5') %>%
      config(displayModeBar = F)
  })
  
  
  
  
  ###########################################################################
  # ========================= ONGLET DATA EXPLORER ======================== #
  ###########################################################################
  
  dataset2 <- reactive({
    read.csv(input$dataset_table)
  })
  
  output$table <- DT::renderDT(dataset2(), filter = 'top', options = list(pageLenght = 15))
  
}




shinyApp(ui, server)

