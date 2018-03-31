setwd("/Users/KokiAndo/Desktop/R/Shiny app/whisky")
library(tidyverse)
library(data.table)
library(shiny)
library(DT)
library(rlang)

whisky <- fread("http://outreach.mathstat.strath.ac.uk/outreach/nessie/datasets/whiskies.txt", data.table = FALSE)
whisky$Longitude <- as.numeric(whisky$Longitude)
whisky$Latitude <- as.numeric(whisky$Latitude)


# whisky : Global Data (not including location data)
whisky <- whisky %>% select(Distillery:Floral)

# whisky.score : tidy whisky data
whisky.score <- whisky %>% 
  gather(key = Review.point, value = Score, Body:Floral)


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
     selectInput(
       inputId = "FirstPref",
       label = "Select your 1st Preference:",
       choices = names(whisky %>% select(-Distillery)),
       selected = "Body"
       ),
     selectInput(
       inputId = "SecondPref",
       label = "Select your 2nd Preference:",
       choices = names(whisky %>% select(-Distillery)),
       selected = "Sweetness"
       ),
     selectInput(
       inputId = "ThirdPref",
       label = "Select your 3rd Preference:",
       choices = names(whisky %>% select(-Distillery)),
       selected = "Smoky"
     )),
    mainPanel(
      plotOutput(outputId = "score"),
      DT::dataTableOutput(outputId = "table"),
      tableOutput(outputId = "table2")
      )
    )
)

server <- function(input, output){
  
  selected_whisky <- reactive({
    req(input$FirstPref)
    req(input$SecondPref)
    whisky %>% 
      group_by(Distillery) %>% 
      # arrange(desc(input$FirstPref), desc(input$SecondPref)) %>% 
      # arrange(UQ('input$FirstPref')) %>% 
      #arrange(UQ(sym('input$FirstPref'))) %>% 
      # arrange(!!sym(UQE(my_col))) %>% 
      arrange(desc(!!sym(UQE(input$FirstPref))), 
              desc(!!sym(UQE(input$SecondPref))),
              desc(!!sym(UQE(input$ThirdPref)))) %>% 
      head(5)
  })
  
  selected_whisky_score <- reactive({
    selected_whisky() %>% 
      gather(key = Review.point, value = Score, Body:Floral)
  })
  
  output$score <- renderPlot(
    
    selected_whisky_score() %>% 
      ggplot(aes(x=Review.point, y = Score, fill = Review.point)) + 
      geom_bar(stat = "identity") + 
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank()) + 
      facet_wrap(~ Distillery)
  
  )
  
  
  # output$table <- renderDataTable({
  #   DT::datatable(
  #     data = selected_whisky(),
  #     option = 
  #       list(lengthMenu = c(3, 5), 
  #            pageLength = 5),
  #     rownames = FALSE
  #   )
  # })
  
  output$table2 <- renderTable(selected_whisky())
  
  }

shinyApp(ui = ui, server = server)
