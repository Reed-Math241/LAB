library(shiny)
library(tidyverse)
library(DT)
library(india.air)
library(shiny) 
library(lubridate)

india_air <- india_air %>% 
  mutate(date = ymd(date)) %>%
  drop_na(date)

glimpse(india_air)

india_air <- india_air[india_air$city != "Mumbai", ]

# User interface
ui <- fluidPage(
  titlePanel("WSB K-Means Clustering"),
  sidebarLayout(
    sidebarPanel(
      # Create a text input widget
      dateRangeInput("year_range", "Range of Date:",
                     min = "2015-01-01",
                     max = "2020-07-01",
                     start = "2015-01-01",
                     end = "2020-07-01"),
      numericInput('clusters', 'Cluster count', 3, min = 1, max = 9)
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput(outputId = "graph")),
                  tabPanel("Summary", tags$p("The data shows the pollution level of Major Cities in India. You can explore the different factors of pollutions like PM2.5, NO, NO2 and CO."),
                           dataTableOutput(outputId = "table")),
                  tabPanel("Reference", 
                           tags$p("The data were obtained from the",
                                  tags$code("india.air"), "library in R."))
      )
    )
  )
)



# Server function
# Server function
server <- function(input, output, session){
  
  updateSelectizeInput(session, 'names', 
                       choices = unique(india_air$city), 
                       server = TRUE)
  
  dat_names <- reactive({ 
    india_air %>%
      filter(city %in% c(unlist(str_split(input$names, " "))),
             date >= input$year_range[1],
             date <= input$year_range[2]) 
  })
  
  output$graph <- renderPlot({
    
    ggplot(data = dat_names(), 
           mapping = aes(x = date, y = .data[[input$variable]],
                         color = city)) +
      geom_line()
  })
  
  
  dat_names_agg <- reactive({ 
    dat_names() %>%
      group_by(city) %>%
      arrange(city)
  })
  
  output$table <-  renderDataTable({
    datatable(dat_names_agg(), 
              options = list(paging = FALSE,
                             searching = FALSE,
                             orderClasses = TRUE))
  })
}



# Creates app
shinyApp(ui = ui, server = server)