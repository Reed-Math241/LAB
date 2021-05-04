library(shiny)
library(tidyverse)
library(DT)
library(india.air)
library(shiny) 
library(lubridate)

wsb_dd_submissions <- read_csv("../WSB-viz/www/wsb_dd_submissions.csv") %>% 
  mutate(ticker=strsplit(title_stocks, " ")) %>% 
  unnest(ticker) %>% 
  drop_na(ticker)


vars <- setdiff(names(iris), "Species")

pageWithSidebar(
  headerPanel('Iris k-means clustering'),
  sidebarPanel(
    selectInput('xcol', 'X Variable', vars),
    selectInput('ycol', 'Y Variable', vars, selected = vars[[2]]),
    numericInput('clusters', 'Cluster count', 3, min = 1, max = 9)
  ),
  mainPanel(
    plotOutput('plot1')
  )
)

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
      
    )




# Server function
server <- function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    iris[, c(input$sentiment, input$count)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  userSelectDate2 <- wsb_dd_submissions %>%
    filter(date >= as.Date("2021-01-01") & date <= as.Date("2021-01-05")) %>%
    filter(post_sentiment != 0)
  
  sentiment <- userSelectDate2 %>%
    group_by(ticker) %>%
    summarise(sentiment = sum(post_sentiment)) %>% 
    drop_na()
  
  mentionedTimes <- userSelectDate2 %>%
    group_by(date) %>%
    count(ticker) %>% 
    group_by(ticker) %>%
    summarise(count = sum(n))
  
  kmeanData <- merge(sentiment, mentionedTimes, by = "ticker") 
  
  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
}



# Creates app
shinyApp(ui = ui, server = server)