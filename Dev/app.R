library(shiny)
library(tidyverse)
library(DT)
library(shiny) 
library(lubridate)
library(cluster)    # clustering algorithms
library(factoextra)
library(ggrepel)
library(readr)


wsb_dd_submissions <- read_csv("Dev/WrangledData/wsb_dd_submissions.csv")
count_ticker <- read_csv("Dev/WrangledData/count_ticker.csv")

count_and_sent <- function(df){
  
  sentiment_of_stock <- function(ticker){
    filtered_rows <- df%>%
      filter(grepl(paste("\\b", ticker, "\\b"), title_stocks))
    return(mean(filtered_rows$title_sentiment))
  }
  
  stocks <- df$title_stocks[!is.na(df$title_stocks)]
  stocks <- paste(stocks, sep = " ")
  stocks <- unlist(as.list(unlist(strsplit(stocks, '[[:space:]]'))))
  stocks <- stocks[-(0:30)]
  
  grouped <- tibble("stock" = stocks) %>%
    group_by(stock) %>%
    summarise(mentions=n())
  
  grouped$sentiment <- unlist(map(grouped$stock, sentiment_of_stock))
  grouped$sentiment[is.nan(grouped$sentiment)] <- 0 
  
  return(grouped)
}


#ui
vars <- setdiff(names(wsb_dd_submissions), "ticker")

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

#server

function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    iris[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
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