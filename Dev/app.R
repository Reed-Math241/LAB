###########
#Libraries#
###########

library(shiny) # for back end
library(tidyverse) # for 
library(bslib) # for theme
library(stringr) # for string manipulation
library(DT)
library(ggrepel)

tickers <- read_csv("www/tickers.csv")


count_and_sent <- function(df){
  sentiment_of_stock <- function(ticker){
    filtered_rows <- df%>%
      filter(grepl(paste("\\b", ticker, "\\b", sep = ""), title_stocks))
    return(mean(filtered_rows$title_sentiment))
  }
  
  stocks <- df$title_stocks[!is.na(df$title_stocks)]
  stocks <- paste(stocks, sep = " ")
  stocks <- as.list(unlist(strsplit(stocks, '[[:space:]]')))
  stocks <- unlist(stocks)
  grouped <- tibble("stock" = stocks) %>%
    group_by(stock) %>%
    summarise(mentions=n())
  grouped$sentiment <- map_dbl(grouped$stock, sentiment_of_stock)
  return(grouped)
}


wsb <- wsb_dd_submissions %>%
  select(title_stocks, selftext, created_utc) %>% 
  drop_na() %>% 
  unnest_tokens(word, selftext) %>% 
  filter(word != "https"& word != "removed" & word != "amp"& word != "1" & word != "2" & word != "3"& word != "4"& word != "x200b" & word != "png"& word != "01" & word != "12" )


######
#Data#
######

#Section to read in data

data <- read_csv("www/wsb_dd_submissions.csv") 

tickers <- read_csv("www/tickers.csv")

###########
#Functions#
###########


ui <- fluidPage(
  
  theme = bs_theme(bg = "#ffffff", #Setting theme
                   fg = "#000000", 
                   primary = "#273c75", 
                   secondary = "#f1c40f", 
                   base_font = "Arial", 
                   heading_font = "Verdana",
                   bootswatch = "flatly"), 
  
  
  # Application title
  navbarPage("WSB DD dashboard: [PUN HERE]",
             tabPanel("Stocks Sentiment Clustering",
                      titlePanel("Stocks Sentiment Clustering"),
                      sidebarLayout(
                        sidebarPanel(
                          dateRangeInput("clusterCreateRange",
                                         "Created Range:",
                                         min = "2018-01-01",
                                         max = "2021-12-31",
                                         start = "2021-04-29",
                                         end = "2021-05-04"),
                          numericInput('clusters', 
                                       'Cluster count', 
                                       3, 
                                       min = 1, 
                                       max = 9)
                        ), #end sidebar panel
                        
                        mainPanel(
                          plotOutput("cluster_graph")
                        )
                      ) # end SIdebar layou
             ), #end StockSentiment Clustering panel
             
             tabPanel("Table",
                      titlePanel("Table"),
                      ######################
                      #Sidebar layout for DF
                      ######################
                      sidebarLayout(
                        sidebarPanel(
                          textInput("title",
                                    "Post Title: "),
                          selectizeInput("author",
                                         "Author: ",
                                         choices = NULL,
                                         multiple = TRUE),
                          sliderInput("scoreRange", "Post Score:",
                                      min = 0, max = 5700,
                                      value = c(0, 5700),
                                      sep=""),
                          dateRangeInput("createRange",
                                         "Created Range:",
                                         min = "2018-01-01",
                                         max = "2021-12-31",
                                         start = "2018-08-02",
                                         end = "2021-12-31"),
                          selectizeInput("titleStocks",
                                         "Title Stocks: ",
                                         choices = NULL,
                                         multiple = TRUE),
                          selectizeInput("postStocks",
                                         "Post Stocks: ",
                                         choices = NULL,
                                         multiple = TRUE),
                          sliderInput("titleSentiment", "Title Sentiment:",
                                      min = -20, max = 16,
                                      value = c(-20, 16),
                                      sep=""),
                          sliderInput("postSentiment", "Post Sentiment:",
                                      min = -240, max = 260,
                                      value = c(-240, 260),
                                      sep=""),
                          checkboxInput("onlySelftext",
                                        "Only posts with selftext",
                                        FALSE)
                        ),
                        ##############
                        #Main panel DF
                        ##############
                        mainPanel(
                          tabPanel("Table", 
                                   DTOutput("table")
                          )
                        )
                        
                      ) #End navbar page
             )
             
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  updateSelectizeInput(session, 'titleStocks', 
                       choices = tickers$Symbol, 
                       server = TRUE)
  updateSelectizeInput(session, 'postStocks', 
                       choices = tickers$Symbol, 
                       server = TRUE)
  updateSelectizeInput(session, 'author', 
                       choices = unique(data$author), 
                       server = TRUE)
  
  table_clean <- reactive({
    filtered <- data %>%
      subset(select=-c(permalink, coin_awards, num_comments)) %>%
      mutate(created_utc = strptime(created_utc, format="%s"))
    
    if(length(input$titleStocks)>0){ #filters title stocks
      input_regex_clean <- str_c("\\b", input$titleStocks, "\\b")
      input_regex_clean <- paste(input_regex_clean, collapse = "|")
      filtered <- filtered %>%
        filter(grepl(input_regex_clean, title_stocks))
    }
    if(length(input$postStocks)>0){
      input_regex_clean <- str_c("\\b", input$postStocks, "\\b")
      input_regex_clean <- paste(input_regex_clean, collapse = "|")
      filtered <- filtered %>%
        filter(grepl(input_regex_clean, post_stocks))
    }
    if(length(input$author)>0){
      input_regex_clean <- str_c("\\b", input$author, "\\b")
      input_regex_clean <- paste(input_regex_clean, collapse = "|")
      filtered <- filtered %>%
        filter(grepl(input_regex_clean, author))
    }
    if(input$title!=""){
      filtered <- filtered %>%
        filter(grepl(tolower(input$title), tolower(title)))
    }
    
    filtered <- filtered %>%
      filter(score >= input$scoreRange[1],
             score <= input$scoreRange[2],
             post_sentiment >= input$postSentiment[1],
             post_sentiment <= input$postSentiment[2],
             title_sentiment >= input$titleSentiment[1],
             title_sentiment <= input$titleSentiment[2],
             created_utc >= as.Date(input$createRange[1]),
             created_utc <= as.Date(input$createRange[2])) %>%
      mutate(created_utc = strftime(created_utc, format="%Y-%m-%d %H:%M:%S"))
    
    
    return(as.data.frame(filtered))
    
  })
  
  
  output$table <- renderDT(table_clean()) # Name of table needed, displays table
  
  cluster_clean <- reactive({
    clean_data <- data %>%
      mutate(created_utc = strptime(created_utc, format="%s")) %>%
      filter(created_utc >= as.Date(input$clusterCreateRange[1]),
             created_utc <= as.Date(input$clusterCreateRange[2]),
             title_sentiment!=0) %>%
      count_and_sent() 
    clean_data_num <- clean_data %>% 
      select(sentiment, mentions)
    
    df <- clean_data_num
    df <- na.omit(df)
    df <- scale(df)
    kmeans <- kmeans(df, centers = input$clusters, nstart = 25)
    df_clustered <- clean_data %>%
      mutate(cluster = as.factor(kmeans$cluster))
    return(as.data.frame(df_clustered))
  }) # end cluster clean
  
  output$cluster_graph <- renderPlot({
    cluster_clean()  %>%
      ggplot(aes(x = mentions, y = sentiment, color = cluster)) +
      geom_point() +
      scale_y_log10()+
      scale_x_log10()+
      geom_text_repel(aes(label =stock), size = 3.5)
    
  })# end of cluster_graph server
  
  output$wordcloud <- renderDT(table_clean()) # Name of table needed, displays table
  
  cluster_clean <- reactive({
    clean_data <- data %>%
      mutate(created_utc = strptime(created_utc, format="%s")) %>%
      filter(created_utc >= as.Date(input$clusterCreateRange[1]),
             created_utc <= as.Date(input$clusterCreateRange[2]),
             title_sentiment!=0) %>%
      count_and_sent() 
    clean_data_num <- clean_data %>% 
      select(sentiment, mentions)
    
    df <- clean_data_num
    df <- na.omit(df)
    df <- scale(df)
    kmeans <- kmeans(df, centers = input$clusters, nstart = 25)
    df_clustered <- clean_data %>%
      mutate(cluster = as.factor(kmeans$cluster))
    return(as.data.frame(df_clustered))
  }) # end cluster clean
  
  output$cluster_graph <- renderPlot({
    cluster_clean()  %>%
      ggplot(aes(x = mentions, y = sentiment, color = cluster)) +
      geom_point() +
      scale_y_log10()+
      scale_x_log10()+
      geom_text_repel(aes(label =stock), size = 3.5)
    
  })# end of cluster_graph server
  
  
  
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
