###########
#Libraries#
###########

library(shiny) # for back end
library(tidyverse) # for 
library(bslib) # for theme
library(stringr) # for string manipulation
library(DT)
library(ggrepel)
library(viridis)
library(wordcloud) 
library(tidytext)

# clustering plot helper function
count_and_sent <- function(df){
    sentiment_of_stock <- function(ticker){
        filtered_rows <- df %>%
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

# bar plot helper function
get_top10 <- function(df){
    top10 <- df %>%
        group_by(ticker) %>%
        summarise(count = sum(n)) %>%
        arrange(desc(count)) %>%
        slice(1:10)
    return(top10)
}

######
#Data#
######

#Section to read in data

data <- read_csv("www/wsb_dd_submissions.csv") %>%
    mutate(created_utc = strptime(created_utc, format="%s"))

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
    navbarPage("WSB DD dashboard: STONKS ONLY GO UP",
               tabPanel("Stock Sentiment",
                        titlePanel("Stocks Sentiment Cluster"),
                        sidebarLayout(
                            sidebarPanel(
                                dateRangeInput("clusterCreateRange",
                                               "Created Range:",
                                               min = "2018-01-01",
                                               max = "2021-12-31",
                                               start = "2021-04-04",
                                               end = "2021-05-04"),
                                numericInput('clusters', 
                                             'Cluster count', 
                                             4, 
                                             min = 1, 
                                             max = 9),
                                submitButton("Change Output")
                            ), #end sidebar panel
                            
                            mainPanel(
                                plotOutput("cluster_graph")
                            )
                        ) # end SIdebar layout
               ), # End StockSentiment Clustering panel
               
               # bar plot page
               tabPanel("Popular Stocks",
                        titlePanel("Most Discussed Stocks"),
                        sidebarLayout(
                            sidebarPanel(
                                dateRangeInput("barCreateRange",
                                               "Created Range:",
                                               min = "2018-01-01",
                                               max = "2021-12-31",
                                               start = "2021-04-29",
                                               end = "2021-05-04"),
                                submitButton("Change Output")
                            ), #end sidebar panel
                            mainPanel(
                                plotOutput("bar_graph")
                            )
                        ) # end Sidebar layou
               ), #end bar plot panel
               
               # word cloud plot page
               tabPanel("Word Cloud",
                        titlePanel("Stock Key Words"),
                        sidebarLayout(
                            sidebarPanel(
                                dateRangeInput("cloudCreateRange",
                                               "Created Range:",
                                               min = "2018-01-01",
                                               max = "2021-12-31",
                                               start = "2021-03-28",
                                               end = "2021-04-29"),
                                submitButton("Change Range"),
                                selectizeInput("StockTicker",
                                               "Stock Ticker: ",
                                               choices = NULL,
                                               multiple = FALSE),
                                submitButton("Change Output")
                            ), #end sidebar panel
                            mainPanel(
                                plotOutput("cloud_graph")
                            )
                        ) # end Sidebar layou
               ), #end word cloud plot panel
               
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
                            
                        ) #End sidebarLayout
               ), # End table page
               tabPanel(
                   "Developers & Sources",
                   tags$p(
                       "This app was created by",
                       tags$a("Taylor Blair",
                              href = "https://github.com/Goodernews",
                              taget = "_blank"),
                       ",  ",
                       tags$a("Jiarong Li",
                              href = "https://github.com/jialicatherine",
                              taget = "_blank"),
                       ", and",
                       tags$a("Sung Bum (Simon) Ahn",
                              href = "https://github.com/ahnsb5117",
                              taget = "_blank"),
                       "as a final project for Math 241: Data Science at Reed College in Spring 2021.
      The goal of this app is to allow Wall Street Bets enthusiasts to explore and visualize their stocks."
                   ),
                   tags$p(
                       "The data used for this app was primarily pulled and scraped from Reddit subreddit",
                       tags$a("r/wallstreetbets Due Diligence(DD),",
                              href = "https://www.reddit.com/r/wallstreetbets/?f=flair_name%3A%22DD%22",
                              target = "_blank"),
                       "and tools were used from packages such as",
                       tags$code("bslib"),
                       ", ",
                       tags$code("wordcloud"),
                       ", and",
                       tags$code("tidyverse"),
                       ".  The app was last updated in Spring 2021"
                   )
               )
               
    ) # End navbar Page
) # End fluid Page


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    # Table server
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
    # End Table server
    
    # Sentiment server
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
            geom_text_repel(aes(label =stock), size = 3.5) +
            xlab("mentioned times") +
            theme_minimal() 

        }) # end of cluster_graph server
    # End Sentiment Server
    
    # bar Server
    bar_data <- reactive({
        clean_data <- data %>%
            mutate(created_utc = strptime(created_utc, format="%s")) %>%
            filter(created_utc >= as.Date(input$clusterCreateRange[1]),
                   created_utc <= as.Date(input$clusterCreateRange[2])) %>%
            mutate(ticker=strsplit(title_stocks, " ")) %>% 
            unnest(ticker) %>% 
            drop_na(ticker) %>%
            group_by(created_utc) %>%
            count(ticker) %>%
            get_top10()
        return(as.data.frame(clean_data))
    }) # end bar data
    
    output$bar_graph <- renderPlot({
        bar_data() %>%
            ggplot(aes(x = ticker, y = count, fill = count)) + 
            geom_col() +
            theme_minimal() 
    })
    # End bar Server

    # Word Cloud Server
#    updateSelectizeInput(session, 'StockTicker',
#                         choices = (unique(tickers$Symbol)),
#                        server = TRUE)
    
    cloud_data <- reactive({
        clean_data <- data %>%
            filter(created_utc >= as.Date(input$cloudCreateRange[1]),
                   created_utc <= as.Date(input$cloudCreateRange[2]),
                   !is.na(post_stocks)) %>%
            mutate(ticker=strsplit(title_stocks, " ")) %>% 
            unnest(ticker) %>% 
            select(ticker, selftext, created_utc) %>%
            drop_na() %>%
            unnest_tokens(word, selftext) %>%
            filter(word != "https"& word != "removed" & word != "amp" & word != "png" & word != "jpg" & word != "pjpg" & word != "preview.redd.it" & word != "webp" & word != "auto" ) %>%
            mutate(TF = grepl("\\d",word)) %>%
            filter(TF != TRUE)
        return(clean_data)
    }) # end cloud clean
    
    observeEvent(input$cloudCreateRange, {
      updateSelectizeInput(session, 'StockTicker',
                         choices = as.character(unique(cloud_data()$ticker)),
                         server = TRUE)
    })
    
    cloud_data2 <- reactive({
        clean_data <- cloud_data() %>%
          filter(ticker == input$StockTicker) %>%
          anti_join(stop_words, by = c("word" = "word")) %>%
          count(word) %>%
          arrange(desc(n)) %>%
          slice(1:50)
        return(as.data.frame(clean_data))
    }) # end cloud clean
    
    output$cloud_graph <- renderPlot({
            wordcloud(words=cloud_data2()$word,
                  freq = cloud_data2()$n,
                  min.freq = 1, max.words=20, random.order=FALSE,
                  rot.per=0.2, colors=brewer.pal(5, "Dark2"))
    })
    # End Word Cloud Server
}

# Run the application 
shinyApp(ui = ui, server = server)
