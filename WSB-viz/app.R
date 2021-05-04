###########
#Libraries#
###########

library(shiny) # for back end
library(tidyverse) # for 
library(bslib) # for theme
library(stringr) # for string manipulation
library(DT)


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
                                               min = "2018-08-02",
                                               max = "2020-07-31",
                                               start = "2018-08-02",
                                               end = "2020-07-31"),
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
                         choices = unique(wsb$author), 
                         server = TRUE)
    
    table_clean <- reactive({
        filtered <- data %>%
            subset(select=-c(permalink)) %>%
            mutate(created_utc = strptime(created_utc, tz = "GMT", format="%s"))
        
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
                filter(grepl(input$title, title))
        }
        
        filtered <- filtered %>%
            mutate(created_utc = strftime(created_utc, tz = Sys.timezone, format="%c")) #%>%
            #filter(score %in% c(scoreRange[1]:scoreRange[2]))
        
        return(as.data.frame(filtered))
        
    })
    
    output$holder_graph <- renderPlot({
        print("imma rendering")
        data %>%
            ggplot(aes(score, num_comments)) +
            geom_point()
    })
    output$table <- renderDT(table_clean()) # Name of table needed, displays table
    
    output$live <- renderPlot({data %>%
            ggplot(aes(score, num_comments)) +
            geom_point()})
}

# Run the application 
shinyApp(ui = ui, server = server)
