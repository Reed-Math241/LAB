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

stock_names <- read_csv("www/merged_common.csv")

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
                                textInput("author",
                                          "Author: "),
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
                                sliderInput("titleSentiment", "Title Sentiment:",
                                            min = -20, max = 16,
                                            value = c(-20, 16),
                                            sep=""),
                                sliderInput("tpostSentiment", "Post Sentiment:",
                                            min = -240, max = 260,
                                            value = c(-240, 260),
                                            sep=""),
                                checkboxInput("onlySelftext",
                                              "Only posts with selftext",
                                              FALSE),
                                checkboxInput("unreturned",
                                              "Only unreturned",
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
server <- function(input, output) {
    updateSelectizeInput(session, 'stockTickers', 
                         choices = unique(stock_names$Symbol), 
                         server = TRUE)
    
    output$holder_graph <- renderPlot({
        print("imma rendering")
        data %>%
            ggplot(aes(score, num_comments)) +
            geom_point()
    })
    output$table <- renderDT(data %>%
                                 subset(select=-c(permalink)) %>%
                                 mutate(created_utc = strptime(created_utc, format="%s"))
                             ) # Name of table needed, displays table
    
    output$live <- renderPlot({data %>%
            ggplot(aes(score, num_comments)) +
            geom_point()})
}

# Run the application 
shinyApp(ui = ui, server = server)
