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

data <- read_csv("www/wsb_dd_submissions.csv") %>%
    head(15)

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
               tabPanel("Analysis",
                        titlePanel("Analysis"),
                        
                        ),
               tabPanel("Posts",
                        titlePanel("Posts"),
                        sidebarLayout(
                            sidebarPanel(),
                            mainPanel(
                                tabsetPanel(
                                    tabPanel("graph", plotOutput("holder_graph")) # name of graph and out put of graph
                                ) # end `tabsetPanel`
                                ) # end `mainPanel`
                        ) #end `sidebarLayout`
               ), #end `tabPanel "posts"`
               
               
               tabPanel("By Author",
                        titlePanel("Author"),
                        sidebarLayout(
                            sidebarPanel( 
                                textInput("author",
                                          "Author: ")
                            ), # End `sidebarPanel`
                            mainPanel(
                                tabPanel("graph", plotOutput("holder_graph")) # name of graph and out put of graph
                            ) #end
                        ), #end `tabPanel`
               
               ),
               
               tabPanel("Table",
                        titlePanel("Table"),
                        sidebarLayout(
                            sidebarPanel(
                                textInput("post_name",
                                          "Post Title: ")
                            ),
                            mainPanel(
                                DTOutput("table")
                            )
                        ) #end `sidebarLayout`
               )
               
    ) #End navbar page

)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$holder_graph <- renderPlot({
        print("imma rendering")
        data %>%
            ggplot(aes(score, num_comments)) +
            geom_point()
    })
    output$table <- renderDT(data) # Name of table needed, displays table
}

# Run the application 
shinyApp(ui = ui, server = server)
