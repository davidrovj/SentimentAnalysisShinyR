library(shiny)
library(shinythemes)
# Define UI for application that draws a histogram
ui <- fluidPage(
    
    theme = shinytheme("cerulean"),
    
    # Application title
    titlePanel(
            h1(style = "color:#3bb0df;", "Tweets Consult for Sentiment Analysis", align = "center")
            ),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput(inputId = "user1",
                      label = h4(style = "color: #3bb0df;","Insert the users you want to analyze!"),
                      value = "@"
                      ),
            
            textInput(inputId = "user2",
                      label = p(""),
                      value = "@"
            ),
            
            # selectInput(inputId = "stopwords",
            #             label = h4(style = "color: #3bb0df","Select the language of the tweets:"),
            #             c("english","spanish", "german"), 
            #             selected = 1),
            
            actionButton(inputId = "analyze", label = "Analyze")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            
            tabsetPanel(
                tabPanel("Correlation",
                         wellPanel(h1(style= "color:#556586; text-align:center;", "Correlation:"),
                                   plotOutput("corr", width = "100%", hover = "Users correlation")
                                   )
                         ),
                tabPanel("Common Words",
                         wellPanel(h3(style = "color:#556586; text-align:center;", 
                                      "Common words between users"),
                                   htmlOutput(style = "color:#556586; text-align:center; font-size:30px",
                                              outputId = "common")
                                   )
                         ),
                tabPanel("General Sentiment Graph",
                         wellPanel(h4(style = "color:#556586; text-align:center;", 
                                      "Users Sentiments Graphic"),
                                   plotOutput("sentiments", width = "100%", hover = "Users sentiment")
                         )),
                tabPanel("Sentiments by date",
                         wellPanel(h4(style = "color:#556586; text-align:center;",
                                      "Users sentiments by time periods "),
                                   p(style = "color:#556586; text-align:center;", 
                                     "0 = Max Negative and 1 = Max Positive"),
                                   plotOutput("date1", width = "100%", hover = "Users sentiment by date"),
                                   p(),
                                   plotOutput("date2", width = "100%", hover = "Users sentiment by date")
                                   )
                         ),
                tabPanel("Positive and Negative",
                         wellPanel(h4(style = "color:#556586; text-align:center;",
                                      "Most used Positive and Negative words"),
                                   h5(style = "color:#556586; text-align:center;","Positive Words"),
                                   plotOutput("pos", width = "100%", 
                                              hover= "Users positive words"),
                                   h5(style = "color:#556586; text-align:center;","Negative Words"),
                                   plotOutput("neg", width = "100%", 
                                              hover= "Users negative words")
                                   )
                         )
                
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    result <- reactiveValues(variable = NA)
    
    observe({if(input$analyze > 0){
               result$user1 <- isolate(as.character(input$user1))
               result$user2 <- isolate(as.character(input$user2))
               
               
               result$tweets <- sentiment_analysis(result$user1,result$user2)
            }
    })
    output$corr <- renderPlot({result$tweets[1]})
    output$common <- renderText({paste(result$tweets[2])})
    output$sentiments <- renderPlot({result$tweets[3]})
    output$date1 <- renderPlot({result$tweets[4]})
    output$date2 <- renderPlot({result$tweets[7]})
    output$pos <- renderPlot({result$tweets[5]})
    output$neg <- renderPlot({result$tweets[6]})
}

# Run the application 
shinyApp(ui = ui, server = server)
