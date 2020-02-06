##############################################
########## Should I Use This Rating ##########
######### Application: Andrew Patton #########
##############################################

#### Packages for App ####
library(knitr)
library(kableExtra)
library(shinythemes)
library(shinycssloaders)
library(DT)
library(colorRamps)
library(grDevices)
library(tidyverse)
library(colorspace)
library(ggrepel)
library(lubridate)

# Define UI for application that draws a histogram

ui <- fluidPage(

    navbarPage("How Not to Abuse Ratings",
               tabPanel("About",
                        mainPanel(h2("blah blah blah"),
                                  htmlOutput("explainer"))
               ),
               tabPanel("Should I Use This Rating?",
                        h4(a("Created by Andrew Patton (@anpatt7)", 
                             style = "color: #1f2024", 
                             href = "https://twitter.com/@anpatt7", 
                             target = "_blank")),
                        h5("words words wwords",
                           style = "color: #991D37"),
                        sidebarLayout(
                            sidebarPanel(
                                numericInput(inputId = "drtg",
                                             label = "DRTG",
                                             value = 100),
                                numericInput(inputId = "ortg",
                                             label = "ORTG",
                                             value = 100),
                                radioButtons(inputId = "count_type",
                                             label = "Possessions or Time?",
                                             choices = c("Possessions",
                                                         "Time")),
                                numericInput(inputId = "count_val",
                                             label = "Number of Poss or Min.",
                                             value = 200),
                                numericInput(inputId = "count_val_test",
                                             label = "How Many Poss or Min. to Test?",
                                             value = 10),
                                actionButton(inputId = "run_test",
                                             label = "Should I Use This Rating?")
                            ),
                            mainPanel(
                                verticalLayout(DT::dataTableOutput("test_table"),
                                               plotOutput('test_table') %>% withSpinner(color = "#44a8f3")
                                )
                                )
                            )
                        )
               )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
