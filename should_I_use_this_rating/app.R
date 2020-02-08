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
              # tabPanel("About",
               #         mainPanel(h2("blah blah blah"),
                #                  htmlOutput("explainer"))
               #),
               tabPanel("Should I Use This Rating?",
                        h4(a("Created by Andrew Patton (@anpatt7)", 
                             style = "color: #1f2024", 
                             href = "https://twitter.com/@anpatt7", 
                             target = "_blank")),
                        h4(a("Explanation by Ryan Davis (@rd11490) ", 
                             style = "color: #1f2024", 
                             href = "https://github.com/rd11490/NBA_Tutorials/tree/master/five_man_net_rating", 
                             target = "_blank")),
                        #h2(a("Temporarily Down Because Andrew Did Something Stupid")),
                        h5("Enter the current rating information and how many min/poss to add.",
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
                                             label = "Possessions or Minutes?",
                                             choices = c("Possessions",
                                                         "Minutes")),
                                numericInput(inputId = "count_val",
                                             label = "Number of Poss or Min. Played",
                                             value = 200),
                                numericInput(inputId = "count_val_add",
                                             label = "How Many Poss or Min. to Add?",
                                             value = 10),
                                actionButton(inputId = "run_test",
                                             label = "Should I Use This Rating?")
                            ),
                            mainPanel(
                                verticalLayout(DT::dataTableOutput("test_table"),
                                               plotOutput('test_chart') %>% withSpinner(color = "#44a8f3")
                                               )
                                )
                            )
                        )
               )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {

    plotData <- eventReactive(input$run_test, {
        
        if(input$count_type == "Minutes") {
            
            poss_in <- input$count_val * 2
            
            num_poss_added <- input$count_val_add * 2
            
        } else {
            
            poss_in <- input$count_val
            
            num_poss_added <- input$count_val_add 
 
        }
        
        #### ORTG ####
        ortg_in <- input$ortg
        
        ppp_in <- ortg_in/100 
        
        points_in <- poss_in * ppp_in
        
        ortg_miss_2 <- 100 * (points_in)/(poss_in + num_poss_added)
        
        ortg_make_2 <- 100 * (points_in + 2 * num_poss_added)/(poss_in + num_poss_added)
        
        ortg_miss_3 <- 100 * (points_in)/(poss_in + num_poss_added)
        
        ortg_make_3 <- 100 * (points_in + 3 * num_poss_added)/(poss_in + num_poss_added)
        
        #### DRTG ####
        drtg_in <- input$drtg
        
        ppp_in <- drtg_in/100 
        
        points_in <- poss_in * ppp_in
        
        drtg_make_2 <- 100 * (points_in + 2 * num_poss_added)/(poss_in + num_poss_added)
        
        drtg_miss_2 <- 100 * (points_in)/(poss_in + num_poss_added)
        
        drtg_make_3 <- 100 * (points_in + 3 * num_poss_added)/(poss_in + num_poss_added)
        
        drtg_miss_3 <- 100 * (points_in)/(poss_in + num_poss_added)
        
        #### NetRTG ####
        netrtg_in <- ortg_in - drtg_in
        
        netrtg_lwr_2 <- round(ortg_miss_2 - drtg_make_2, 2)
        
        netrtg_upr_2 <- round(ortg_make_2 - drtg_miss_2, 2) 
        
        netrtg_lwr_3 <- round(ortg_miss_3 - drtg_make_3, 2)
        
        netrtg_upr_3 <- round(ortg_make_3 - drtg_miss_3, 2) 
        
        res <- data.frame(`Result Type` = c("Negative","Initial","Positive"),
                          `ORTG (FG2)` = c(ortg_miss_2, ortg_in, ortg_make_2),
                          `DRTG (FG2)` = c(drtg_make_2, drtg_in, drtg_miss_2),
                          `NetRTG (FG2)` = c(netrtg_lwr_2, netrtg_in, netrtg_upr_2),
                          `ORTG (FG3)` = c(ortg_miss_3, ortg_in, ortg_make_3),
                          `DRTG (FG3)` = c(drtg_make_3, drtg_in, drtg_miss_3),
                          `NetRTG (FG3)` = c(netrtg_lwr_3, netrtg_in, netrtg_upr_3),
                          check.names = FALSE) 
        
        
        if(input$count_type == "Minutes") {
            
            res <- res %>% 
                mutate(Minutes = c(input$count_val + input$count_val_add, 
                                   input$count_val,
                                   input$count_val + input$count_val_add)) %>% 
                mutate_if(is.numeric, function(x) round(x, 1))
            
        } else {
            
            res <- res %>% 
                mutate(Possessions = c(input$count_val + input$count_val_add, 
                                   input$count_val,
                                   input$count_val + input$count_val_add)) %>% 
                mutate_if(is.numeric, function(x) round(x, 1))
            
        }
        
        return(res)
    
    })
    

output$test_table <- DT::renderDataTable({
        
        if("Minutes" %in% names(plotData())) {
            
            caption <- paste0("Rating Changes Based on ", plotData()[1, 8] - plotData()[2, 8], " Additional Minutes")
            
        } else {
            
            caption <- paste0("Rating Changes Based on ", plotData()[1, 8] - plotData()[2, 8], " Additional Possesions")
            
        }
        
        datatable(plotData(), 
                  filter = "top", 
                  rownames = FALSE,
                  options = list(dom = 'ft',
                                 columnDefs = list(list(className = 'dt-center', targets = "_all"))), 
                  caption = caption)
        
        
    })
    
    
output$test_chart <- renderPlot({
    
    if("Minutes" %in% names(plotData())) {
        
            check_val <- (plotData()[2, 8]) * 2
        
        } else {
            
            check_val <- plotData()[2, 8]
            
        }
        
        if(between(check_val, 0, 200) == TRUE) {
            
            pick_from <- c("Absolutely Not.",
                           "Under No Circumstances.",
                           "Only If Used Against Celtics.",
                           "Please Tag Ryan Davis When Used.",
                           "This Should be Illegal.",
                           "YOLO")
            
            color <- "#EF2D56"
            
        } else if(between(check_val, 201, 500) == TRUE){
            
            pick_from <- c("Shrug Emoji",
                           "You Sure You Need This?",
                           "Caveat Heavily.",
                           "Only To Make Sixers Look Good.")
            
            color <- "#ED7D3A"
            
            
        } else if(between(check_val, 501, 750) == TRUE){
            
            pick_from <- c("Probably Only Need One `However...`",
                           "Getting There Actually.",
                           "I Won't Be Angry.",
                           "Meaningfulish")
            
            color <- "#DCED31"
            
        } else {
            
            pick_from <- c("Actually, Yes.",
                           "Go For It, Champ.",
                           "Sick Job Stat Hunting.",
                           "Make Sure to Thank BBall Reference.")
            
            color <- "#0CCE6B"
            
        }
        
        textData <- data.frame(x = 0, 
                               y = 0, 
                               lab = sample(pick_from, 1), 
                               val = check_val)
        
        ggplot(data = textData, aes(x = 0, y = 0)) +
            geom_label(aes(label = lab), 
                       size = 20,
                       fill = color,
                       label.padding = unit(1, "lines")) +
            labs("Should I Use This Rating?") +
            theme_void(base_size = 36)
        
    })
    
        
}

# Run the application 
shinyApp(ui = ui, server = server)
