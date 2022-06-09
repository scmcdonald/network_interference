#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(here)
library(plotly)
library(reshape2)
library(RColorBrewer)

cb_colors <- brewer.pal(n = 8, name = "Dark2")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Matrix Coefficient Visualization"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("threshold",
                        "Threshold:",
                        min = 0.01,
                        max = 0.1,
                        value = 0.05),
            selectInput("model_lag", "Model Lag:",
                        c("5" = "5",
                          "10" = "10",
                          "20" = "20")), 
            selectInput("bigvar", "Big Var Method:",
                        c("Basic" = "Basic",
                          "HLAGC" = "HLAGC",
                          "HLAGELEM" = "HLAGELEM", 
                          "HLAGOO" = "HLAGOO")),
            selectInput("date", "Date:",
                        seq(as.Date("2021-10-07"), as.Date("2021-11-30"), 1)),
            sliderInput("lag", "Lag:", 
                        min = 1, max = 5, value = 1, step = 1),
            width = 2
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("distPlot"), width = 10
        )
    )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
    
    filtered_data <- reactive({
        coefs <- readRDS(paste("coefs/coef_", input$date, "_L", input$model_lag, "_", input$bigvar,".rds", sep = ""))
        coefs_no_intercept <- coefs[, -1]
        coefs_threshold <- coefs_no_intercept
        return(coefs_threshold)
        })
    observe({
        updateSliderInput(session = getDefaultReactiveDomain(), "lag", max = as.numeric(input$model_lag))
    })

    output$distPlot <- renderPlotly({
        
        coefs_threshold_subset <- filtered_data()
        
        coefs_threshold_subset[abs(coefs_threshold_subset) < input$threshold] <- NA
        
        p <- coefs_threshold_subset[, grep(pattern = paste("L", input$lag, "$", sep = ""), colnames(coefs_threshold_subset))] %>%
            as.matrix() %>%
            melt() %>%
            rename(state = Var1, lag = Var2) %>%
            mutate(lag = str_remove(lag, paste("L", input$lag, sep = ""))) %>%
            ggplot(aes(x = lag, y = state)) +
            geom_tile(aes(fill = value), color = "white")  +
            scale_fill_gradientn(colors = c(cb_colors[2], "white", cb_colors[1]), limits = c(-1, 1))+
            theme_minimal()+
            labs(y = "State", x = paste("L", input$lag, sep = ""), 
                 title = paste( "Date: ", input$date,  " Threshold: ", input$threshold, " VAR(", input$model_lag, ") BigVAR Method: ", input$bigvar, " Lag: ", input$lag, sep = ""))
       
     ggplotly(p, height = 1000, width = 1300) 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
