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


coefs <- readRDS("coef_10_15_2021_L10_HLAGC.rds")
coefs_no_intercept <- coefs[, -1]
coefs_threshold <- coefs_no_intercept

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Matrix Visualization, 10-15-2021, Lag 10, HLAGC"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("threshold",
                        "Threshold:",
                        min = 0.01,
                        max = 0.1,
                        value = 0.05),
            sliderInput("lag",
                        "Lag:",
                        min = 1,
                        max = 10,
                        value = 1), width = 2
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("distPlot"), width = 10
        )
    )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlotly({
        
        coefs_threshold_subset <- coefs_threshold
        
        coefs_threshold_subset[abs(coefs_threshold_subset) < input$threshold] <- NA
        
        
        
        p <- coefs_threshold_subset[, grep(pattern = paste("L", input$lag, "$", sep = ""), colnames(coefs_threshold_subset))] %>%
            as.matrix() %>%
            melt() %>%
            rename(state = Var1, lag = Var2) %>%
            mutate(lag = str_remove(lag, paste("L", input$lag, sep = ""))) %>%
            ggplot(aes(x = lag, y = state)) +
            geom_tile(aes(fill = value), color = "white")  +
            scale_fill_gradientn(colours = c(cb_colors[2], "white", cb_colors[1]), limits = c(-0.4, 0.4))+
               theme_minimal()+
             labs(y = "State", x = paste("L", input$lag, sep = ""), caption = paste("Threshold: ", input$threshold, sep = ""))
       
     ggplotly(p, height = 1000, width = 1300) 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
