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
source("paint.R")

avail_files <- sapply(list.files("data"), function(x){return(paste0("data/", x))})
chunks_pred <- read.table(avail_files[1])

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Back-of-the-Envelope Chromosome Painting Tool"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("slider2", label = h3("Slider Range"), min = 0, max = max(chunks_pred$V2), 
                        value = c(0, max(chunks_pred$V2))),
            selectInput("chromosome_top", "Which chromosome?", avail_files),
            selectInput("chromosome_bottom", "Which chromosome?", avail_files)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        file_top <- input$chromosome_top
        file_bottom <- input$chromosome_bottom
        top_plot <- read.table(file_top)
        bottom_plot <- read.table(file_bottom)
        a <- input$slider2[1]
        b <- input$slider2[2]
        plot.chunks(top_plot, bottom_plot, a, b)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
