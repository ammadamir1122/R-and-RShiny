library(shiny)
library(tidyverse)
library(plotly)

# Load the data
data = read.csv("Dataset_spine.csv")

# Define the UI
ui <- fluidPage(
  
  # Sidebar with input controls
  sidebarLayout(
    sidebarPanel(
      selectInput("statistic", "Select a statistic:",
                  choices = c("Mean", "Median", "Mode")),
      selectInput("feature", "Select a feature:",
                  choices = names(data)[1:12])
    ),
    
    # Show a plot of the selected statistic and feature
    mainPanel(
      plotlyOutput("barplot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # Calculate summary statistics
  summary_stats <- reactive({
    if (input$statistic == "Mean") {
      data %>%
        summarize(stat = mean(!!sym(input$feature)))
    } else if (input$statistic == "Median") {
      data %>%
        summarize(stat = median(!!sym(input$feature)))
    } else {
      data %>%
        summarize(stat = as.numeric(names(sort(-table(!!sym(input$feature)))))[1])
    }
  })
  
  # Create a bar chart
  output$barplot <- renderPlotly({
    ggplotly(
      ggplot(summary_stats(), aes(x = "Statistic", y = stat, fill = "Statistic")) +
        geom_bar(stat = "identity", width = 0.25) +
        theme_minimal() +
        labs(title = paste0(input$statistic, " of ", input$feature))
    )
  })
}

# Run the app
shinyApp(ui, server)
