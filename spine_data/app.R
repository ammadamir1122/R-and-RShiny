library(shiny)
library(tidyverse)
library(plotly)

# Load the Spine Dataset

data = read.csv("Dataset_spine.csv")

# Define UI
ui <- fluidPage(
  
  # Sidebar layout
  sidebarLayout(
    
    # Sidebar panel
    sidebarPanel(
      
      # Select feature
      selectInput(inputId = "feature",
                  label = "Select feature:",
                  choices = names(data),
                  selected = "Pelvic Incidence"),
      
      # Select statistic
      radioButtons(inputId = "statistic",
                   label = "Select statistic:",
                   choices = c("Mean"),
                   selected = "Mean")
      
    ),
    
    # Main panel
    mainPanel(
      
      # Box plot
      plotlyOutput(outputId = "boxplot"),
      
      # Table
      tableOutput(outputId = "summary")
      
    )
    
  )
  
)

# Define server
server <- function(input, output) {
  
  # Store selected statistic value
  selected_statistic <- reactive({
    input$statistic
  })
  
  # Calculate summary statistics for selected feature
  selected_feature <- reactive({
    selected_statistic() == "Mean"
    data %>%
      summarise(mean = mean(!!sym(input$feature)))
  })
  
  # Create box plot
  output$boxplot <- renderPlotly({
    ggplotly(
      ggplot(data, aes(x = 1, y = !!sym(input$feature))) +
        geom_boxplot() +
        geom_point(data = selected_feature(), aes(y = mean), color = "red", size = 3) +
        coord_flip() +
        theme_bw() +
        labs(y = input$feature, x = NULL, title = paste(selected_statistic(), "of", input$feature))
    )
  })
  
  # Create summary table
  output$summary <- renderTable({
    selected_feature()
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
