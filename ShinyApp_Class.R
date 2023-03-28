# Shiny App New
# Tomasz Wlodarczyk
# twlodarczyk@arizona.edu
# 2023-03-27

# Load required packages
library(shiny)
library(ggplot2)

dt_plants <- read.csv("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data/Tomasz.csv")

# Define UI
ui <- fluidPage(
  
  # Define sidebar with three options
  sidebarLayout(
    sidebarPanel(
      selectInput("plot_type", "Choose a plot type:",
                  c("Correlation Plot" = "corr_plot",
                    "Boxplot" = "boxplot",
                    "Barplot" = "barplot")),
      sliderInput("concentration", "Concentration:",
                  min = min(dt_plants$Cu_concentration),
                  max = max(dt_plants$Cu_concentration),
                  value = mean(dt_plants$Cu_concentration),
                  step = 0.1)
    ),
    
    # Define main panel for displaying plots
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Define function to create correlation plot
  create_corr_plot <- function() {
    ggplot(dt_plants, aes(x = Cu_concentration, y = Scientific_Name)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Correlation Plot",
           x = "Concentration",
           y = "Plants")
  }
  
  # Define function to create boxplot
  create_boxplot <- function() {
    ggplot(dt_plants, aes(y = Cu_concentration)) +
      geom_boxplot() +
      labs(title = "Boxplot",
           y = "Concentration")
  }
  
  # Define function to create barplot
  create_barplot <- function() {
    dt_plants_subset <- dt_plants[dt_plants$Cu_concentration >= input$Cu_concentration, ]
    ggplot(dt_plants_subset, aes(x = Scientific_Name)) +
      geom_bar() +
      labs(title = "Barplot",
           x = "Plants",
           y = "Count")
  }
  
  # Create plot based on input plot_type and concentration
  output$plot <- renderPlot({
    if (input$plot_type == "corr_plot") {
      create_corr_plot()
    } else if (input$plot_type == "boxplot") {
      create_boxplot()
    } else {
      create_barplot()
    }
  })
}

# Run the app
shinyApp(ui, server)