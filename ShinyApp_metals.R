##Shiny App assignment 

library(shiny)
library(tidyverse)

df <- read.csv("your_dataset.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("Metal concentrations in plants vs soil"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("soil_concentration", "Soil Concentration", min = 0, max = 10, value = 5, step = 0.1)
    ),
    mainPanel(
      plotOutput("metal_plot")
    )
  )
)



server <- function(input, output) {
  
  output$metal_plot <- renderPlot({
    
    # Filter the dataset based on the selected soil concentration
    df_filtered <- df %>% 
      filter(get(paste0("Soil_", input$soil_concentration)) == 1)
    
    # Gather the data into a tidy format
    df_tidy <- df_filtered %>% 
      select(Scientific_Name, contains("Plant")) %>% 
      gather(key = "metal", value = "concentration", -Scientific_Name)
    
    # Create the bar plot
    ggplot(df_tidy, aes(x = Scientific_Name, y = concentration, fill = metal)) + 
      geom_bar(stat = "identity", position = "dodge") +
      xlab("Plant Species") +
      ylab("Concentration") +
      ggtitle(paste0("Metal Concentrations in Plants with Soil Concentration: ", input$soil_concentration))
  })
  
}



shinyApp(ui, server)




################################ VER 2
#Dalem ze mam 


library(shiny)
library(tidyverse)

df <- read.csv("your_dataset.csv", stringsAsFactors = FALSE)



ui <- fluidPage(
  titlePanel("Metal concentrations in plants vs soil"),
  sidebarLayout(
    sidebarPanel(
      selectInput("soil_metal", "Soil Metal", choices = c("Cu", "Zn", "Fe"), selected = "Cu"),
      sliderInput("soil_concentration", "Soil Concentration", min = 0, max = 10, value = 5, step = 0.1)
    ),
    mainPanel(
      plotOutput("metal_plot")
    )
  )
)



server <- function(input, output) {
  
  output$metal_plot <- renderPlot({
    
    # Filter the dataset based on the selected soil concentration and soil metal
    df_filtered <- df %>% 
      filter(get(paste0(input$soil_metal, "_soil")) == input$soil_concentration)
    
    # Gather the data into a tidy format
    df_tidy <- df_filtered %>% 
      select(Scientific_Name, contains("Plant")) %>% 
      gather(key = "metal", value = "concentration", -Scientific_Name)
    
    # Create the bar plot
    ggplot(df_tidy, aes(x = Scientific_Name, y = concentration, fill = metal)) + 
      geom_bar(stat = "identity", position = "dodge") +
      xlab("Plant Species") +
      ylab("Concentration") +
      ggtitle(paste0("Metal Concentrations in Plants with ", input$soil_metal, " Soil Concentration: ", input$soil_concentration))
  })
  
}

shinyApp(ui, server)