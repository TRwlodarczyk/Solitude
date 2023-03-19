####################################
# Data Professor                   #
# http://youtube.com/dataprofessor #
# http://github.com/dataprofessor  #
####################################

# Modified from https://shiny.rstudio.com/tutorial/written-tutorial/lesson1/

library(shiny)
library(plotly)
library(heatmaply)
library(ggcorrplot)


setwd("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/Data")
dt <-read.delim("my_data.txt")

utils::data("stackoverflow", "car_prices", "Sacramento", package = "modeldata")

data_list = list(
  "StackOverflow" = stackoverflow,
  "Car Prices"    = car_prices,
  "Sacramento Housing" = Sacramento
)

# Define UI for app that draws a histogram ----
ui <- navbarPage(
  
  title = "Data Explorer",
  
  theme = bslib::bs_theme(version = 4, bootswatch = "minty"),
  
  tabPanel(
    title = "Explore",
    
    sidebarLayout(
      
      sidebarPanel(
        width = 3,
        h1("Explore Solitude Plant Dataset"),
        
        # Requires Reactive Programming Knowledge
        # - Taught in Shiny Dashboards (DS4B 102-R)
        shiny::selectInput(
          inputId = "dataset_choice",
          label   = "Data Connection",
          choices = c("StackOverflow", "Car Prices", "Sacramento Housing")
        ),
        
        # Requires Boostrap Knowledge
        # - Taught in Shiny Developer with AWS (DS4B 202A-R)
        hr(),
        h3("Apps by Business Science"),
        p("Go from beginner to building full-stack shiny apps."),
        p("This was created with Shiny") %>%
          a(
            href = 'https://www.business-science.io/',
            target = "_blank",
            class = "btn btn-lg btn-primary"
          ) %>%
          div(class = "text-center")
        
        
      ),
      
      mainPanel(
        h1("Correlation Between Plant elements"),
        plotlyOutput("corrplot", height = 700)
      )
    )
    
  )
  
  
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$corrplot <- renderPlotly({
    
    g <- heatmaply_cor(
      cor(dt_cor),
      xlab = "Features", 
      ylab = "Features",
      k_col = 2, 
      k_row = 2
    )
    
    plotly::ggplotly(g)
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

