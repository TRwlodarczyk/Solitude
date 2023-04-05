# Shiny App New
# Tomasz Wlodarczyk
# twlodarczyk@arizona.edu
# 2023-03-27

# Load required packages
library(shiny)
library(ggplot2)
library(bslib)
library(DT)
library(plotly)
library(heatmaply)
library(ggcorrplot)
library(corrplot)


dt_plants <- read.csv("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/GitHub/Solitude/Tomasz.csv")

# Define UI
ui <- fluidPage(titlePanel("Solitude Plants & Soil"),
                
                
 tags$img(src="CESMlogo.png", align = "right",height='100px',width='400px', deleteFile=FALSE),                
                
  theme = bs_theme(
    bg = "#075773", 
    fg = "#FDF7F7", 
    primary = "#CFBBA4", 
    base_font = font_google("Prompt"),
    code_font = font_google("JetBrains Mono")
  ),
  

  
  # Define main panel for displaying plots and tables
  mainPanel(
    navlistPanel(
      "Plots & Tables",
      id = "tabs",
      selected = "Plots",
      tabPanel(
        "Plots",
        conditionalPanel(
          condition = "input.tabs == 'Plots'",
          sidebarLayout(
            sidebarPanel(
              selectInput("plot_type", "Choose a plot type:",
                          c("Correlation Plot" = "corr_plot",
                            "Boxplot" = "boxplot",
                            "Barplot" = "barplot")),
              conditionalPanel(
                condition = "input.plot_type == 'barplot'",
                sliderInput("concentration", "Concentration:",
                            min = min(dt_plants$Cu_concentration),
                            max = max(dt_plants$Cu_concentration),
                            value = mean(dt_plants$Cu_concentration),
                            step = 0.1)
              ),
              conditionalPanel(
                condition = "input.plot_type == 'boxplot'",
                radioButtons("concentration_var", "Select a variable:",
                             choices = c("Copper concentration" = "Cu_concentration",
                                         "Selenium concentration" = "Se_concentration",
                                         "Rhenium concentration" = "Re_concentration"))
              )
            ),
            mainPanel(
              plotOutput("plot")
            )
          )
        )
      ),
      tabPanel("Tables", 
               radioButtons("table_select", "Select a table:",
                            choices = c("Plants Table" = "plants_table",
                                        "Soil Table" = "soil_table",
                                        "Summary Table" = "summary_table"),
                            selected = "plants_table"),
               dataTableOutput("table_data")
      )
    )
  )
)

# Define server
server <- function(input, output) {
  
  
  # Render the table data
  output$table_data <- renderDataTable({
    if (input$table_select == "plants_table") {
      datatable(dt_plants)
    } else if (input$table_select == "soil_table") {
      # You can replace "path_to_soil_table.csv" with the actual directory for your soil table
      datatable(read.csv("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/GitHub/Solitude/Solitude_CESM_Soil_3.6.23.csv"))
    } else if (input$table_select == "summary_table") {
      datatable(read.csv("C:/Users/twlodarczyk/OneDrive - University of Arizona/Desktop/All documents/1 PhD/CNRS + Synch/Field/Soltitude/GitHub/Solitude/Summary.csv"))
    }
  })
    
    
#Define function to create correlation plot - THIS DOES NOT WORK
#  create_corr_plot <- function() {
#    heatmaply_cor(
#      cor(dt_plants[,5:17]),
#      xlab = "Features", 
#      ylab = "Features",
#      k_col = 2, 
#      k_row = 2
#    )
#  }
 
  # Define function to create correlation plot - THIS ONE WORKS
  create_corr_plot <- function() {
    ggcorrplot(cor(dt_plants[,5:17]), 
               title = "Concentrations of elements in plants", 
               hc.order = TRUE) + 
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.margin = unit(c(1,1,1,1), "cm")
      )
  }
  


# Why is the heatmaply displayed in Viewer but not in ShinyApp
# Why is the logo not displayed
  

  # Define function to create boxplot
#  create_boxplot <- function() {
#    ggplot(dt_plants, aes(y = Cu_concentration)) +
#      geom_boxplot() +
#      labs(title = "Boxplot",
#           y = "Concentration")
#  }
 
  
  # Define function to create boxplot
  create_boxplot <- function() {
    if (input$concentration_var == "Cu_concentration") {
      dt_subset <- dt_plants
      ylab <- "Copper concentration"
    } else if (input$concentration_var == "Se_concentration") {
      dt_subset <- dt_plants
      ylab <- "Selenium concentration"
    } else if (input$concentration_var == "Re_concentration") {
      dt_subset <- dt_plants
      ylab <- "Rhenium concentration"
    }
    
#    ggplot(dt_subset, aes(y = get(input$concentration_var))) +
#      geom_boxplot() +
#      labs(title = "Boxplot",
#           y = ylab)



ggplot(dt_subset, aes(x = reorder(Scientific_Name, !!sym(input$concentration_var), FUN = median),
                      y = !!sym(input$concentration_var), group=Scientific_Name)) +
  geom_boxplot()+
  geom_jitter(aes(colour = Plot), size=1.7) +
  coord_flip()+
  scale_color_manual(values = c("#38A6A5", "#73AF48", "#EDAD08", "#CC503E")) +
  scale_x_discrete(guide = guide_axis(angle = 0))+
  #scale_y_continuous(limits = c(0, 600), breaks = seq(0, 800, by = 50)) +
  theme_classic()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=11),
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size=12, face="italic"),
        axis.title.y = element_blank(),
        legend.key.size = unit(1, "lines"),
        legend.text = element_text(size = 11),
        plot.background = element_rect(fill = "grey"),
        panel.background = element_rect(fill = "grey", colour="black")) +
  guides(colour = guide_legend(override.aes = list(size = 3.5)))+
  ylab(paste0(ylab, " (ppm)"))
  }
  
  ######################################
   
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
  }, height = 600, width = 800)
  

}

# Run the app
shinyApp(ui, server)

