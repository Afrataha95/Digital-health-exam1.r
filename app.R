# Set CRAN mirror
options(repos = c(CRAN = "https://cran.rstudio.com"))

# Load necessary libraries
library(shiny)
library(ggplot2)
library(plotly)
library(shinythemes)
library(thematic)
library(shinyjs)
library(shinydashboard)
library(dplyr)
library(DT)
library(tidyr)

# Load the data
life_expectancy_data <- read.csv("life_expectancy.csv", stringsAsFactors = FALSE)

# Define UI
ui <- fluidPage(
  useShinyjs(),  # Enable shinyjs
  theme = shinytheme("superhero"),  # Set the theme to 'superhero'
  
  titlePanel("Life Expectancy Data Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select Country:", choices = unique(life_expectancy_data$Country), selected = "United States"),
      sliderInput("yearRange", "Select Year Range:", min = min(life_expectancy_data$Year), max = max(life_expectancy_data$Year), value = c(min(life_expectancy_data$Year), max(life_expectancy_data$Year))),
      checkboxGroupInput("variables", "Select Variables to Plot:", choices = names(life_expectancy_data)[4:length(names(life_expectancy_data))], selected = c("LifeExpectancy", "GDP")),
      
      # Additional Information
      HTML("<br /><h5 style='font-size: 14px;'>Understanding the Data</h5>
            <p style='font-size: 12px;'>This application allows you to visualize life expectancy data across various countries and years. Select a country and a range of years to see the trends. You can also choose which variables to plot. The data includes information on life expectancy, GDP, and other health-related metrics.</p>")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotlyOutput("lifeExpPlot")),
        tabPanel("Table", dataTableOutput("dataTable"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  filteredData <- reactive({
    life_expectancy_data %>%
      filter(Country == input$country & Year >= input$yearRange[1] & Year <= input$yearRange[2])
  })
  
  output$lifeExpPlot <- renderPlotly({
    plotData <- filteredData()
    plotData <- plotData[, c("Year", input$variables)]
    plotData <- gather(plotData, key = "Variable", value = "Value", -Year)
    
    p <- ggplot(plotData, aes(x = Year, y = Value, color = Variable, text = paste("Year:", Year, "<br>Value:", Value))) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      labs(title = paste("Trends in", input$country), x = "Year", y = "Value") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position = "bottom"
      )
    
    ggplotly(p, tooltip = "text") %>% 
      layout(legend = list(orientation = "h", x = 0.1, y = -0.3))
  })
  
  output$dataTable <- renderDataTable({
    datatable(
      filteredData(),
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All')),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#485563', 'color': '#fff'});",
          "}"
        )
      ),
      class = "stripe hover cell-border order-column"
    ) %>% formatStyle(
      columns = names(filteredData()),
      backgroundColor = styleInterval(5, c('white', 'lightblue')),
      color = 'black'
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
