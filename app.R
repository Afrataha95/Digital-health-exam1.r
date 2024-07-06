# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

# Load your life expectancy data from your CSV file
life_expectancy_data <- read.csv("Life_Expectancy_Data.csv", header = TRUE)

# Define UI for the app
ui <- fluidPage(
  titlePanel("Life Expectancy Over Time by Country"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_country", "Select Country:", 
                  choices = unique(life_expectancy_data$Country), 
                  selected = unique(life_expectancy_data$Country)[1])
    ),
    mainPanel(
      plotOutput("country_plot")
    )
  )
)

# Define server logic for the app
server <- function(input, output) {
  
  output$country_plot <- renderPlot({
    filtered_data <- filter(life_expectancy_data, Country == input$selected_country)
    ggplot(data = filtered_data, aes(x = Year, y = LifeExpectancy)) +
      geom_line() +
      labs(title = paste("Life Expectancy Over Time for", input$selected_country),
           x = "Year",
           y = "Life Expectancy") +
      theme_minimal()
  })
}

# Create the Shiny app
shinyApp(ui = ui, server = server)
