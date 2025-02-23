library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(purrr)

# Load Data
nba_data <- read.csv("https://raw.githubusercontent.com/Shahv03/nba-shiny-app/main/nba_2022-23_all_stats_with_salary.csv")

options(scipen = 999)

stat_choices <- c("Points per Game (PTS)" = "PTS",
                  "Usage Percentage (USG%)" = "USG.",
                  "Player Efficiency Rating (PER)" = "PER",
                  "Minutes Per Game (MP)" = "MP",
                  "Games Started (GS)" = "GS",
                  "True Shooting Percentage (TS%)" = "TS.",
                  "Field Goal Percentage (FG%)" = "FG.",
                  "Three-Point Percentage (3P%)" = "X3P.",
                  "Turnover Percentage (TOV%)" = "TOV.",
                  "Offensive Win Shares (OWS)" = "OWS",
                  "Defensive Win Shares (DWS)" = "DWS",
                  "Win Shares (WS)" = "WS",
                  "Win Shares per 48 Minutes (WS/48)" = "WS.48",
                  "Offensive Box Plus/Minus (OBPM)" = "OBPM",
                  "Defensive Box Plus/Minus (DBPM)" = "DBPM",
                  "Box Plus/Minus (BPM)" = "BPM",
                  "Value Over Replacement Player (VORP)" = "VORP")

# Process positions to handle multi-position players
nba_data <- nba_data %>% 
  mutate(PositionList = strsplit(Position, "-"),
         Team = sapply(strsplit(Team, "/"), tail, 1))

# Extract unique positions
positions <- unique(unlist(nba_data$PositionList))

# UI
ui <- fluidPage(
  titlePanel("NBA Performance vs Salary (2022-2023 Season)"),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput("instructions"), 
      selectInput("stat", "Choose Performance Stat:", choices = names(stat_choices)),
      sliderInput("games", "Filter by Games Played:",
                  min = min(nba_data$GP, na.rm = TRUE),
                  max = max(nba_data$GP, na.rm = TRUE),
                  value = range(nba_data$GP, na.rm = TRUE), step = 1),
      checkboxGroupInput("positions", "Select Positions:", choices = positions, selected = positions)
    ),
    
    mainPanel(
      plotlyOutput("salaryPlot"),
      uiOutput("footer")  
    )
  )
)

# Server
server <- function(input, output) {
  
  output$instructions <- renderUI({
    HTML("<p>Select a performance stat to compare against player salaries.</p>")
  })

  output$footer <- renderUI({
    HTML("<p style='text-align:center; font-size:14px;'>Created by Vaibhav Shah</p>")
  })
  
  # Filtered data
  filtered_data <- reactive({
    nba_data %>%
      filter(GP >= input$games[1] & GP <= input$games[2]) %>%
      filter(map_lgl(PositionList, ~ any(.x %in% input$positions)))  # Cleaner than sapply()
  })
  
  # graph
  output$salaryPlot <- renderPlotly({
    actual_stat <- stat_choices[[input$stat]]
    
    p <- ggplot(filtered_data(), aes(x = .data[[actual_stat]], y = Salary, color = Position, text = Player.Name)) +
      geom_point(alpha = 0.6) +
      scale_y_continuous(labels = scales::comma) +
      labs(title = paste("NBA 2022-23:", input$stat, "vs Salary"),
           x = input$stat, y = "Salary ($)") +
      theme_minimal() +
      theme(axis.text.y = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text")
  })
}

# Run App
shinyApp(ui, server)
