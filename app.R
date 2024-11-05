##################
# Shiny app by Elise Arps
# November 5, 2024
# 
# Compares proportion of accidents by various categories sorted by state.
#
# Deployed at #https://elisea.shinyapps.io/assignment9/
# Source code at GitHub: 
##################

library(readr)
library(tidyverse)
library(rsconnect)

accident <- read_csv("accident.csv")
accident <- accident %>%
  select(STATE, STATENAME, ST_CASE, MONTH, MONTHNAME, DAY, DAY_WEEK, HOUR, RUR_URB, RUR_URBNAME, WRK_ZONE, WRK_ZONENAME, LGT_COND, LGT_CONDNAME, WEATHER, WEATHERNAME, SCH_BUS, SCH_BUSNAME, RAIL, RAILNAME, FATALS)
accident <- accident %>%
  mutate(date = make_date(year = 2024, month = MONTH, day = DAY))


library(shiny)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(shinythemes)
library(bslib)

ui <- fluidPage(
  theme = bs_theme(
    bootswatch = "lux",      # Use a modern theme like "lux" or "cyborg" (for dark mode)
    primary = "#3498db",      # Set primary color
    secondary = "#2ecc71",    # Set secondary color
    base_font = font_google("Roboto"),    # Set custom Google font
    heading_font = font_google("Montserrat"),
    bg = "#ff6347",           # Light background color
    fg = "#f5f5f5"            # Dark text color
  ),
  titlePanel("Proportional Bar Plot of Accidents by Month"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("state", 
                  "Select State:", 
                  choices = unique(accident$STATENAME), 
                  selected = unique(accident$STATENAME)[1]),
      
      selectInput("variable", 
                  "Select Proportion Variable:", 
                  choices = c("Weather" = "WEATHERNAME", 
                              "Light Condition" = "LGT_CONDNAME", 
                              "Rural/Urban" = "RUR_URBNAME", 
                              "Work Zone" = "WRK_ZONENAME", 
                              "School Bus" = "SCH_BUSNAME"), 
                  selected = "WEATHERNAME")
    ),
    
    mainPanel(
      plotOutput("barPlot")
    )
  )
)

server <- function(input, output) {
  
  output$barPlot <- renderPlot({
    data <- accident %>%
      filter(STATENAME == input$state) %>%
      group_by(MONTH, !!sym(input$variable)) %>%
      summarise(count = n(), .groups = 'drop') %>%
      group_by(MONTH) %>%
      mutate(proportion = count / sum(count)) 
    
    ggplot(data, aes(x = factor(MONTH, levels = 1:12, labels = month.abb), y = proportion, fill = !!sym(input$variable))) +
      geom_bar(stat = "identity", position = "fill") +  
      labs(x = "Month", y = "Proportion", title = paste("Proportional Bar Plot of Accidents by Month")) +
      theme_minimal() +
      scale_y_continuous(labels = scales::percent)
  })
}

shinyApp(ui = ui, server = server)

#https://elisea.shinyapps.io/assignment9/