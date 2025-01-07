library(shiny)
library(rgbif)
library(rinat)
library(ggplot2)
library(dplyr)
library(lubridate)
library(maps)
library(plotly)

# Function to assign seasons - chronological ordering
get_season <- function(dates) {
  months <- month(dates)
  seasons <- case_when(
    months %in% c(12, 1, 2) ~ "Winter",
    months %in% c(3, 4, 5) ~ "Spring",
    months %in% c(6, 7, 8) ~ "Summer",
    months %in% c(9, 10, 11) ~ "Fall"
  )
  factor(seasons, 
         levels = c("Spring", "Summer", "Fall", "Winter"),
         ordered = TRUE)
}

# UI definition
ui <- fluidPage(
  titlePanel("iNaturalist Species Observations in California"),
  
  sidebarLayout(
    sidebarPanel(
      # Input for species query
      textInput("species_query", 
                "Species Name:", 
                value = "Danaus plexippus"),
      
      # Dynamic species selection - only shows up when there are multiple species
      uiOutput("species_selector"),
      
      # Quality checkbox
      HTML("<label>Data Quality Filter:</label>"),
      checkboxInput("research_quality",
                    "Research Grade Only",
                    value = TRUE),
      
      # Year range slider
      sliderInput("year_range",
                  "Select Year Range:",
                  min = 2010,
                  max = 2024,
                  value = c(2010, 2024)),
      
      # Time grouping radio buttons
      radioButtons("time_group",
                   "Group observations by:",
                   choices = c("Month", "Season", "Year"),
                   selected = "Month")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Map", plotOutput("map")),
        tabPanel("Time Series", plotlyOutput("time_series"))
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Reactive data fetch - only triggered when species query changes
  raw_data <- reactive({
    req(input$species_query)
    
    # California bounding box
    california_bb <- c(32.5, -124.5, 42, -114)
    
    withProgress(message = 'Fetching new species data...', {
      get_inat_obs(
        query = input$species_query,
        bounds = california_bb
      )
    })
  }) %>% bindEvent(input$species_query)
  
  # Generate species checklist UI
  output$species_selector <- renderUI({
    req(raw_data())
    species_list <- unique(raw_data()$scientific_name)
    
    # Only show if there are multiple species
    if(length(species_list) > 1) {
      checkboxGroupInput("selected_species",
                         "Select species to include:",
                         choices = sort(species_list),
                         selected = species_list)
    }
  })
  
  # Filter data based on user inputs
  filtered_data <- reactive({
    data <- raw_data()
    
    # Convert dates
    data <- data %>%
      mutate(date = as.Date(datetime))
    
    # Apply species filter if selector is present
    if(!is.null(input$selected_species)) {
      data <- data %>% 
        filter(scientific_name %in% input$selected_species)
    }
    
    # Apply year filter
    filtered <- data %>%
      filter(
        year(date) >= input$year_range[1],
        year(date) <= input$year_range[2]
      )
    
    # Apply quality filter if selected
    if(input$research_quality) {
      filtered <- filtered %>% filter(quality_grade == "research")
    }
    
    filtered
  })
  
  # Map output
  output$map <- renderPlot({
    req(filtered_data())
    
    inat_map(filtered_data(), plot = FALSE) +
      borders("state", regions = "california") +
      coord_fixed(xlim = c(-124.5, -114), ylim = c(32.5, 42)) +
      theme_bw() +
      ggtitle(paste("Observations in California",
                    if(input$research_quality) "(Research Grade)" else "(All Observations)"))
  })
  
  # Time series output
  output$time_series <- renderPlotly({
    req(filtered_data())
    
    if(input$time_group == "Month") {
      counts_df <- filtered_data() %>%
        mutate(group_date = floor_date(date, unit = "month")) %>%
        count(group_date) %>%
        mutate(label = format(group_date, "%B %Y")) %>%
        arrange(group_date)
      
      plot_ly(counts_df, 
              x = ~group_date, 
              y = ~n, 
              type = "bar",
              marker = list(color = "orange"),
              text = ~paste("Date:", label,
                            "<br>Count:", n),
              hoverinfo = "text") %>%
        layout(
          title = "Observations Over Time",
          xaxis = list(title = "Date", tickangle = 45),
          yaxis = list(title = "Number of Observations"),
          hoverlabel = list(bgcolor = "white")
        )
      
    } else if(input$time_group == "Season") {
      counts_df <- filtered_data() %>%
        mutate(
          year = year(date),
          season = get_season(date)
        ) %>%
        count(year, season) %>%
        mutate(label = paste(season, year)) %>%
        arrange(year, season)
      
      plot_ly(counts_df,
              x = ~interaction(year, season),
              y = ~n,
              type = "bar",
              marker = list(color = "orange"),
              text = ~paste("Period:", label,
                            "<br>Count:", n),
              hoverinfo = "text") %>%
        layout(
          title = "Observations by Season",
          xaxis = list(title = "Season", tickangle = 45),
          yaxis = list(title = "Number of Observations"),
          hoverlabel = list(bgcolor = "white")
        )
      
    } else { # Year
      counts_df <- filtered_data() %>%
        mutate(year = year(date)) %>%
        count(year) %>%
        arrange(year)
      
      plot_ly(counts_df,
              x = ~as.factor(year),
              y = ~n,
              type = "bar",
              marker = list(color = "orange"),
              text = ~paste("Year:", year,
                            "<br>Count:", n),
              hoverinfo = "text") %>%
        layout(
          title = "Observations by Year",
          xaxis = list(title = "Year", tickangle = 45),
          yaxis = list(title = "Number of Observations"),
          hoverlabel = list(bgcolor = "white")
        )
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)