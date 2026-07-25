#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

if (!require(shiny)) install.packages("shiny")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")
if (!require(plotly)) install.packages("plotly")
if (!require(leaflet)) install.packages("leaflet")
if (!require(sf)) install.packages("sf")



library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(leaflet)
library(sf)


trend_data <- read.csv("data/trend_data.csv")
map_data <- read.csv("data/map_data.csv")
ranking_data <- read.csv("data/ranking_data.csv")
police_point_data <- st_read("data/london_police_points.geojson")
police_polygon_data <- st_read("data/london_police_polygons.geojson")




get_street_ranking <- function(crime_type,
                               month = "all",
                               top_n = 10) {
  df <- ranking_data
  
  df <- df %>% filter(category == crime_type)
  
  df %>%
    count(street_clean, sort = TRUE) %>%
    slice_head(n = top_n)
}




ui <- fluidPage(
  
  # App title
  fluidRow(
    column(
      width = 12,
      h1("London Crime Dashboard (2024)",
         style = "margin-top: 20px; font-weight: 700;")
    )
  ),
  
  
  fluidRow(
    column(
      width = 4,
      img(
        src = "image.jpg", 
        width = "100%",
        style = "border-radius: 10px; margin-top: 10px;"
      )
    ),
    column(
      width = 8,
      h3("Welcome to the London Crime Dashboard"),
      p("This dashboard provides a comprehensive overview of crime patterns across London in 2024."),
      p("Scroll down to explore three key analytical sections:"),
      tags$ol(
        tags$li("Crime Map : Explore spatial crime patterns and police station coverage."),
        tags$li("Top Streets : Identify high-crime areas and drill down into crime categories."),
        tags$li("Temporal Patterns : Understand how crime changes across months and categories.")
      )
    )
  ),
  
  hr(),
  
 
  h2("Section 1: Crime Map of London"),
  "This interactive map visualizes crime locations across London.",
  br(),
  "You can select a crime category and optionally overlay police station boundaries and points.",
  
  sidebarLayout(
    sidebarPanel(
      h4("Map Controls"),
      selectInput(
        inputId = "map_crime",
        label = "Crime Category:",
        choices = sort(unique(map_data$category)),
        selected = "violent-crime"
      ),
      checkboxInput("show_police", "Show Police Stations", value = FALSE)
    ),
    mainPanel(
      leafletOutput("crime_map", height = 600)
    )
  ),
  
  hr(),
  
 
  
  h2("Section 2: Top Streets/Areas & Crime Breakdown"),
  "This section highlights the streets/areas with the highest crime counts and allows you to explore crime category distributions.",
  br(),
  strong("Tip:"), " Hover over any bar or pie slice to see detailed information.",
  
  sidebarLayout(
    sidebarPanel(
      h4("Filter for Plot 1"),
      selectInput(
        inputId = "street_crime",
        label = "Crime Category:",
        choices = sort(unique(ranking_data$category)),
        selected = "violent-crime"
      ),
      helpText("This filter only affects the first plot.")
    ),
    mainPanel(
      h4("Plot 1: Top 10 Areas for Selected Crime Category"),
      plotlyOutput("street_plot", height = "350px"),
      hr(),
      
      h4("Plot 2: Top 10 Areas by Total Crime Count"),
      plotlyOutput("top10_total_plot", height = "300px"),
      hr(),
      
      h4("Plot 3: Crime Category Distribution for Selected Area"),
      plotlyOutput("street_category_plot", height = "300px")
    )
  ),
  
  hr(),
  

  h2("Section 3: Temporal Crime Patterns"),
  "Explore monthly trends for a specific crime category and compare categories within a selected month.",
  
  sidebarLayout(
    sidebarPanel(
      h4("Temporal Controls"),
      selectInput(
        inputId = "trend_crime",
        label = "Crime Category (Trend):",
        choices = sort(unique(trend_data$category)),
        selected = "violent-crime"
      ),
      selectInput(
        inputId = "month",
        label = "Month (Comparison):",
        choices = sprintf("%02d", 1:12),
        selected = "01"
      )
    ),
    mainPanel(
      h4("Plot 1: Monthly Trend for Selected Crime Category"),
      plotlyOutput("trend_plot", height = "350px"),
      hr(),
      
      h4("Plot 2: Crime Category Comparison for Selected Month"),
      plotlyOutput("bar_plot", height = "350px")
    )
  )
)


server <- function(input, output, session) {
  
 
  output$crime_map <- renderLeaflet({
    
    selected_data <- map_data %>%
      filter(category == input$map_crime)
    
    m <- leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(
        data = selected_data,
        lng = ~longitude,
        lat = ~latitude,
        radius = 3,
        fillOpacity = 0.6,
        stroke = FALSE,
        color = "red",
        clusterOptions = markerClusterOptions()
      )
    
    if (input$show_police) {
      m <- m %>%
        addPolygons(
          data = police_polygon_data,
          color = "blue",
          weight = 2,
          fillOpacity = 0.2,
          label = police_polygon_data$name
        ) %>%
        addCircles(
          data = police_point_data,
          lng = ~st_coordinates(geometry)[, 1],
          lat = ~st_coordinates(geometry)[, 2],
          radius = 5,
          color = "blue",
          fillOpacity = 0.8,
          label = police_point_data$name
        )
    }
    
    m
  })
  
  
  
  output$street_plot <- renderPlotly({
    
    df <- get_street_ranking(input$street_crime)
    
    p <- ggplot(df, aes(
      x = n,
      y = reorder(street_clean, n),
      fill = n,
      text = paste("Count:", n)
    )) +
      geom_col() +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(
        title = paste("Top 10 Areas for", input$street_crime),
        x = "Crime Count",
        y = "Area"
      )
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  
  
  output$top10_total_plot <- renderPlotly({
    
    df <- ranking_data %>%
      count(street_clean, sort = TRUE) %>%
      slice_head(n = 10)
    
    p <- ggplot(df, aes(
      x = n,
      y = reorder(street_clean, n),
      fill = n,
      customdata = street_clean,
      text = paste("Count:", n)
    )) +
      geom_col() +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(
        x = "Crime Count",
        y = "Area"
      )
    
    ggplotly(p, tooltip = "text", source = "total_top10") %>%
      config(displayModeBar = FALSE)
  })
  
  
  
  output$street_category_plot <- renderPlotly({
    
    click <- event_data("plotly_click", source = "total_top10")
    
    if (is.null(click)) {
      return(
        plotly_empty(type = "scatter") %>%
          layout(title = "Click a bar in Plot 2 to see crime category distribution") %>%
          config(displayModeBar = FALSE)
      )
    }
    
    area_clicked <- click$customdata
    
    df <- ranking_data %>%
      filter(street_clean == area_clicked) %>%
      count(category, sort = TRUE)
    
    plot_ly(
      df,
      labels = ~category,
      values = ~n,
      type = "pie",
      textinfo = "percent",
      hoverinfo = "text",
      text = ~paste("Category:", category, "<br>Count:", n)
    ) %>%
      layout(
        title = paste("Crime Category Distribution in", area_clicked),
        showlegend = TRUE,
        legend = list(orientation = "v", x = 1.05, y = 1)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  

  output$trend_plot <- renderPlotly({
    
    df <- trend_data %>%
      filter(category == input$trend_crime) %>%
      mutate(month_num = as.integer(as.character(month))) %>%
      arrange(month_num)
    
    p <- ggplot(df, aes(
      x = month_num,
      y = count,
      group = 1,
      text = paste("Month:", month_num, "<br>Count:", count)
    )) +
      geom_line(color = "darkblue", linewidth = 1.2) +
      geom_point(color = "darkblue", size = 3) +
      scale_x_continuous(breaks = 1:12) +
      theme_minimal() +
      labs(
        title = paste("Monthly Trend for", input$trend_crime),
        x = "Month",
        y = "Crime Count"
      )
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  
  output$bar_plot <- renderPlotly({
    
    df <- trend_data %>%
      mutate(month = sprintf("%02d", as.integer(as.character(month)))) %>%
      filter(month == input$month)
    
    p <- ggplot(df, aes(
      x = count,
      y = reorder(category, count),
      fill = count,
      text = paste("Count:", count)
    )) +
      geom_col() +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(
        title = paste("Crime Category Comparison in Month", input$month),
        x = "Crime Count",
        y = "Crime Category"
      )
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
}

shinyApp(ui, server)
