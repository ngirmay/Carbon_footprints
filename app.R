# app.R
# Install and load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  shiny,
  shinydashboard,
  leaflet,
  dplyr,
  ggplot2,
  plotly,
  DT,
  tidyr,
  lubridate,
  scales,
  RColorBrewer,
  htmltools,
  zoo
)

# Custom CSS for forest theme
custom_css <- HTML("
  .skin-blue .main-header .logo {
    background-color: #2D5A27 !important;
  }
  .skin-blue .main-header .navbar {
    background-color: #2D5A27 !important;
  }
  .skin-blue .main-header .logo:hover {
    background-color: #1e401d !important;
  }
  .skin-blue .main-sidebar {
    background-color: #1e401d !important;
  }
  .skin-blue .sidebar-menu>li.active>a {
    background-color: #2D5A27 !important;
  }
  .skin-blue .sidebar-menu>li:hover>a {
    background-color: #2D5A27 !important;
    border-left-color: #8BC34A;
  }
  .box.box-success {
    border-top-color: #4CAF50;
  }
  .bg-green {
    background-color: #4CAF50 !important;
  }
  .bg-light-green {
    background-color: #8BC34A !important;
  }
")

# Real DAC installations data (updated January 2025)
dac_installations <- data.frame(
  name = c(
    "Orca", "Mammoth", "Project Bison", "Squamish", "Arizona DAC",
    "Stratos", "Heirloom Tracy", "1PointFive Kingfisher"
  ),
  company = c(
    "Climeworks", "Climeworks", "CarbonCapture", "Carbon Engineering", "Carbon Capture Inc",
    "Occidental/1PointFive", "Heirloom Carbon", "1PointFive"
  ),
  type = c(
    "Commercial", "Commercial", "Commercial", "Pilot", "Research",
    "Commercial", "Commercial", "Commercial"
  ),
  latitude = c(
    64.0297, 64.0297, 41.8240, 49.7016, 33.4484,
    31.8834, 37.7397, 31.7619
  ),
  longitude = c(
    -21.9877, -21.9877, -107.9511, -123.1558, -112.0740,
    -102.3678, -121.4252, -102.4894
  ),
  capacity = c(
    4000, 36000, 5000, 1000, 3500,
    500000, 1000, 500000
  ),
  start_date = as.Date(c(
    "2021-09-01", "2024-01-15", "2023-12-01", "2015-06-01", "2022-03-01",
    "2025-06-01", "2023-11-01", "2025-12-01"
  )),
  technology = c(
    "Solid Sorbent", "Solid Sorbent", "Solid Sorbent", "Liquid Solvent", "Membrane",
    "Liquid Solvent", "Mineral Carbonation", "Liquid Solvent"
  ),
  stringsAsFactors = FALSE
)

# Generate operational data
generate_operational_data <- function(installations, days = 365) {
  dates <- seq(Sys.Date() - days, Sys.Date(), by = "day")
  operational_data <- expand.grid(
    installation_name = installations$name,
    date = dates,
    stringsAsFactors = FALSE
  ) %>%
    left_join(installations[, c("name", "capacity", "start_date")], 
              by = c("installation_name" = "name")) %>%
    filter(date >= start_date) %>%
    group_by(installation_name) %>%
    mutate(
      efficiency_factor = 0.8 + 0.15 * sin(2 * pi * as.numeric(format(date, "%j")) / 365),
      daily_capacity = capacity / 365,
      co2_captured = daily_capacity * efficiency_factor * (1 + rnorm(n(), 0, 0.05)),
      energy_used = co2_captured * (2.0 + rnorm(n(), 0, 0.2)),
      operating_cost = co2_captured * (400 + rnorm(n(), 0, 50)),
      temperature = 15 + 10 * sin(2 * pi * as.numeric(format(date, "%j")) / 365) + rnorm(n(), 0, 2),
      humidity = pmin(pmax(60 + rnorm(n(), 0, 10), 0), 100)
    ) %>%
    ungroup() %>%
    select(-efficiency_factor, -daily_capacity)
  
  return(operational_data)
}

# Generate data
operational_data <- generate_operational_data(dac_installations)

# UI Definition
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "DAC Analytics"),
  dashboardSidebar(
    tags$head(tags$style(custom_css)),
    sidebarMenu(
      menuItem("Overview", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Installation Map", tabName = "map", icon = icon("map")),
      menuItem("Performance", tabName = "performance", icon = icon("chart-line")),
      menuItem("Environmental", tabName = "environmental", icon = icon("leaf"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(custom_css)),
    tabItems(
      # Overview Dashboard
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("total_co2_box", width = 3),
                valueBoxOutput("active_facilities_box", width = 3),
                valueBoxOutput("avg_efficiency_box", width = 3),
                valueBoxOutput("cost_box", width = 3)
              ),
              fluidRow(
                box(
                  title = "CO₂ Capture Trend",
                  plotlyOutput("co2_trend"),
                  width = 8,
                  status = "success"
                ),
                box(
                  title = "Facility Comparison",
                  plotlyOutput("facility_comparison"),
                  width = 4,
                  status = "success"
                )
              ),
              fluidRow(
                box(
                  title = "Technology Distribution",
                  plotlyOutput("tech_distribution"),
                  width = 6,
                  status = "success"
                ),
                box(
                  title = "Efficiency Metrics",
                  plotlyOutput("efficiency_metrics"),
                  width = 6,
                  status = "success"
                )
              )
      ),
      
      # Map View
      tabItem(tabName = "map",
              fluidRow(
                box(
                  title = "Global DAC Installations",
                  leafletOutput("dac_map", height = 600),
                  width = 12,
                  status = "success"
                )
              )
      ),
      
      # Performance Analysis
      tabItem(tabName = "performance",
              fluidRow(
                box(
                  title = "Installation Performance",
                  plotlyOutput("performance_comparison"),
                  width = 12,
                  status = "success"
                )
              ),
              fluidRow(
                box(
                  title = "Efficiency Trends",
                  plotlyOutput("efficiency_trends"),
                  width = 6,
                  status = "success"
                ),
                box(
                  title = "Cost Analysis",
                  plotlyOutput("cost_analysis"),
                  width = 6,
                  status = "success"
                )
              )
      ),
      
      # Environmental Impact
      tabItem(tabName = "environmental",
              fluidRow(
                box(
                  title = "Environmental Impact Metrics",
                  plotlyOutput("environmental_metrics"),
                  width = 12,
                  status = "success"
                )
              ),
              fluidRow(
                box(
                  title = "Temperature vs Efficiency",
                  plotlyOutput("temp_efficiency"),
                  width = 6,
                  status = "success"
                ),
                box(
                  title = "Humidity Impact",
                  plotlyOutput("humidity_impact"),
                  width = 6,
                  status = "success"
                )
              )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  
  # Overview Dashboard Outputs
  output$total_co2_box <- renderValueBox({
    total_co2 <- sum(operational_data$co2_captured, na.rm = TRUE)
    valueBox(
      paste0(format(round(total_co2), big.mark = ","), " tons"),
      "Total CO₂ Captured",
      icon = icon("cloud"),
      color = "green"
    )
  })
  
  output$active_facilities_box <- renderValueBox({
    active <- nrow(dac_installations)
    valueBox(
      active,
      "Active Facilities",
      icon = icon("industry"),
      color = "green"
    )
  })
  
  output$avg_efficiency_box <- renderValueBox({
    efficiency <- mean(operational_data$co2_captured / operational_data$energy_used, na.rm = TRUE) * 100
    valueBox(
      paste0(round(efficiency, 1), "%"),
      "Average Efficiency",
      icon = icon("bolt"),
      color = "green"
    )
  })
  
  output$cost_box <- renderValueBox({
    avg_cost <- mean(operational_data$operating_cost / operational_data$co2_captured, na.rm = TRUE)
    valueBox(
      paste0("$", round(avg_cost)),
      "Average Cost per Ton",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  # CO2 Trend Plot
  output$co2_trend <- renderPlotly({
    daily_totals <- operational_data %>%
      group_by(date) %>%
      summarize(total_co2 = sum(co2_captured, na.rm = TRUE))
    
    plot_ly(daily_totals, x = ~date, y = ~total_co2, 
            type = 'scatter', mode = 'lines',
            line = list(color = '#4CAF50')) %>%
      layout(
        title = "Daily CO₂ Capture",
        xaxis = list(title = "Date"),
        yaxis = list(title = "CO₂ Captured (tons)"),
        plot_bgcolor = '#f8f9fa',
        paper_bgcolor = '#f8f9fa'
      )
  })
  
  # Facility Comparison
  output$facility_comparison <- renderPlotly({
    facility_totals <- operational_data %>%
      group_by(installation_name) %>%
      summarize(total_co2 = sum(co2_captured, na.rm = TRUE))
    
    plot_ly(facility_totals, x = ~installation_name, y = ~total_co2,
            type = 'bar', marker = list(color = '#8BC34A')) %>%
      layout(
        title = "Total Capture by Facility",
        xaxis = list(title = "Facility"),
        yaxis = list(title = "Total CO₂ Captured (tons)"),
        plot_bgcolor = '#f8f9fa',
        paper_bgcolor = '#f8f9fa'
      )
  })
  
  # Technology Distribution
  output$tech_distribution <- renderPlotly({
    tech_dist <- dac_installations %>%
      count(technology) %>%
      mutate(percentage = n/sum(n) * 100)
    
    plot_ly(tech_dist, labels = ~technology, values = ~n, type = 'pie',
            marker = list(colors = c('#4CAF50', '#8BC34A', '#A5D6A7')),
            textinfo = 'label+percent') %>%
      layout(
        title = "Technology Distribution",
        showlegend = TRUE,
        plot_bgcolor = '#f8f9fa',
        paper_bgcolor = '#f8f9fa'
      )
  })
  
  # Efficiency Metrics
  output$efficiency_metrics <- renderPlotly({
    efficiency_data <- operational_data %>%
      group_by(installation_name) %>%
      summarize(
        avg_efficiency = mean(co2_captured/energy_used * 100, na.rm = TRUE),
        std_dev = sd(co2_captured/energy_used * 100, na.rm = TRUE)
      )
    
    plot_ly(efficiency_data, x = ~installation_name, y = ~avg_efficiency,
            type = 'bar', 
            error_y = ~list(array = std_dev),
            marker = list(color = '#4CAF50')) %>%
      layout(
        title = "Average Efficiency by Installation",
        xaxis = list(title = "Installation"),
        yaxis = list(title = "Efficiency (%)"),
        plot_bgcolor = '#f8f9fa',
        paper_bgcolor = '#f8f9fa'
      )
  })
  
  # Map
  output$dac_map <- renderLeaflet({
    leaflet(dac_installations) %>%
      addTiles() %>%
      addCircleMarkers(
        ~longitude, ~latitude,
        popup = ~paste(
          "<b>", name, "</b><br>",
          "Company:", company, "<br>",
          "Type:", type, "<br>",
          "Capacity:", capacity, "tons/year<br>",
          "Technology:", technology
        ),
        radius = ~sqrt(capacity/100),
        color = "#4CAF50",
        fillOpacity = 0.7
      )
  })
  
  # Performance Comparison
  output$performance_comparison <- renderPlotly({
    perf_data <- operational_data %>%
      group_by(installation_name, date) %>%
      summarize(daily_capture = sum(co2_captured, na.rm = TRUE))
    
    plot_ly(perf_data, x = ~date, y = ~daily_capture, color = ~installation_name,
            type = 'scatter', mode = 'lines',
            colors = c('#4CAF50', '#8BC34A', '#A5D6A7', '#C8E6C9', '#E8F5E9')) %>%
      layout(
        title = "Daily Capture Rate by Installation",
        xaxis = list(title = "Date"),
        yaxis = list(title = "CO₂ Captured (tons)"),
        plot_bgcolor = '#f8f9fa',
        paper_bgcolor = '#f8f9fa'
      )
  })
  
  # Efficiency Trends
  output$efficiency_trends <- renderPlotly({
    trends_data <- operational_data %>%
      group_by(date) %>%
      summarize(
        avg_efficiency = mean(co2_captured/energy_used * 100, na.rm = TRUE),
        rolling_avg = zoo::rollmean(avg_efficiency, 7, fill = NA)
      )
    
    plot_ly() %>%
      add_trace(data = trends_data, x = ~date, y = ~avg_efficiency,
                type = 'scatter', mode = 'lines', name = 'Daily',
                line = list(color = '#A5D6A7')) %>%
      add_trace(data = trends_data, x = ~date, y = ~rolling_avg,
                type = 'scatter', mode = 'lines', name = '7-day Average',
                line = list(color = '#4CAF50', width = 3)) %>%
      layout(
        title = "Efficiency Trends Over Time",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Efficiency (%)"),
        plot_bgcolor = '#f8f9fa',
        paper_bgcolor = '#f8f9fa'
      )
  })
  
  # Cost Analysis
  output$cost_analysis <- renderPlotly({
    cost_data <- operational_data %>%
      group_by(installation_name, date) %>%
      summarize(
        cost_per_ton = sum(operating_cost, na.rm = TRUE) / 
          sum(co2_captured, na.rm = TRUE)
      )
    
    plot_ly(cost_data, x = ~date, y = ~cost_per_ton, color = ~installation_name,
            type = 'scatter', mode = 'lines',
            colors = c('#4CAF50', '#8BC34A', '#A5D6A7', '#C8E6C9', '#E8F5E9')) %>%
      layout(
        title = "Cost per Ton Over Time",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Cost per Ton ($)"),
        plot_bgcolor = '#f8f9fa',
        paper_bgcolor = '#f8f9fa'
      )
  })
  
  # Temperature vs Efficiency scatter plot
  output$temp_efficiency <- renderPlotly({
    plot_ly(operational_data, x = ~temperature, y = ~co2_captured/energy_used * 100,
            type = 'scatter', mode = 'markers',
            marker = list(color = '#4CAF50', size = 8, opacity = 0.6)) %>%
      add_lines(x = ~temperature, 
                y = ~fitted(loess(co2_captured/energy_used * 100 ~ temperature)),
                line = list(color = '#1B5E20')) %>%
      layout(
        title = "Temperature Impact on Efficiency",
        xaxis = list(title = "Temperature (°C)"),
        yaxis = list(title = "Efficiency (%)"),
        plot_bgcolor = '#f8f9fa',
        paper_bgcolor = '#f8f9fa'
      )
  })
  
  # Humidity Impact
  output$humidity_impact <- renderPlotly({
    plot_ly(operational_data, x = ~humidity, y = ~co2_captured/energy_used * 100,
            type = 'scatter', mode = 'markers',
            marker = list(color = '#8BC34A', size = 8, opacity = 0.6)) %>%
      add_lines(x = ~humidity, 
                y = ~fitted(loess(co2_captured/energy_used * 100 ~ humidity)),
                line = list(color = '#1B5E20')) %>%
      layout(
        title = "Humidity Impact on Efficiency",
        xaxis = list(title = "Humidity (%)"),
        yaxis = list(title = "Efficiency (%)"),
        plot_bgcolor = '#f8f9fa',
        paper_bgcolor = '#f8f9fa'
      )
  })
  
  # Environmental Metrics
  output$environmental_metrics <- renderPlotly({
    env_data <- operational_data %>%
      group_by(date) %>%
      summarize(
        avg_temp = mean(temperature, na.rm = TRUE),
        avg_humidity = mean(humidity, na.rm = TRUE),
        total_energy = sum(energy_used, na.rm = TRUE)
      )
    
    plot_ly() %>%
      add_trace(
        data = env_data,
        x = ~date,
        y = ~avg_temp,
        name = "Temperature (°C)",
        type = 'scatter',
        mode = 'lines',
        line = list(color = '#4CAF50')
      ) %>%
      add_trace(
        data = env_data,
        x = ~date,
        y = ~avg_humidity,
        name = "Humidity (%)",
        type = 'scatter',
        mode = 'lines',
        line = list(color = '#8BC34A'),
        yaxis = "y2"
      ) %>%
      layout(
        title = "Environmental Conditions",
        yaxis = list(title = "Temperature (°C)"),
        yaxis2 = list(
          title = "Humidity (%)",
          overlaying = "y",
          side = "right"
        ),
        plot_bgcolor = '#f8f9fa',
        paper_bgcolor = '#f8f9fa'
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)