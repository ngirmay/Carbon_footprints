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

# Load real DAC facilities data from CSV (sources: IEA, DOE, company announcements)
# Data compiled from:
# - IEA CCUS Projects Database
# - DOE Regional DAC Hubs Program
# - AlliedOffsets Global DAC Tracking
# - Company press releases (Climeworks, 1PointFive, Heirloom, etc.)

dac_facilities_file <- "data/dac_facilities.csv"

if (file.exists(dac_facilities_file)) {
  dac_installations <- read.csv(dac_facilities_file, stringsAsFactors = FALSE) %>%
    mutate(
      start_date = as.Date(start_date),
      capacity = capacity_tonnes_yr
    ) %>%
    rename(
      type = status,
      name = name
    )
} else {
  # Fallback to embedded data if CSV not found
  dac_installations <- data.frame(
    name = c(
      "Orca", "Mammoth", "Project Cypress", "Stratos", "Heirloom Tracy",
      "Project Bison Phase I", "Squamish Pilot", "Deep Sky Alpha"
    ),
    company = c(
      "Climeworks", "Climeworks", "Climeworks/Heirloom/Battelle", "1PointFive/Carbon Engineering",
      "Heirloom", "CarbonCapture Inc", "Carbon Engineering", "Deep Sky"
    ),
    country = c("Iceland", "Iceland", "USA", "USA", "USA", "USA", "Canada", "Canada"),
    region = c("Europe", "Europe", "Louisiana", "Texas", "California", "Wyoming", "British Columbia", "Alberta"),
    type = c(
      "Operational", "Operational", "Planned", "Under Construction", "Operational",
      "Operational", "Operational", "Under Construction"
    ),
    latitude = c(64.0392, 64.0392, 30.2241, 31.8457, 37.7397, 41.1400, 49.7016, 53.5461),
    longitude = c(-21.4011, -21.4011, -93.2174, -102.3676, -121.4252, -104.8202, -123.1558, -113.4938),
    capacity = c(4000, 36000, 1000000, 500000, 1000, 10000, 1000, 30000),
    start_date = as.Date(c(
      "2021-09-08", "2024-05-08", "2026-01-01", "2025-06-01", "2023-11-09",
      "2024-01-15", "2015-06-01", "2025-03-01"
    )),
    technology = c(
      "Solid Sorbent (Low-temp)", "Solid Sorbent (Low-temp)", "Hybrid (Solid + Limestone)",
      "Liquid Solvent (High-temp)", "Limestone Mineralization", "Solid Sorbent",
      "Liquid Solvent", "Multi-technology"
    ),
    stringsAsFactors = FALSE
  )
}

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
      menuItem("Facilities Database", tabName = "database", icon = icon("database")),
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

      # Facilities Database
      tabItem(tabName = "database",
              fluidRow(
                valueBoxOutput("total_capacity_box", width = 3),
                valueBoxOutput("operational_count_box", width = 3),
                valueBoxOutput("planned_capacity_box", width = 3),
                valueBoxOutput("countries_count_box", width = 3)
              ),
              fluidRow(
                box(
                  title = "Global DAC Facilities Database (Sources: IEA, DOE, Company Reports)",
                  DTOutput("facilities_table"),
                  width = 12,
                  status = "success"
                )
              ),
              fluidRow(
                box(
                  title = "Capacity by Status",
                  plotlyOutput("capacity_by_status"),
                  width = 6,
                  status = "success"
                ),
                box(
                  title = "Capacity by Region",
                  plotlyOutput("capacity_by_region"),
                  width = 6,
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
  
  # Map with color by status
  output$dac_map <- renderLeaflet({
    # Color palette by status
    status_colors <- colorFactor(
      palette = c("#4CAF50", "#FFC107", "#2196F3"),
      domain = c("Operational", "Under Construction", "Planned")
    )

    leaflet(dac_installations) %>%
      addTiles() %>%
      addCircleMarkers(
        ~longitude, ~latitude,
        popup = ~paste(
          "<b>", name, "</b><br>",
          "Company:", company, "<br>",
          "Status:", type, "<br>",
          "Capacity:", format(capacity, big.mark = ","), "tonnes/year<br>",
          "Technology:", technology, "<br>",
          "Region:", ifelse(exists("region"), region, "N/A")
        ),
        radius = ~pmin(sqrt(capacity/500), 25),
        color = ~status_colors(type),
        fillOpacity = 0.7
      ) %>%
      addLegend(
        position = "bottomright",
        colors = c("#4CAF50", "#FFC107", "#2196F3"),
        labels = c("Operational", "Under Construction", "Planned"),
        title = "Status"
      )
  })

  # ═══════════════════════════════════════════════════════════════════════════
  # DATABASE TAB - Facilities table and summary stats
  # ═══════════════════════════════════════════════════════════════════════════

  output$total_capacity_box <- renderValueBox({
    total <- sum(dac_installations$capacity, na.rm = TRUE)
    valueBox(
      paste0(format(round(total/1000), big.mark = ","), " kt/yr"),
      "Total Announced Capacity",
      icon = icon("industry"),
      color = "green"
    )
  })

  output$operational_count_box <- renderValueBox({
    operational <- sum(dac_installations$type == "Operational", na.rm = TRUE)
    valueBox(
      operational,
      "Operational Facilities",
      icon = icon("check-circle"),
      color = "green"
    )
  })

  output$planned_capacity_box <- renderValueBox({
    planned <- sum(dac_installations$capacity[dac_installations$type %in% c("Planned", "Under Construction")], na.rm = TRUE)
    valueBox(
      paste0(format(round(planned/1000), big.mark = ","), " kt/yr"),
      "Pipeline Capacity",
      icon = icon("clock"),
      color = "yellow"
    )
  })

  output$countries_count_box <- renderValueBox({
    countries <- length(unique(dac_installations$country))
    valueBox(
      countries,
      "Countries",
      icon = icon("globe"),
      color = "blue"
    )
  })

  output$facilities_table <- renderDT({
    display_data <- dac_installations %>%
      select(name, company, country, region, type, capacity, technology, start_date) %>%
      mutate(
        capacity = format(capacity, big.mark = ","),
        start_date = format(start_date, "%Y-%m")
      ) %>%
      rename(
        "Facility" = name,
        "Company" = company,
        "Country" = country,
        "Region" = region,
        "Status" = type,
        "Capacity (t/yr)" = capacity,
        "Technology" = technology,
        "Start Date" = start_date
      )

    datatable(
      display_data,
      options = list(
        pageLength = 15,
        searchHighlight = TRUE,
        order = list(list(4, 'asc'))  # Sort by status
      ),
      filter = 'top',
      rownames = FALSE
    )
  })

  output$capacity_by_status <- renderPlotly({
    status_data <- dac_installations %>%
      group_by(type) %>%
      summarize(
        total_capacity = sum(capacity, na.rm = TRUE) / 1000,
        count = n()
      ) %>%
      mutate(type = factor(type, levels = c("Operational", "Under Construction", "Planned")))

    plot_ly(status_data, x = ~type, y = ~total_capacity,
            type = 'bar',
            text = ~paste(count, "facilities"),
            marker = list(color = c('#4CAF50', '#FFC107', '#2196F3'))) %>%
      layout(
        title = "Capacity by Development Status",
        xaxis = list(title = "Status"),
        yaxis = list(title = "Capacity (kt CO₂/year)"),
        plot_bgcolor = '#f8f9fa',
        paper_bgcolor = '#f8f9fa'
      )
  })

  output$capacity_by_region <- renderPlotly({
    region_data <- dac_installations %>%
      group_by(country) %>%
      summarize(
        total_capacity = sum(capacity, na.rm = TRUE) / 1000,
        count = n()
      ) %>%
      arrange(desc(total_capacity))

    plot_ly(region_data, x = ~reorder(country, total_capacity), y = ~total_capacity,
            type = 'bar',
            text = ~paste(count, "facilities"),
            marker = list(color = '#8BC34A')) %>%
      layout(
        title = "Capacity by Country",
        xaxis = list(title = "Country"),
        yaxis = list(title = "Capacity (kt CO₂/year)"),
        plot_bgcolor = '#f8f9fa',
        paper_bgcolor = '#f8f9fa'
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