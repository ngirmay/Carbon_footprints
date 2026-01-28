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

# Clean light theme CSS with teal accent
custom_css <- HTML("
  /* Main header - teal gradient */
  .skin-blue .main-header .logo {
    background: linear-gradient(135deg, #0d9488 0%, #14b8a6 100%) !important;
    color: #ffffff !important;
    font-weight: 600;
  }
  .skin-blue .main-header .navbar {
    background: linear-gradient(135deg, #0d9488 0%, #14b8a6 100%) !important;
  }
  .skin-blue .main-header .logo:hover {
    background: linear-gradient(135deg, #0f766e 0%, #0d9488 100%) !important;
  }
  .skin-blue .main-header .navbar .sidebar-toggle {
    color: #ffffff !important;
  }

  /* Sidebar - clean white */
  .skin-blue .main-sidebar {
    background-color: #ffffff !important;
    box-shadow: 2px 0 5px rgba(0,0,0,0.1);
  }
  .skin-blue .sidebar a {
    color: #374151 !important;
  }
  .skin-blue .sidebar-menu > li > a {
    border-left: 3px solid transparent;
  }
  .skin-blue .sidebar-menu > li.active > a {
    background-color: #f0fdfa !important;
    color: #0d9488 !important;
    border-left: 3px solid #14b8a6;
    font-weight: 500;
  }
  .skin-blue .sidebar-menu > li:hover > a {
    background-color: #f0fdfa !important;
    color: #0d9488 !important;
  }
  .skin-blue .sidebar-menu > li > .treeview-menu {
    background-color: #f9fafb !important;
  }

  /* Content area - light gray */
  .content-wrapper {
    background-color: #f3f4f6 !important;
  }

  /* Boxes - white with subtle shadows */
  .box {
    background-color: #ffffff !important;
    border-top: 3px solid #14b8a6 !important;
    border-radius: 12px;
    box-shadow: 0 1px 3px rgba(0, 0, 0, 0.1), 0 1px 2px rgba(0, 0, 0, 0.06);
  }
  .box-header {
    color: #1f2937 !important;
    border-bottom: 1px solid #e5e7eb;
  }
  .box-title {
    color: #1f2937 !important;
    font-weight: 600;
  }
  .box.box-success {
    border-top-color: #10b981 !important;
  }
  .box.box-primary {
    border-top-color: #3b82f6 !important;
  }
  .box.box-warning {
    border-top-color: #f59e0b !important;
  }
  .box.box-danger {
    border-top-color: #ef4444 !important;
  }

  /* Value boxes - vibrant colors */
  .small-box {
    border-radius: 12px;
    box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
  }
  .small-box h3 {
    font-size: 32px;
    font-weight: 700;
  }
  .small-box.bg-aqua {
    background: linear-gradient(135deg, #06b6d4 0%, #22d3ee 100%) !important;
  }
  .small-box.bg-green {
    background: linear-gradient(135deg, #10b981 0%, #34d399 100%) !important;
  }
  .small-box.bg-yellow {
    background: linear-gradient(135deg, #f59e0b 0%, #fbbf24 100%) !important;
  }
  .small-box.bg-blue {
    background: linear-gradient(135deg, #3b82f6 0%, #60a5fa 100%) !important;
  }
  .small-box.bg-purple {
    background: linear-gradient(135deg, #8b5cf6 0%, #a78bfa 100%) !important;
  }
  .small-box.bg-teal {
    background: linear-gradient(135deg, #14b8a6 0%, #2dd4bf 100%) !important;
  }

  /* DataTables - clean styling */
  .dataTables_wrapper {
    color: #374151 !important;
  }
  table.dataTable {
    color: #1f2937 !important;
    background-color: #ffffff !important;
  }
  table.dataTable thead th {
    background-color: #f9fafb !important;
    color: #0d9488 !important;
    border-bottom: 2px solid #14b8a6 !important;
    font-weight: 600;
  }
  table.dataTable tbody tr {
    background-color: #ffffff !important;
  }
  table.dataTable tbody tr:hover {
    background-color: #f0fdfa !important;
  }
  table.dataTable tbody tr:nth-child(even) {
    background-color: #f9fafb !important;
  }
  .dataTables_filter input {
    background-color: #ffffff !important;
    color: #1f2937 !important;
    border: 1px solid #d1d5db !important;
    border-radius: 6px;
    padding: 6px 12px;
  }
  .dataTables_filter input:focus {
    border-color: #14b8a6 !important;
    outline: none;
    box-shadow: 0 0 0 3px rgba(20, 184, 166, 0.1);
  }
  .dataTables_length select {
    background-color: #ffffff !important;
    color: #1f2937 !important;
    border: 1px solid #d1d5db !important;
    border-radius: 6px;
  }

  /* Map container */
  .leaflet-container {
    border-radius: 8px;
  }
")

# Color palette for charts (light theme)
chart_colors <- list(
  primary = "#14b8a6",
  secondary = "#3b82f6",
  accent = "#f59e0b",
  success = "#10b981",
  warning = "#eab308",
  danger = "#ef4444",
  purple = "#8b5cf6",
  pink = "#ec4899",
  bg = "#ffffff",
  grid = "#e5e7eb",
  text = "#374151"
)

plotly_light_layout <- function(p, title = "") {
  p %>% layout(
    title = list(text = title, font = list(color = chart_colors$text, size = 14)),
    paper_bgcolor = chart_colors$bg,
    plot_bgcolor = chart_colors$bg,
    xaxis = list(
      gridcolor = chart_colors$grid,
      tickfont = list(color = chart_colors$text),
      titlefont = list(color = chart_colors$text),
      linecolor = chart_colors$grid
    ),
    yaxis = list(
      gridcolor = chart_colors$grid,
      tickfont = list(color = chart_colors$text),
      titlefont = list(color = chart_colors$text),
      linecolor = chart_colors$grid
    ),
    legend = list(font = list(color = chart_colors$text))
  )
}

# Load real DAC facilities data from CSV
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
    name = c("Orca", "Mammoth", "Project Cypress", "Stratos", "Heirloom Tracy",
             "Project Bison Phase I", "Squamish Pilot", "Deep Sky Alpha"),
    company = c("Climeworks", "Climeworks", "Climeworks/Heirloom/Battelle",
                "1PointFive/Carbon Engineering", "Heirloom", "CarbonCapture Inc",
                "Carbon Engineering", "Deep Sky"),
    country = c("Iceland", "Iceland", "USA", "USA", "USA", "USA", "Canada", "Canada"),
    region = c("Europe", "Europe", "Louisiana", "Texas", "California", "Wyoming",
               "British Columbia", "Alberta"),
    type = c("Operational", "Operational", "Planned", "Under Construction", "Operational",
             "Operational", "Operational", "Under Construction"),
    latitude = c(64.0392, 64.0392, 30.2241, 31.8457, 37.7397, 41.1400, 49.7016, 53.5461),
    longitude = c(-21.4011, -21.4011, -93.2174, -102.3676, -121.4252, -104.8202, -123.1558, -113.4938),
    capacity = c(4000, 36000, 1000000, 500000, 1000, 10000, 1000, 30000),
    start_date = as.Date(c("2021-09-08", "2024-05-08", "2026-01-01", "2025-06-01",
                           "2023-11-09", "2024-01-15", "2015-06-01", "2025-03-01")),
    technology = c("Solid Sorbent (Low-temp)", "Solid Sorbent (Low-temp)",
                   "Hybrid (Solid + Limestone)", "Liquid Solvent (High-temp)",
                   "Limestone Mineralization", "Solid Sorbent", "Liquid Solvent",
                   "Multi-technology"),
    stringsAsFactors = FALSE
  )
}

# Generate operational data with more realistic patterns
generate_operational_data <- function(installations, days = 365) {
  dates <- seq(Sys.Date() - days, Sys.Date(), by = "day")
  operational_data <- expand.grid(
    installation_name = installations$name,
    date = dates,
    stringsAsFactors = FALSE
  ) %>%
    left_join(installations[, c("name", "capacity", "start_date", "technology")],
              by = c("installation_name" = "name")) %>%
    filter(date >= start_date) %>%
    group_by(installation_name) %>%
    mutate(
      # Technology-specific efficiency factors
      tech_factor = case_when(
        grepl("Solid Sorbent", technology) ~ 0.85,
        grepl("Liquid Solvent", technology) ~ 0.78,
        grepl("Limestone", technology) ~ 0.82,
        grepl("Electrochemical", technology) ~ 0.75,
        TRUE ~ 0.80
      ),
      # Seasonal efficiency variation
      seasonal_factor = 0.9 + 0.1 * cos(2 * pi * as.numeric(format(date, "%j")) / 365),
      # Ramp-up factor for new facilities (takes 6 months to reach full capacity)
      days_operational = as.numeric(date - start_date),
      ramp_up_factor = pmin(days_operational / 180, 1),
      # Calculate daily values
      daily_capacity = capacity / 365,
      efficiency = tech_factor * seasonal_factor * ramp_up_factor * (1 + rnorm(n(), 0, 0.03)),
      co2_captured = daily_capacity * efficiency,
      # Energy consumption varies by technology
      energy_factor = case_when(
        grepl("Solid Sorbent", technology) ~ 1.8,
        grepl("Liquid Solvent", technology) ~ 2.5,
        grepl("Limestone", technology) ~ 1.5,
        TRUE ~ 2.0
      ),
      energy_used = co2_captured * energy_factor * (1 + rnorm(n(), 0, 0.1)),
      # Cost varies by scale and technology
      base_cost = case_when(
        capacity >= 100000 ~ 250,
        capacity >= 10000 ~ 350,
        TRUE ~ 500
      ),
      operating_cost = co2_captured * base_cost * (1 + rnorm(n(), 0, 0.1)),
      # Environmental conditions
      temperature = 15 + 10 * sin(2 * pi * as.numeric(format(date, "%j")) / 365) + rnorm(n(), 0, 3),
      humidity = pmin(pmax(55 + 15 * cos(2 * pi * as.numeric(format(date, "%j")) / 365) + rnorm(n(), 0, 8), 20), 95)
    ) %>%
    ungroup() %>%
    select(-tech_factor, -seasonal_factor, -ramp_up_factor, -daily_capacity, -energy_factor, -base_cost, -days_operational)

  return(operational_data)
}

# Generate data
operational_data <- generate_operational_data(dac_installations)

# UI Definition
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "DAC Analytics"),
  dashboardSidebar(
    tags$head(tags$style(custom_css)),
    sidebarMenu(
      menuItem("Overview", tabName = "dashboard", icon = icon("gauge-high")),
      menuItem("Installation Map", tabName = "map", icon = icon("earth-americas")),
      menuItem("Facilities Database", tabName = "database", icon = icon("database")),
      menuItem("Market Analysis", tabName = "market", icon = icon("chart-pie")),
      menuItem("Performance", tabName = "performance", icon = icon("chart-line")),
      menuItem("Cost Economics", tabName = "economics", icon = icon("dollar-sign")),
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
                  title = "Cumulative CO₂ Capture Progress",
                  plotlyOutput("cumulative_capture", height = 300),
                  width = 8,
                  status = "success"
                ),
                box(
                  title = "Top Performers",
                  plotlyOutput("top_performers", height = 300),
                  width = 4,
                  status = "success"
                )
              ),
              fluidRow(
                box(
                  title = "Technology Mix",
                  plotlyOutput("tech_distribution", height = 280),
                  width = 4,
                  status = "primary"
                ),
                box(
                  title = "Capture by Technology",
                  plotlyOutput("capture_by_tech", height = 280),
                  width = 4,
                  status = "primary"
                ),
                box(
                  title = "Efficiency by Technology",
                  plotlyOutput("efficiency_by_tech", height = 280),
                  width = 4,
                  status = "primary"
                )
              )
      ),

      # Map View
      tabItem(tabName = "map",
              fluidRow(
                box(
                  title = "Global DAC Installations",
                  leafletOutput("dac_map", height = 650),
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
                  plotlyOutput("capacity_by_status", height = 300),
                  width = 6,
                  status = "primary"
                ),
                box(
                  title = "Capacity by Country",
                  plotlyOutput("capacity_by_region", height = 300),
                  width = 6,
                  status = "primary"
                )
              )
      ),

      # NEW: Market Analysis Tab
      tabItem(tabName = "market",
              fluidRow(
                valueBoxOutput("market_growth_box", width = 3),
                valueBoxOutput("avg_facility_size_box", width = 3),
                valueBoxOutput("pipeline_multiplier_box", width = 3),
                valueBoxOutput("leading_company_box", width = 3)
              ),
              fluidRow(
                box(
                  title = "Capacity Growth Timeline",
                  plotlyOutput("capacity_timeline", height = 350),
                  width = 8,
                  status = "success"
                ),
                box(
                  title = "Company Market Share",
                  plotlyOutput("company_share", height = 350),
                  width = 4,
                  status = "success"
                )
              ),
              fluidRow(
                box(
                  title = "Technology Adoption Over Time",
                  plotlyOutput("tech_adoption", height = 300),
                  width = 6,
                  status = "primary"
                ),
                box(
                  title = "Geographic Distribution",
                  plotlyOutput("geo_distribution", height = 300),
                  width = 6,
                  status = "primary"
                )
              )
      ),

      # Performance Analysis
      tabItem(tabName = "performance",
              fluidRow(
                box(
                  title = "Daily Capture Rate by Installation",
                  plotlyOutput("performance_comparison", height = 350),
                  width = 12,
                  status = "success"
                )
              ),
              fluidRow(
                box(
                  title = "Efficiency Trends (7-day Rolling Average)",
                  plotlyOutput("efficiency_trends", height = 300),
                  width = 6,
                  status = "primary"
                ),
                box(
                  title = "Capacity Utilization",
                  plotlyOutput("utilization_chart", height = 300),
                  width = 6,
                  status = "primary"
                )
              )
      ),

      # NEW: Cost Economics Tab
      tabItem(tabName = "economics",
              fluidRow(
                valueBoxOutput("lowest_cost_box", width = 3),
                valueBoxOutput("highest_cost_box", width = 3),
                valueBoxOutput("avg_cost_trend_box", width = 3),
                valueBoxOutput("cost_reduction_box", width = 3)
              ),
              fluidRow(
                box(
                  title = "Cost per Ton by Facility",
                  plotlyOutput("cost_comparison", height = 350),
                  width = 6,
                  status = "success"
                ),
                box(
                  title = "Cost vs Scale (Learning Curve)",
                  plotlyOutput("cost_scale_curve", height = 350),
                  width = 6,
                  status = "success"
                )
              ),
              fluidRow(
                box(
                  title = "Cost Trend Over Time",
                  plotlyOutput("cost_trend", height = 300),
                  width = 8,
                  status = "primary"
                ),
                box(
                  title = "Cost Distribution",
                  plotlyOutput("cost_histogram", height = 300),
                  width = 4,
                  status = "primary"
                )
              )
      ),

      # Environmental Impact
      tabItem(tabName = "environmental",
              fluidRow(
                valueBoxOutput("total_captured_box", width = 3),
                valueBoxOutput("energy_efficiency_box", width = 3),
                valueBoxOutput("cars_equivalent_box", width = 3),
                valueBoxOutput("trees_equivalent_box", width = 3)
              ),
              fluidRow(
                box(
                  title = "Environmental Conditions Timeline",
                  plotlyOutput("environmental_metrics", height = 350),
                  width = 12,
                  status = "success"
                )
              ),
              fluidRow(
                box(
                  title = "Temperature vs Efficiency",
                  plotlyOutput("temp_efficiency", height = 300),
                  width = 6,
                  status = "primary"
                ),
                box(
                  title = "Humidity vs Efficiency",
                  plotlyOutput("humidity_impact", height = 300),
                  width = 6,
                  status = "primary"
                )
              )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {

  # ═══════════════════════════════════════════════════════════════════════════
  # OVERVIEW TAB
  # ═══════════════════════════════════════════════════════════════════════════

  output$total_co2_box <- renderValueBox({
    total_co2 <- sum(operational_data$co2_captured, na.rm = TRUE)
    valueBox(
      paste0(format(round(total_co2), big.mark = ","), " t"),
      "Total CO₂ Captured",
      icon = icon("cloud"),
      color = "teal"
    )
  })

  output$active_facilities_box <- renderValueBox({
    active <- sum(dac_installations$type == "Operational")
    valueBox(
      active,
      "Operational Facilities",
      icon = icon("industry"),
      color = "green"
    )
  })

  output$avg_efficiency_box <- renderValueBox({
    efficiency <- mean(operational_data$efficiency, na.rm = TRUE) * 100
    valueBox(
      paste0(round(efficiency, 1), "%"),
      "Avg Capture Efficiency",
      icon = icon("gauge-high"),
      color = "blue"
    )
  })

  output$cost_box <- renderValueBox({
    avg_cost <- mean(operational_data$operating_cost / operational_data$co2_captured, na.rm = TRUE)
    valueBox(
      paste0("$", round(avg_cost)),
      "Avg Cost per Ton",
      icon = icon("dollar-sign"),
      color = "purple"
    )
  })

  # Cumulative Capture Chart
  output$cumulative_capture <- renderPlotly({
    cum_data <- operational_data %>%
      arrange(date) %>%
      group_by(date) %>%
      summarize(daily_total = sum(co2_captured, na.rm = TRUE)) %>%
      mutate(cumulative = cumsum(daily_total))

    plot_ly(cum_data, x = ~date, y = ~cumulative,
            type = 'scatter', mode = 'lines',
            fill = 'tozeroy',
            line = list(color = chart_colors$primary, width = 2),
            fillcolor = 'rgba(0, 212, 170, 0.2)') %>%
      plotly_light_layout("Cumulative CO₂ Captured (tonnes)") %>%
      layout(yaxis = list(title = "Cumulative CO₂ (tonnes)"),
             xaxis = list(title = ""))
  })

  # Top Performers
  output$top_performers <- renderPlotly({
    top_data <- operational_data %>%
      group_by(installation_name) %>%
      summarize(total = sum(co2_captured, na.rm = TRUE)) %>%
      arrange(desc(total)) %>%
      head(6)

    plot_ly(top_data, x = ~reorder(installation_name, total), y = ~total,
            type = 'bar', orientation = 'h',
            marker = list(color = chart_colors$primary)) %>%
      plotly_light_layout("") %>%
      layout(xaxis = list(title = "CO₂ Captured (tonnes)"),
             yaxis = list(title = ""))
  })

  # Technology Distribution Pie
  output$tech_distribution <- renderPlotly({
    tech_data <- dac_installations %>%
      mutate(tech_simple = case_when(
        grepl("Solid Sorbent", technology) ~ "Solid Sorbent",
        grepl("Liquid Solvent", technology) ~ "Liquid Solvent",
        grepl("Limestone|Mineralization", technology) ~ "Mineralization",
        grepl("Electrochemical", technology) ~ "Electrochemical",
        TRUE ~ "Other"
      )) %>%
      count(tech_simple)

    plot_ly(tech_data, labels = ~tech_simple, values = ~n, type = 'pie',
            marker = list(colors = c(chart_colors$primary, chart_colors$secondary,
                                    chart_colors$accent, chart_colors$purple, chart_colors$pink)),
            textinfo = 'label+percent',
            textfont = list(color = '#ffffff')) %>%
      plotly_light_layout("") %>%
      layout(showlegend = FALSE)
  })

  # Capture by Technology
  output$capture_by_tech <- renderPlotly({
    tech_capture <- operational_data %>%
      mutate(tech_simple = case_when(
        grepl("Solid Sorbent", technology) ~ "Solid Sorbent",
        grepl("Liquid Solvent", technology) ~ "Liquid Solvent",
        grepl("Limestone|Mineralization", technology) ~ "Mineralization",
        grepl("Electrochemical", technology) ~ "Electrochemical",
        TRUE ~ "Other"
      )) %>%
      group_by(tech_simple) %>%
      summarize(total = sum(co2_captured, na.rm = TRUE) / 1000) %>%
      arrange(desc(total))

    plot_ly(tech_capture, x = ~reorder(tech_simple, total), y = ~total,
            type = 'bar',
            marker = list(color = c(chart_colors$primary, chart_colors$secondary,
                                   chart_colors$accent, chart_colors$purple))) %>%
      plotly_light_layout("") %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "CO₂ (kt)"))
  })

  # Efficiency by Technology
  output$efficiency_by_tech <- renderPlotly({
    eff_data <- operational_data %>%
      mutate(tech_simple = case_when(
        grepl("Solid Sorbent", technology) ~ "Solid Sorbent",
        grepl("Liquid Solvent", technology) ~ "Liquid Solvent",
        grepl("Limestone|Mineralization", technology) ~ "Mineralization",
        TRUE ~ "Other"
      )) %>%
      group_by(tech_simple) %>%
      summarize(
        avg_eff = mean(efficiency, na.rm = TRUE) * 100,
        sd_eff = sd(efficiency, na.rm = TRUE) * 100
      )

    plot_ly(eff_data, x = ~tech_simple, y = ~avg_eff,
            type = 'bar',
            error_y = list(array = ~sd_eff, color = chart_colors$text),
            marker = list(color = chart_colors$secondary)) %>%
      plotly_light_layout("") %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Efficiency (%)", range = c(0, 100)))
  })

  # ═══════════════════════════════════════════════════════════════════════════
  # MAP TAB
  # ═══════════════════════════════════════════════════════════════════════════

  output$dac_map <- renderLeaflet({
    # Vibrant, distinct colors for each status
    status_colors <- c("Operational" = "#10b981",
                       "Under Construction" = "#f59e0b",
                       "Planned" = "#3b82f6")

    leaflet(dac_installations) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -20, lat = 35, zoom = 2) %>%
      addCircleMarkers(
        ~longitude, ~latitude,
        popup = ~paste0(
          "<div style='font-family: system-ui, sans-serif; min-width: 200px;'>",
          "<h4 style='margin: 0 0 8px 0; color: #1f2937;'>", name, "</h4>",
          "<table style='font-size: 13px; color: #4b5563;'>",
          "<tr><td style='padding: 2px 8px 2px 0; font-weight: 500;'>Company:</td><td>", company, "</td></tr>",
          "<tr><td style='padding: 2px 8px 2px 0; font-weight: 500;'>Status:</td><td><span style='background:", status_colors[type], "; color: white; padding: 2px 8px; border-radius: 4px; font-size: 11px;'>", type, "</span></td></tr>",
          "<tr><td style='padding: 2px 8px 2px 0; font-weight: 500;'>Capacity:</td><td><b>", format(capacity, big.mark = ","), "</b> t/yr</td></tr>",
          "<tr><td style='padding: 2px 8px 2px 0; font-weight: 500;'>Technology:</td><td>", technology, "</td></tr>",
          "<tr><td style='padding: 2px 8px 2px 0; font-weight: 500;'>Location:</td><td>", region, ", ", country, "</td></tr>",
          "</table>",
          "</div>"
        ),
        label = ~name,
        radius = ~pmax(pmin(sqrt(capacity/200), 35), 8),
        color = "#ffffff",
        weight = 2,
        fillColor = ~status_colors[type],
        fillOpacity = 0.85,
        stroke = TRUE
      ) %>%
      addLegend(
        position = "bottomright",
        colors = unname(status_colors),
        labels = names(status_colors),
        title = "Facility Status",
        opacity = 1
      )
  })

  # ═══════════════════════════════════════════════════════════════════════════
  # DATABASE TAB
  # ═══════════════════════════════════════════════════════════════════════════

  output$total_capacity_box <- renderValueBox({
    total <- sum(dac_installations$capacity, na.rm = TRUE)
    valueBox(
      paste0(format(round(total/1000), big.mark = ","), " kt/yr"),
      "Total Announced Capacity",
      icon = icon("industry"),
      color = "teal"
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
        "Facility" = name, "Company" = company, "Country" = country,
        "Region" = region, "Status" = type, "Capacity (t/yr)" = capacity,
        "Technology" = technology, "Start Date" = start_date
      )

    datatable(display_data,
      options = list(pageLength = 15, searchHighlight = TRUE,
                    order = list(list(4, 'asc'))),
      filter = 'top', rownames = FALSE
    ) %>%
      formatStyle('Status',
        backgroundColor = styleEqual(
          c('Operational', 'Under Construction', 'Planned'),
          c('#059669', '#d97706', '#2563eb')
        ),
        color = 'white'
      )
  })

  output$capacity_by_status <- renderPlotly({
    status_data <- dac_installations %>%
      group_by(type) %>%
      summarize(total_capacity = sum(capacity, na.rm = TRUE) / 1000, count = n()) %>%
      mutate(type = factor(type, levels = c("Operational", "Under Construction", "Planned")))

    plot_ly(status_data, x = ~type, y = ~total_capacity,
            type = 'bar', text = ~paste(count, "facilities"),
            marker = list(color = c('#00d4aa', '#f6ad55', '#4299e1'))) %>%
      plotly_light_layout("") %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Capacity (kt/yr)"))
  })

  output$capacity_by_region <- renderPlotly({
    region_data <- dac_installations %>%
      group_by(country) %>%
      summarize(total_capacity = sum(capacity, na.rm = TRUE) / 1000, count = n()) %>%
      arrange(desc(total_capacity))

    plot_ly(region_data, x = ~reorder(country, total_capacity), y = ~total_capacity,
            type = 'bar', text = ~paste(count, "facilities"),
            marker = list(color = chart_colors$secondary)) %>%
      plotly_light_layout("") %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Capacity (kt/yr)"))
  })

  # ═══════════════════════════════════════════════════════════════════════════
  # MARKET ANALYSIS TAB (NEW)
  # ═══════════════════════════════════════════════════════════════════════════

  output$market_growth_box <- renderValueBox({
    operational_cap <- sum(dac_installations$capacity[dac_installations$type == "Operational"], na.rm = TRUE)
    total_cap <- sum(dac_installations$capacity, na.rm = TRUE)
    growth <- round((total_cap / operational_cap - 1) * 100)
    valueBox(
      paste0(growth, "x"),
      "Pipeline vs Operational",
      icon = icon("arrow-trend-up"),
      color = "teal"
    )
  })

  output$avg_facility_size_box <- renderValueBox({
    avg_size <- mean(dac_installations$capacity, na.rm = TRUE)
    valueBox(
      paste0(format(round(avg_size/1000), big.mark = ","), " kt"),
      "Avg Facility Size",
      icon = icon("building"),
      color = "blue"
    )
  })

  output$pipeline_multiplier_box <- renderValueBox({
    planned <- sum(dac_installations$capacity[dac_installations$type == "Planned"], na.rm = TRUE)
    operational <- sum(dac_installations$capacity[dac_installations$type == "Operational"], na.rm = TRUE)
    mult <- round(planned / operational)
    valueBox(
      paste0(mult, "x"),
      "Planned vs Operational",
      icon = icon("rocket"),
      color = "purple"
    )
  })

  output$leading_company_box <- renderValueBox({
    top_company <- dac_installations %>%
      group_by(company) %>%
      summarize(cap = sum(capacity)) %>%
      arrange(desc(cap)) %>%
      slice(1) %>%
      pull(company)
    valueBox(
      substr(top_company, 1, 15),
      "Leading by Capacity",
      icon = icon("trophy"),
      color = "yellow"
    )
  })

  # Capacity Timeline
  output$capacity_timeline <- renderPlotly({
    timeline_data <- dac_installations %>%
      arrange(start_date) %>%
      mutate(
        year = year(start_date),
        cumulative_cap = cumsum(capacity) / 1000000
      ) %>%
      group_by(year, type) %>%
      summarize(
        added_cap = sum(capacity) / 1000,
        .groups = "drop"
      )

    plot_ly(timeline_data, x = ~year, y = ~added_cap, color = ~type,
            type = 'bar',
            colors = c("Operational" = chart_colors$primary,
                      "Under Construction" = chart_colors$accent,
                      "Planned" = chart_colors$secondary)) %>%
      plotly_light_layout("Capacity Additions by Year") %>%
      layout(barmode = 'stack',
             xaxis = list(title = "Year", dtick = 1),
             yaxis = list(title = "Capacity Added (kt/yr)"))
  })

  # Company Market Share
  output$company_share <- renderPlotly({
    company_data <- dac_installations %>%
      mutate(company_short = gsub("/.*", "", company)) %>%
      group_by(company_short) %>%
      summarize(cap = sum(capacity, na.rm = TRUE)) %>%
      arrange(desc(cap)) %>%
      head(8)

    plot_ly(company_data, labels = ~company_short, values = ~cap, type = 'pie',
            marker = list(colors = c(chart_colors$primary, chart_colors$secondary,
                                    chart_colors$accent, chart_colors$purple,
                                    chart_colors$pink, chart_colors$success,
                                    chart_colors$warning, chart_colors$danger)),
            textinfo = 'label+percent',
            textfont = list(color = '#ffffff', size = 10)) %>%
      plotly_light_layout("") %>%
      layout(showlegend = FALSE)
  })

  # Technology Adoption Timeline
  output$tech_adoption <- renderPlotly({
    tech_timeline <- dac_installations %>%
      mutate(
        year = year(start_date),
        tech_simple = case_when(
          grepl("Solid Sorbent", technology) ~ "Solid Sorbent",
          grepl("Liquid Solvent", technology) ~ "Liquid Solvent",
          grepl("Limestone|Mineralization", technology) ~ "Mineralization",
          TRUE ~ "Other"
        )
      ) %>%
      group_by(year, tech_simple) %>%
      summarize(count = n(), .groups = "drop")

    plot_ly(tech_timeline, x = ~year, y = ~count, color = ~tech_simple,
            type = 'scatter', mode = 'lines+markers',
            colors = c(chart_colors$primary, chart_colors$secondary,
                      chart_colors$accent, chart_colors$purple)) %>%
      plotly_light_layout("") %>%
      layout(xaxis = list(title = "Year", dtick = 1),
             yaxis = list(title = "New Facilities"))
  })

  # Geographic Distribution
  output$geo_distribution <- renderPlotly({
    geo_data <- dac_installations %>%
      group_by(region) %>%
      summarize(
        count = n(),
        capacity = sum(capacity, na.rm = TRUE) / 1000
      ) %>%
      arrange(desc(capacity))

    plot_ly() %>%
      add_trace(data = geo_data, x = ~region, y = ~capacity, type = 'bar',
                name = 'Capacity (kt)', marker = list(color = chart_colors$primary)) %>%
      add_trace(data = geo_data, x = ~region, y = ~count * 100, type = 'scatter',
                mode = 'lines+markers', name = 'Facilities (scaled)',
                yaxis = 'y2', line = list(color = chart_colors$accent)) %>%
      plotly_light_layout("") %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Capacity (kt/yr)"),
        yaxis2 = list(title = "Facilities", overlaying = "y", side = "right",
                     tickfont = list(color = chart_colors$accent))
      )
  })

  # ═══════════════════════════════════════════════════════════════════════════
  # PERFORMANCE TAB
  # ═══════════════════════════════════════════════════════════════════════════

  output$performance_comparison <- renderPlotly({
    perf_data <- operational_data %>%
      group_by(installation_name, date) %>%
      summarize(daily_capture = sum(co2_captured, na.rm = TRUE), .groups = "drop")

    plot_ly(perf_data, x = ~date, y = ~daily_capture, color = ~installation_name,
            type = 'scatter', mode = 'lines') %>%
      plotly_light_layout("") %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "CO₂ Captured (tonnes/day)"))
  })

  output$efficiency_trends <- renderPlotly({
    trends_data <- operational_data %>%
      group_by(date) %>%
      summarize(avg_eff = mean(efficiency, na.rm = TRUE) * 100) %>%
      mutate(rolling = zoo::rollmean(avg_eff, 7, fill = NA))

    plot_ly() %>%
      add_trace(data = trends_data, x = ~date, y = ~avg_eff,
                type = 'scatter', mode = 'lines', name = 'Daily',
                line = list(color = chart_colors$secondary, width = 1)) %>%
      add_trace(data = trends_data, x = ~date, y = ~rolling,
                type = 'scatter', mode = 'lines', name = '7-day Avg',
                line = list(color = chart_colors$primary, width = 3)) %>%
      plotly_light_layout("") %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Efficiency (%)"))
  })

  # Capacity Utilization
  output$utilization_chart <- renderPlotly({
    util_data <- operational_data %>%
      left_join(dac_installations %>% select(name, capacity),
                by = c("installation_name" = "name")) %>%
      mutate(daily_max = capacity.y / 365) %>%
      group_by(installation_name) %>%
      summarize(
        avg_capture = mean(co2_captured, na.rm = TRUE),
        daily_max = first(daily_max),
        utilization = avg_capture / daily_max * 100
      ) %>%
      arrange(desc(utilization))

    plot_ly(util_data, x = ~reorder(installation_name, utilization), y = ~utilization,
            type = 'bar',
            marker = list(color = ifelse(util_data$utilization > 80,
                                        chart_colors$primary, chart_colors$accent))) %>%
      plotly_light_layout("") %>%
      layout(xaxis = list(title = "", tickangle = 45),
             yaxis = list(title = "Utilization (%)", range = c(0, 100)))
  })

  # ═══════════════════════════════════════════════════════════════════════════
  # COST ECONOMICS TAB (NEW)
  # ═══════════════════════════════════════════════════════════════════════════

  output$lowest_cost_box <- renderValueBox({
    cost_by_facility <- operational_data %>%
      group_by(installation_name) %>%
      summarize(cost = mean(operating_cost / co2_captured, na.rm = TRUE))
    lowest <- min(cost_by_facility$cost, na.rm = TRUE)
    valueBox(
      paste0("$", round(lowest)),
      "Lowest Cost/Ton",
      icon = icon("arrow-down"),
      color = "green"
    )
  })

  output$highest_cost_box <- renderValueBox({
    cost_by_facility <- operational_data %>%
      group_by(installation_name) %>%
      summarize(cost = mean(operating_cost / co2_captured, na.rm = TRUE))
    highest <- max(cost_by_facility$cost, na.rm = TRUE)
    valueBox(
      paste0("$", round(highest)),
      "Highest Cost/Ton",
      icon = icon("arrow-up"),
      color = "yellow"
    )
  })

  output$avg_cost_trend_box <- renderValueBox({
    recent <- operational_data %>%
      filter(date > Sys.Date() - 30) %>%
      summarize(cost = mean(operating_cost / co2_captured, na.rm = TRUE)) %>%
      pull(cost)
    valueBox(
      paste0("$", round(recent)),
      "Current Avg Cost",
      icon = icon("dollar-sign"),
      color = "blue"
    )
  })

  output$cost_reduction_box <- renderValueBox({
    early <- operational_data %>%
      filter(date < Sys.Date() - 300) %>%
      summarize(cost = mean(operating_cost / co2_captured, na.rm = TRUE)) %>%
      pull(cost)
    recent <- operational_data %>%
      filter(date > Sys.Date() - 30) %>%
      summarize(cost = mean(operating_cost / co2_captured, na.rm = TRUE)) %>%
      pull(cost)
    reduction <- round((early - recent) / early * 100)
    valueBox(
      paste0(reduction, "%"),
      "Cost Reduction (YoY)",
      icon = icon("chart-line"),
      color = "teal"
    )
  })

  output$cost_comparison <- renderPlotly({
    cost_data <- operational_data %>%
      group_by(installation_name) %>%
      summarize(
        avg_cost = mean(operating_cost / co2_captured, na.rm = TRUE),
        sd_cost = sd(operating_cost / co2_captured, na.rm = TRUE)
      ) %>%
      arrange(avg_cost)

    plot_ly(cost_data, x = ~reorder(installation_name, avg_cost), y = ~avg_cost,
            type = 'bar',
            error_y = list(array = ~sd_cost, color = chart_colors$text),
            marker = list(color = chart_colors$primary)) %>%
      plotly_light_layout("") %>%
      layout(xaxis = list(title = "", tickangle = 45),
             yaxis = list(title = "Cost per Ton ($)"))
  })

  # Cost vs Scale (Learning Curve)
  output$cost_scale_curve <- renderPlotly({
    scale_data <- operational_data %>%
      left_join(dac_installations %>% select(name, capacity),
                by = c("installation_name" = "name")) %>%
      group_by(installation_name, capacity.y) %>%
      summarize(avg_cost = mean(operating_cost / co2_captured, na.rm = TRUE), .groups = "drop")

    plot_ly(scale_data, x = ~capacity.y, y = ~avg_cost, text = ~installation_name,
            type = 'scatter', mode = 'markers+text',
            textposition = 'top center',
            textfont = list(size = 9, color = chart_colors$text),
            marker = list(color = chart_colors$secondary, size = 12)) %>%
      plotly_light_layout("") %>%
      layout(xaxis = list(title = "Facility Capacity (t/yr)", type = "log"),
             yaxis = list(title = "Cost per Ton ($)"))
  })

  # Cost Trend
  output$cost_trend <- renderPlotly({
    cost_trend <- operational_data %>%
      group_by(date) %>%
      summarize(avg_cost = mean(operating_cost / co2_captured, na.rm = TRUE)) %>%
      mutate(rolling = zoo::rollmean(avg_cost, 14, fill = NA))

    plot_ly() %>%
      add_trace(data = cost_trend, x = ~date, y = ~avg_cost,
                type = 'scatter', mode = 'lines', name = 'Daily',
                line = list(color = chart_colors$secondary, width = 1, opacity = 0.5)) %>%
      add_trace(data = cost_trend, x = ~date, y = ~rolling,
                type = 'scatter', mode = 'lines', name = '14-day Avg',
                line = list(color = chart_colors$primary, width = 3)) %>%
      plotly_light_layout("") %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Cost per Ton ($)"))
  })

  # Cost Histogram
  output$cost_histogram <- renderPlotly({
    plot_ly(operational_data, x = ~operating_cost / co2_captured,
            type = 'histogram', nbinsx = 30,
            marker = list(color = chart_colors$primary,
                         line = list(color = chart_colors$bg, width = 1))) %>%
      plotly_light_layout("") %>%
      layout(xaxis = list(title = "Cost per Ton ($)"),
             yaxis = list(title = "Frequency"))
  })

  # ═══════════════════════════════════════════════════════════════════════════
  # ENVIRONMENTAL TAB
  # ═══════════════════════════════════════════════════════════════════════════

  output$total_captured_box <- renderValueBox({
    total <- sum(operational_data$co2_captured, na.rm = TRUE)
    valueBox(
      paste0(format(round(total), big.mark = ","), " t"),
      "Total CO₂ Captured",
      icon = icon("cloud"),
      color = "teal"
    )
  })

  output$energy_efficiency_box <- renderValueBox({
    avg_energy <- mean(operational_data$energy_used / operational_data$co2_captured, na.rm = TRUE)
    valueBox(
      paste0(round(avg_energy, 1), " MWh/t"),
      "Energy per Ton",
      icon = icon("bolt"),
      color = "yellow"
    )
  })

  output$cars_equivalent_box <- renderValueBox({
    total_co2 <- sum(operational_data$co2_captured, na.rm = TRUE)
    cars <- round(total_co2 / 4.6)  # avg car = 4.6 tonnes CO2/year
    valueBox(
      paste0(format(cars, big.mark = ","), " cars"),
      "Annual Emissions Offset",
      icon = icon("car"),
      color = "blue"
    )
  })

  output$trees_equivalent_box <- renderValueBox({
    total_co2 <- sum(operational_data$co2_captured, na.rm = TRUE)
    trees <- round(total_co2 / 0.022)  # avg tree absorbs 22kg/year
    valueBox(
      paste0(format(round(trees/1000), big.mark = ","), "k trees"),
      "Equivalent Absorption",
      icon = icon("tree"),
      color = "green"
    )
  })

  output$environmental_metrics <- renderPlotly({
    env_data <- operational_data %>%
      group_by(date) %>%
      summarize(
        avg_temp = mean(temperature, na.rm = TRUE),
        avg_humidity = mean(humidity, na.rm = TRUE),
        total_capture = sum(co2_captured, na.rm = TRUE)
      )

    plot_ly() %>%
      add_trace(data = env_data, x = ~date, y = ~avg_temp,
                name = "Temperature (°C)", type = 'scatter', mode = 'lines',
                line = list(color = chart_colors$accent)) %>%
      add_trace(data = env_data, x = ~date, y = ~avg_humidity,
                name = "Humidity (%)", type = 'scatter', mode = 'lines',
                yaxis = "y2", line = list(color = chart_colors$secondary)) %>%
      plotly_light_layout("") %>%
      layout(
        yaxis = list(title = "Temperature (°C)"),
        yaxis2 = list(title = "Humidity (%)", overlaying = "y", side = "right",
                     tickfont = list(color = chart_colors$secondary))
      )
  })

  output$temp_efficiency <- renderPlotly({
    sample_data <- operational_data %>% sample_n(min(1000, nrow(.)))

    plot_ly(sample_data, x = ~temperature, y = ~efficiency * 100,
            type = 'scatter', mode = 'markers',
            marker = list(color = chart_colors$primary, size = 5, opacity = 0.4)) %>%
      plotly_light_layout("") %>%
      layout(xaxis = list(title = "Temperature (°C)"),
             yaxis = list(title = "Efficiency (%)"))
  })

  output$humidity_impact <- renderPlotly({
    sample_data <- operational_data %>% sample_n(min(1000, nrow(.)))

    plot_ly(sample_data, x = ~humidity, y = ~efficiency * 100,
            type = 'scatter', mode = 'markers',
            marker = list(color = chart_colors$secondary, size = 5, opacity = 0.4)) %>%
      plotly_light_layout("") %>%
      layout(xaxis = list(title = "Humidity (%)"),
             yaxis = list(title = "Efficiency (%)"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
