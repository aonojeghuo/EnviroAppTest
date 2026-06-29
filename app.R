### **Part 2: The Decoupled Shiny App (`app.R`)**
#Your main `app.R` is now drastically shorter, stripped of all slow APIs and dependencies like `httr2`, `tidyhydat`, and `cffdrs`. It will read the pre-processed data instantaneously directly from the Posit Connect memory cache.

#*Note: Replace "jolexyenviro/hydromet_ab_data_v2"` in the server function with the exact pin name Posit Connect generates for you after you publish Part 1.*
  
#  ```r
library(shiny)
library(xml2)
library(httr)
library(dplyr)
library(ggplot2)
library(leaflet)
library(DT)
library(plotly)
library(lubridate)
library(pins)

#library(pins)

# --------------------------- Projection & CONFIG ----------------------------
alberta_crs <- leafletCRS(
  crsClass = "L.Proj.CRS",
  code = "EPSG:3400",
  proj4def = "+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs",
  resolutions = c(8192, 4096, 2048, 1024, 512, 256, 128, 64, 32, 16, 8, 4, 2, 1) 
)

weather_vars <- c(
  "Max Temperature (°C)" = "temp_max", 
  "Min Temperature (°C)" = "temp_min",
  "Precipitation (mm)" = "precip", 
  "Snowfall (cm)" = "snowfall",
  "Max Wind Speed (km/h)" = "wind_speed_max",
  "Alberta Rivers Flow (m³/s)" = "flow_value",
  "Fire Weather Index (ECCC GeoMet)" = "fwi_geomet",
  "Weather Alerts (ECCC GeoMet)" = "eccc_alerts" 
)

ab_places <- tibble::tribble(
  ~name, "Airdrie", "Athabasca", "Banff", "Brooks", "Calgary", "Camrose", 
  "Cardston", "Cochrane", "Cold Lake", "Cypress Hills", "Drumheller", 
  "Edmonton", "Fairview", "Fort McMurray", "Fort Saskatchewan", 
  "Grande Prairie", "High Level", "Hinton", "Jasper", "Lac La Biche", 
  "Leduc", "Lethbridge", "Medicine Hat", "Okotoks", "Olds", "Peace River", 
  "Pincher Creek", "Red Deer", "Rocky Mountain House", "Slave Lake", 
  "Spruce Grove", "St. Albert", "Stettler", "Taber", "Vegreville", 
  "Westlock", "Wainwright"
) %>% arrange(name)

# Establish connection to the server board
## board <- pins::board_connect(auth = "envvar")
# Establish connection using the custom environment variables.
# This forces the pins package to authenticate properly instead of relying on internal container logic.
board <- board_connect(
  server = Sys.getenv("PUBLIC_SERVER"),
  key = Sys.getenv("PUBLIC_KEY")
)
# --------------------------- UI ----------------------------------------------
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "yeti", primary = "#4CAF50" , secondary = "#2787b0"),
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"),
    tags$style(HTML("
      .title-header { background-color: #2787b0; color: white; padding: 15px; border-radius: 4px; }
      .btn-primary { background-color: #4CAF50 !important; border-color: #4CAF50 !important; font-weight: bold; }
      h4, h3 { color: #2787b0; font-weight: 600; }
      .retrieval-note { font-style: italic; font-size: 0.85em; color: #7f8c8d; margin-top: 5px; }
      .dataTables_wrapper .dataTables_paginate .paginate_button.current, 
      .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover { background-color: #1A5276 !important; border: 1px solid #1A5276 !important; color: white !important; }
      .dataTables_wrapper .dataTables_paginate .paginate_button:hover { background-color: #1A5276 !important; border: 1px solid #1A5276 !important; color: white !important; }
    "))
  ),
  div(class = "title-header", h2("Jolexy Environmental: Integrated Hydro-Met Dashboard for Alberta", style = "margin:0;")),
  br(),
  
  sidebarLayout(
    sidebarPanel(width = 3,
                 h4("Map & Data Controls"),
                 selectInput("variable", "Active Layer:", choices = weather_vars, selected = "precip"),
                 selectInput("basemap", "Basemap Style:", choices = c(
                   "Light Base" = "CartoDB.Positron", "Dark Base" = "CartoDB.DarkMatter", 
                   "Hybrid Satellite" = "Hybrid", "Satellite" = "Esri.WorldImagery",
                   "Street (Clear Borders)" = "Esri.WorldStreetMap", 
                   "Terrain" = "OpenTopoMap", "Topographic (Clear Borders)" = "Esri.WorldTopoMap"
                 )),
                 actionButton("refresh", "Pull Latest Update", icon = icon("sync"), class = "btn-primary w-100"),
                 hr(),
                 div(style = "font-size: 11px; color: #7f8c8d; margin-top: 10px;",
                     p(strong("Data Source Registry:")),
                     tags$ul(
                       tags$li(a("Open-Meteo Weather API", href = "https://open-meteo.com/", target = "_blank")),
                       tags$li(a("MSC GeoMet (ECCC Alerts)", href = "https://eccc-msc.github.io/open-data/", target = "_blank")),
                       tags$li(a("Alberta Rivers (Hydrometric Network)", href = "https://rivers.alberta.ca/", target = "_blank")),
                       tags$li(a("Fire Weather Index (Natural Resources Canada CWFIS)", href = "https://cwfis.cfs.nrcan.gc.ca/", target = "_blank"))
                     ),
                     br(),
                     p(strong("Dashboard Developer:")),
                     p("Built by Ajoke R. Onojeghuo (PhD)"),
                     p(strong("About Dashboard:")),
                     tags$a(href = "https://github.com/aonojeghuo/JolexyEnviroWeather_readme/blob/main/README.md", "View Dashboard Description", target = "_blank")
                 ),
                 hr(),
                 div(style = "text-align: center;",
                     a(href = "https://jolexyenviro.com/", target = "_blank",
                       tags$img(src = "jolexy_transparent.png", style = "max-width: 55%;")))
    ),
    
    mainPanel(width = 9,
              leafletOutput("map", height = "400px"),
              hr(), 
              fluidRow(
                column(8, uiOutput("detail_header")),
                column(4, tags$div(style = "height: 75px; display: flex; align-items: center;",
                                   selectInput("selected_city", label = "Community Detail:", choices = ab_places$name, selected = "Edmonton", width = "100%")))
              ),
              fluidRow(
                column(4, plotlyOutput("detail_temp", height = "220px")),
                column(4, plotlyOutput("detail_precip", height = "220px")),
                column(4, plotlyOutput("detail_wind", height = "220px"))
              )
    )
  ),
  hr(), 
  fluidRow(
    column(8, tags$div(style = "display: flex; align-items: center; height: 75px;", uiOutput("table_title_ui"))),
    column(4, tags$div(style = "display: flex; align-items: center; height: 75px;", uiOutput("date_ui"))) 
  ),
  DTOutput("summary_tbl"),
  br(),
  helpText("*Table displays 7 days of historical observations and a 3-day forecast from Open-Meteo, with weather conditions classified via WMO codes. Streamflow metrics are sourced directly from live Alberta Rivers telemetry until 6:00pm MST. Fire Weather Index is sourced from NRCan CWFIS, and ECCC alerts are sourced from the Environment and Climate Change Canada (MSC GeoMet) API."),
  br(),
  tags$div(style = "font-size: 14px;", uiOutput("retrieval_time_ui")),
  br(), br()
)

# --------------------------- Server ------------------------------------------
server <- function(input, output, session) {
  
  # Reactive read: Automatically polls the server pin for updates every hour (3600000 ms), 
  # bypassing the need for users to refresh the page.
#  fc_all <- pin_reactive_read(board, "https://019f10f9-b766-791b-6c6a-a437c567c703.share.connect.posit.cloud", interval = 3600000)
  # 
  # fc_all <- reactive({
  #   # 1. Start an internal Shiny timer that triggers every hour
  #   invalidateLater(3600000)
  #   
  #   # 2. Read the data from our public URL board
  #   pins::pin_read(board, "hydromet_ab_data_v2")
  # })
  # 
  # 
  # output$date_ui <- renderUI({
  #   req(fc_all())
  #   dates <- sort(unique(fc_all()$date))
  #   selectInput("sel_date", "Date (Historical & Forecast):", choices = as.character(dates), selected = as.character(Sys.Date()))
  # })
  # 
  
  # Custom Reactive Engine: Surgical HTTP Download
  # Standard Reactive Engine using pins package
  fc_all <- reactive({
    # 1. Start an internal Shiny timer that triggers every hour
    invalidateLater(3600000)
    
    # 2. Read the pin using the standard pins function.
    # IMPORTANT: Ensure the name exactly matches the name you used in pin_write in your ETL script!
    pins::pin_read(board, "jolexyenviro/hydromet_ab_data_v2")
  })
  
  
  # Map Render logic
  output$map <- renderLeaflet({
    df <- fc_all()
    req(nrow(df) > 0, input$sel_date, input$variable)
    plot_df <- df %>% filter(date == as.Date(input$sel_date))
    legend_label <- names(weather_vars)[weather_vars == input$variable]
    
    l <- leaflet(plot_df)
    
    if (input$basemap == "Hybrid") {
      l <- l %>% 
        addProviderTiles("Esri.WorldImagery") %>% 
        addProviderTiles("CartoDB.PositronOnlyLabels", options = providerTileOptions(opacity = 0.9))
    } else {
      l <- l %>% addProviderTiles(input$basemap)
    }
    l <- l %>% setView(lng = -114.5, lat = 54.5, zoom = 5)
    
    if (input$variable == "fwi_geomet") {
      l <- l %>% 
        addWMSTiles(
          baseUrl = "https://cwfis.cfs.nrcan.gc.ca/geoserver/public/wms",
          layers = "public:fwi_current", 
          options = WMSTileOptions(format = "image/png", transparent = TRUE, opacity = 0.6, version = "1.1.1"),
          attribution = "Data: NRCan CWFIS"
        ) %>%
        addCircleMarkers(
          lng = ~lon, lat = ~lat, layerId = ~name, radius = 4, 
          fillColor = "transparent", color = "#333", weight = 2, label = ~name
        ) %>%
        addControl(
          html = '<div style="background-color: rgba(255, 255, 255, 0.9); padding: 5px; border-radius: 5px; box-shadow: 0 0 15px rgba(0,0,0,0.2);">
                    <img src="https://cwfis.cfs.nrcan.gc.ca/geoserver/public/wms?REQUEST=GetLegendGraphic&VERSION=1.1.1&FORMAT=image/png&LAYER=public:fwi_current" alt="NRCan FWI Legend">
                  </div>',
          position = "bottomright"
        )
      return(l) 
    }
    
    if (input$variable == "eccc_alerts") {
      l <- l %>% 
        addWMSTiles(
          baseUrl = "https://geo.weather.gc.ca/geomet?",
          layers = "ALERTS", 
          options = WMSTileOptions(format = "image/png", transparent = TRUE, opacity = 0.75, version = "1.3.0"),
          attribution = "Data © Environment and Climate Change Canada"
        ) %>%
        addCircleMarkers(
          lng = ~lon, lat = ~lat, layerId = ~name, radius = 5, 
          fillColor = "#1A5276", color = "#FFFFFF", weight = 1.5, fillOpacity = 0.8, label = ~name
        ) %>%
        addControl(
          html = '<div style="background-color: rgba(255, 255, 255, 0.9); padding: 8px; border-radius: 5px; box-shadow: 0 0 15px rgba(0,0,0,0.2); font-family: Arial, sans-serif; font-size: 11px;">
                    <strong style="display:block; margin-bottom:5px;">ECCC Alert Legend</strong>
                    <img src="https://geo.weather.gc.ca/geomet?version=1.3.0&service=WMS&request=GetLegendGraphic&sld_version=1.1.0&layer=ALERTS&format=image/png" alt="ECCC Weather Alerts Legend" style="width: 150px; height: 300px;">
                  </div>',
          position = "bottomright"
        )
      return(l)
    }
    
    param_values <- plot_df[[input$variable]]
    if (all(is.na(param_values))) {
      l <- l %>% addCircleMarkers(
        lng = ~lon, lat = ~lat, layerId = ~name, radius = 9, 
        fillColor = "#bdc3c7", fillOpacity = 0.85, color = "white", weight = 2,
        label = ~name, popup = ~paste0("<b>", name, "</b><br/>", legend_label, ": Offline / No Data")
      )
    } else {
      pal_type <- switch(input$variable, 
                         "precip"="Blues", "snowfall"="Purples", "flow_value"="Blues", 
                         "wind_speed_max"="GnBu", "temp_min"="YlOrRd", "temp_max"="YlOrRd")
      pal <- colorNumeric(palette = pal_type, domain = param_values, na.color = "#bdc3c7")
      l <- l %>% addCircleMarkers(
        lng = ~lon, lat = ~lat, layerId = ~name, radius = 12, 
        fillColor = ~pal(param_values), fillOpacity = 0.85, color = "white", weight = 2,
        label = ~name, popup = ~paste0("<b>", name, "</b><br/>", legend_label, ": ", ifelse(is.na(param_values), "No Data", param_values))
      ) %>% addLegend(pal = pal, values = param_values, title = legend_label, position = "bottomright", opacity = 0.85)
    }
    return(l)
  }) 
  
  output$detail_header <- renderUI({ tagList(h3(paste0("10-Day Weather: ", input$selected_city)), br()) })
  
  output$detail_temp <- renderPlotly({
    req(fc_all(), input$selected_city)
    df_sub <- fc_all() %>% filter(name == input$selected_city) %>% mutate(is_freezing = temp_mean < 0)
    p <- ggplot(df_sub, aes(date, temp_mean)) + 
      geom_ribbon(aes(ymin = temp_min, ymax = temp_max), fill = "#FFB74D", alpha = 0.25) +
      geom_path(aes(color = is_freezing, group = 1), linewidth = 0.8) + 
      scale_color_manual(values = c("FALSE" = "#E65100", "TRUE" = "#2980b9")) +
      geom_vline(xintercept = as.numeric(Sys.Date()), linetype = "dashed", color = "red") + 
      theme_minimal() + labs(title = "10-Day Temperature Horizon", y = "°C", x = NULL) + theme(legend.position = "none")
    ggplotly(p) 
  })
  
  output$detail_precip <- renderPlotly({
    req(fc_all(), input$selected_city)
    df_sub <- fc_all() %>% filter(name == input$selected_city) %>% mutate(Window = ifelse(date >= Sys.Date(), "Forecast", "Historical"))
    p <- ggplot(df_sub, aes(date, precip, fill = Window)) + 
      geom_col(alpha = 0.85) + 
      geom_vline(xintercept = as.numeric(Sys.Date() - 0.5), linetype = "dashed", color = "red") +
      scale_fill_manual(values = c("Forecast" = "#2e98bf", "Historical" = "#1A5276")) +
      labs(title = "10-Day Precipitation Horizon", y = "mm", x = NULL) + 
      theme_minimal() + theme(legend.position = "none")
    ggplotly(p)
  })
  
  output$detail_wind <- renderPlotly({
    req(fc_all(), input$selected_city)
    df_sub <- fc_all() %>% filter(name == input$selected_city)
    p <- ggplot(df_sub, aes(date, wind_speed_max)) + 
      geom_line(color ="#66507d", linewidth = 0.8) + geom_point(color = "#66507d", size = 1.5) +
      geom_vline(xintercept = as.numeric(Sys.Date()), linetype = "dashed", color = "red") +
      labs(title = "10-Day Wind Speed Horizon", y = "km/h", x = NULL) + theme_minimal()
    ggplotly(p)
  })
  
  output$table_title_ui <- renderUI({
    req(input$sel_date)
    day_type <- ifelse(as.Date(input$sel_date) < Sys.Date(), "Community Weather Forecast on", "Community Weather Forecast Profiles for")
    h4(paste(day_type, format(as.Date(input$sel_date), "%B %d, %Y"), "*"))
  })
  
  output$summary_tbl <- renderDT({
    req(fc_all(), input$sel_date)
    alert_header <- paste0("ECCC Weather Alerts (", format(Sys.Date(), "%b %d, %Y"), ") - Click on each icon for more information.")
    
    raw_df <- fc_all() %>% 
      filter(date == as.Date(input$sel_date)) %>%
      mutate(
        alert_pill = ifelse(
          !is.na(alert_pill) & alert_pill != "", 
          paste0('<div style="cursor: pointer; display: inline-block; transition: transform 0.2s;" onclick="Shiny.setInputValue(\'alert_click\', \'', name, '\', {priority: \'event\'})" onmouseover="this.style.transform=\'scale(1.05)\'" onmouseout="this.style.transform=\'scale(1)\'">', alert_pill, '</div>'),
          alert_pill
        ),
        fwi_category = case_when(
          fwi_value <= 5  ~ "Low", fwi_value <= 10 ~ "Moderate",
          fwi_value <= 18 ~ "High", fwi_value <= 29 ~ "Very High",
          TRUE            ~ "Extreme"
        )
      ) %>%
      select(Location = name, !!alert_header := alert_pill, `Conditions` = condition, `Max Temp (°C)` = temp_max, `Min Temp (°C)` = temp_min, `Precip (mm)` = precip, `Snowfall (cm)` = snowfall, `Alberta Rivers Flow (m³/s)` = flow_value, `Fire Weather Index` = fwi_category)
    
    datatable(raw_df, escape = FALSE, rownames = FALSE, options = list(pageLength = 10, dom = 'itp', autoWidth = TRUE)) %>%
      formatStyle('Fire Weather Index', backgroundColor = styleEqual(c("Low", "Moderate", "High", "Very High", "Extreme"), c('#1F78B4', '#FFFF99', '#FDBF6F', '#FF7F00', '#E31A1C')), color = styleEqual(c("Low", "Moderate", "High", "Very High", "Extreme"), c('white', 'black', 'black', 'black', 'white')), fontWeight = 'bold')
  })
  
  output$retrieval_time_ui <- renderText({
    req(fc_all())
    # Reads the ingestion timestamp saved by the ETL script
    ingest_time <- fc_all()$ingest_time[1]
    if(is.null(ingest_time)) return("Data last refreshed: Unknown")
    local_time <- with_tz(ingest_time, tzone = "America/Edmonton")
    paste("Background Data last refreshed:", format(local_time, "%Y-%m-%d %H:%M:%S %Z"))
  })
  
  # Modal Logic
  observeEvent(input$alert_click, {
    clicked_location <- input$alert_click
    location_data <- fc_all() %>% filter(name == clicked_location, date == as.Date(input$sel_date)) %>% slice(1)
    
    has_active_alert <- nrow(location_data) > 0 && !is.na(location_data$eccc_status) && !(location_data$eccc_status %in% c("None", "No Active Alerts", ""))
    
    if (has_active_alert && "descrip_en" %in% names(location_data) && !is.na(location_data$descrip_en)) {
      alert_headline  <- location_data$headline
      alert_details <- paste0("**Local Bulletin for ", clicked_location, " and surrounding area:**\n\n", location_data$descrip_en)
      alert_status    <- toupper(location_data$status)
      alert_effective <- tryCatch(format(as.POSIXct(location_data$effective), "%B %d, %Y at %I:%M %p"), error = function(e) "Immediate")
      alert_expires   <- tryCatch(format(as.POSIXct(location_data$expires), "%B %d, %Y at %I:%M %p"), error = function(e) "End of Horizon")
      
      alert_check <- tolower(paste(alert_headline, location_data$eccc_status))
      bg_color     <- "#fff5f5"; border_color <- "#e74c3c"; title_color  <- "#dc3545"
      if (grepl("orange", alert_check)) { bg_color <- "#fff7ed"; border_color <- "#e67e22"; title_color <- "#d35400"
      } else if (grepl("yellow", alert_check) || grepl("watch", alert_check)) { bg_color <- "#fef9e7"; border_color <- "#f1c40f"; title_color <- "#b7950b"
      } else if (grepl("info", alert_check) || grepl("statement", alert_check) || grepl("advisory", alert_check)) { bg_color <- "#ebf5fb"; border_color <- "#2980b9"; title_color <- "#1f618d" }
      
      status_color <- if (tolower(location_data$status) == "active") border_color else "#777777"
      
      showModal(modalDialog(
        title = tags$div(style = "display: flex; align-items: center; gap: 10px; width: 100%;", tags$strong("Environment Canada Weather Alert", style = paste0("color: ", title_color, "; font-size: 1.3rem;")), tags$span(alert_status, style = paste0("background-color:", status_color, "; color: white; padding: 3px 8px; font-size: 0.8rem; border-radius: 4px; font-weight: bold;"))),
        tags$div(style = "font-family: Arial, sans-serif; line-height: 1.6; color: #333333; padding: 5px;", tags$h4(tags$strong(alert_headline), style = paste0("color: ", title_color, "; text-transform: capitalize; margin-top: 0; margin-bottom: 15px; border-bottom: 2px solid #ecf0f1; padding-bottom: 8px;")), tags$div(style = paste0("background-color: ", bg_color, "; border-left: 4px solid ", border_color, "; padding: 15px; border-radius: 0 4px 4px 0; font-size: 1.05rem; white-space: pre-line; margin-bottom: 20px; box-shadow: inset 0 1px 2px rgba(0,0,0,0.03);"), alert_details), tags$table(style = "width: 100%; border-collapse: collapse; font-size: 0.9rem; background-color: #f9f9f9; border-radius: 4px;", tags$tr(tags$td(tags$strong("Issued/Effective:"), style = "padding: 10px; border-bottom: 1px solid #eeeeee; width: 30%; color: #555555;"), tags$td(alert_effective, style = "padding: 10px; border-bottom: 1px solid #eeeeee;")), tags$tr(tags$td(tags$strong("Anticipated Expiry:"), style = "padding: 10px; color: #555555;"), tags$td(alert_expires, style = "padding: 10px;")))),
        size = "l", easyClose = TRUE, fade = TRUE, footer = modalButton("Close Window")
      ))
    } else {
      showModal(modalDialog(
        title = tags$strong(paste("Weather Status:", clicked_location), style = "color: #2787b0;"),
        tags$div(style = "text-align: center; padding: 20px; font-family: Arial, sans-serif;", tags$div(style = "font-size: 3rem; color: #2ecc71; margin-bottom: 10px;", HTML("&#10004;")), tags$h4(tags$strong("No Active Weather Alerts"), style = "color: #2c3e50; margin-bottom: 10px;"), tags$p(paste("Atmospheric conditions for", clicked_location, "are stable. Environment and Climate Change Canada has issued no watches, warnings, or special statements for this community on the selected date."), style = "color: #7f8c8d; font-size: 1.05rem;")),
        size = "m", easyClose = TRUE, footer = modalButton("Close")
      ))
    }
  })
}

shinyApp(ui, server)