final_datar <- read.csv("C:/Users/Heni/Downloads/R TEST/final_data2.csv")
library(shiny)
library(dplyr)
library(ggplot2)
library(leaflet)
library(sf)
library(stringr)
new_data <- read.csv("C:/Users/Heni/Downloads/R TEST/new_data.csv")
final_data1 <- final_datar

# Define UI
ui <- fluidPage(
  titlePanel("Ridership Comparison"),
  
  sidebarLayout(
    sidebarPanel(
      dateInput("start_date_ref", "Select Reference Start Date:", value = as.Date("2017-01-01")),
      dateInput("end_date_ref", "Select Reference End Date:", value = as.Date("2017-01-03")),
      dateInput("start_date_comp", "Select Comparison Start Date:", value = as.Date("2017-01-07")),
      dateInput("end_date_comp", "Select Comparison End Date:", value = as.Date("2017-01-10")),
      leafletOutput("map_station"),
      br()
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Ridership Comparison", 
                 plotOutput("ridership_comparison"),
                 tableOutput("station_stats")),
        tabPanel("Station Statistics", 
                 selectInput("station_name", "Select Station:", choices = unique(final_data1$LIBELLE_ARRET), selected = "PERNETY"),
                 tableOutput("station_stats_tab")),
        tabPanel("Holiday Statistics",
                 selectInput("holiday_type", "Select Holiday Type:", choices = unique(final_data1$HolidayType), selected = "Public"),
                 tableOutput("holiday_stats_tab"))
      )
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Filter data based on user inputs
  filtered_data_ref <- reactive({
    final_data1 %>%
      filter(JOUR >= input$start_date_ref, JOUR <= input$end_date_ref)
  })
  
  filtered_data_comp <- reactive({
    final_data1 %>%
      filter(JOUR >= input$start_date_comp, JOUR <= input$end_date_comp)
  })
  
  # Update ridership comparison plot based on filtered data
  output$ridership_comparison <- renderPlot({
    ggplot() +
      geom_line(data = filtered_data_ref(), aes(x = JOUR, y = Sum_NB_VALD, color = Weekday), linetype = "dashed") +
      geom_line(data = filtered_data_comp(), aes(x = JOUR, y = Sum_NB_VALD, color = Weekday), linetype = "dashed") +
      labs(title = "Ridership Comparison",
           x = "Date",
           y = "Total Validations",
           color = "Day of the Week") +
      theme_minimal()
  })
  
  filtered_new_data <- new_data %>%
    filter(ZdCId %in% final_data1$ID_REFA_LDA) 
  
  print(filtered_new_data)
  final_data_sf <- st_as_sf(filtered_new_data, coords = c("ZdAXEpsg2154", "ZdAYEpsg2154"), crs = 2154)
  final_data_sf <- st_transform(final_data_sf, 4326)  # Transform to EPSG 4326
  
  filtered_new_data$X <- st_coordinates(final_data_sf)[, "X"]
  filtered_new_data$Y <- st_coordinates(final_data_sf)[, "Y"]
  
  # Create a leaflet map for station selection
  output$map_station <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = mean(st_coordinates(final_data_sf$geometry)[,"X"]), lat = mean(st_coordinates(final_data_sf$geometry)[,"Y"]), zoom = 11) %>%
      addMarkers(data = final_data_sf, lng = st_coordinates(final_data_sf$geometry)[,"X"], lat = st_coordinates(final_data_sf$geometry)[,"Y"])
  })
  # Observe the selection on the map and update station statistics
  observeEvent(input$map_station_marker_click, {
    click <- input$map_station_marker_click
    if (!is.null(click)) {
      lat <- click$lat
      lng <- click$lng
      print(lat)
      print(lng)
      print("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
      print(str_sub(as.character(lng), end = 7))
      id = filtered_new_data %>% filter(str_detect(as.character(X), str_sub(as.character(lng), end = 7))) %>% filter(str_detect(as.character(Y), str_sub(as.character(lat), end = 7))) %>% select(ZdCId)
      print(id)
      # Filter final_data1 based on the clicked coordinates
      selected_station <- final_data1 %>% ungroup()%>%
        filter(ID_REFA_LDA==id$ZdCId[1]) %>%
        select(LIBELLE_ARRET) %>% distinct()
      print(selected_station)
      output$station_stats <- renderTable({
        summarise_data <- final_data1 %>%
          filter(LIBELLE_ARRET %in% selected_station$LIBELLE_ARRET) %>%
          group_by(LIBELLE_ARRET, Weekday) %>%
          summarise(
            Total_Validations = sum(Sum_NB_VALD, na.rm = TRUE),
            Avg_Validations = mean(Sum_NB_VALD, na.rm = TRUE),
            Max_Validations = max(Sum_NB_VALD, na.rm = TRUE)
          )
        return(summarise_data)
      })
    }
  })
  
  # Add server logic for the "Station Statistics" tab
  observeEvent(input$station_name, {
    selected_station <- final_data1 %>%
      filter(LIBELLE_ARRET == input$station_name) %>%
      select(LIBELLE_ARRET) %>% 
      distinct()
    
    output$station_stats_tab <- renderTable({
      summarise_data <- final_data1 %>%
        filter(LIBELLE_ARRET %in% selected_station$LIBELLE_ARRET) %>%
        group_by(LIBELLE_ARRET, Weekday) %>%
        summarise(
          Total_Validations = sum(Sum_NB_VALD, na.rm = TRUE),
          Avg_Validations = mean(Sum_NB_VALD, na.rm = TRUE),
          Max_Validations = max(Sum_NB_VALD, na.rm = TRUE)
        )
      return(summarise_data)
    })
  })
  
  # Add server logic for the "Holiday Statistics" tab
  observeEvent(input$holiday_type, {
    output$holiday_stats_tab <- renderTable({
      summarise_data <- final_data1 %>%
        filter(HolidayType == input$holiday_type) %>%
        group_by(LIBELLE_ARRET, Weekday) %>%
        summarise(
          Total_Validations = sum(Sum_NB_VALD, na.rm = TRUE),
          Avg_Validations = mean(Sum_NB_VALD, na.rm = TRUE),
          Max_Validations = max(Sum_NB_VALD, na.rm = TRUE)
        )
      return(summarise_data)
    })
  })
}

# Run the Shiny app
shinyApp(ui, server)
