##Required packages
library(shiny)
library(shinymanager)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(DT)
library(sf)

##Data loading
HCWs_data <- readRDS("~/WSU_dashboard/HCWDashboard.rds")

##Authentication setup
credentials <- data.frame(
  user = c("user1", "admin"),
  password = c("pass1", "adminpass"), 
  stringsAsFactors = FALSE)

##User Interface Design 
ui <- secure_app(fluidPage(
  titlePanel("HCWs Vaccination in Kakamega County"),
  sidebarLayout(
    sidebarPanel(
      selectInput("teamInput", "Select Team", choices = NULL),  ##Update choices dynamically in server
      selectInput("subcountyInput", "Select Subcounty", choices = NULL),
      actionButton("update", "Update")),
    mainPanel(
      tabsetPanel(
        tabPanel("Vaccination Counts", plotlyOutput("vaccinationPlot")),
        tabPanel("Gender Distribution", uiOutput("femaleHCWBox")),
        tabPanel("Risk Level", plotlyOutput("riskLevelPlot")),
        tabPanel("Cadre Categorization", DTOutput("cadreTable")),
        tabPanel("Geographical Mapping", leafletOutput("map")),
        tabPanel("Progress Tracking", plotlyOutput("progressPlot")))))))

server <- function(input, output, session) {

    ##Reactive values to store filtered data
    filtered_data <- reactiveVal()
    
    ##Populate team and subcounty dropdowns
    observe({
      updateSelectInput(session, "teamInput", choices = unique(HCWs_data$team))
      updateSelectInput(session, "subcountyInput", choices = unique(HCWs_data$subcounty))
    })
    
    ##Update subcountyInput when teamInput is selected
    observeEvent(input$teamInput, {
      ##The assumption l made is each team works in only one subcounty, 
      selected_subcounty <- HCWs_data %>%
        filter(team == input$teamInput) %>%
        .$subcounty %>%
        unique()
      
      ##Update subcountyInput to the subcounty corresponding to the selected team
      updateSelectInput(session, "subcountyInput", selected = selected_subcounty)})
    
    ##Update filtered data based on selections
    observeEvent(input$update, {
      filtered_data(HCWs_data %>%
                      filter(team %in% input$teamInput,
                             subcounty %in% input$subcountyInput))})
    
    ##Vaccination counts plot  
    output$vaccinationPlot <- renderPlotly({
      data <- filtered_data() %>%
        arrange(date) %>%
        group_by(date) %>%
        summarise(daily_count = n()) %>%
        mutate(cumulative_count = cumsum(daily_count))
      
      ##interactive plots
      p <- plot_ly(data, x = ~date) %>%
        add_lines(y = ~daily_count, name = 'Daily Count', line = list(color = 'blue')) %>%
        add_lines(y = ~cumulative_count, name = 'Cumulative Count', line = list(color = 'red')) %>%
        layout(
          title = "Daily and Cumulative Vaccination Counts",
          xaxis = list(title = "Date"),
          yaxis = list(title = "Count"),
          legend = list(orientation = "h", x = 0.5, y = -0.2, xanchor = 'center'),  
          margin = list(b = 100))
      p  
    })
  
  ##Gender distribution
    output$femaleHCWBox <- renderUI({
      data <- filtered_data()
      female_count <- sum(data$sex == "Female", na.rm = TRUE)
      total_count <- nrow(data)
      proportion_female <- ifelse(total_count > 0, female_count / total_count, 0)
      proportion_text <- paste0("Proportion: ", scales::percent(proportion_female))
      
      div(
        style = "padding: 20px; background-color: pink; color: white; border-radius: 5px; display: inline-block;",
        tags$i(class = "fa fa-female", style = "font-size: 48px;"),  # Female icon
        tags$h3(style = "margin: 10px 0;", female_count, " Female HCWs"),
        tags$p(style = "margin: 0;", proportion_text)
      )})
  
  ##Risk level visualization
    output$riskLevelPlot <- renderPlotly({
      data <- filtered_data() %>%
        group_by(risk_level) %>%
        summarise(count = n())
      
      plot_ly(data, labels = ~risk_level, values = ~count, type = 'pie', hole = 0.4,
              textinfo = 'label+percent', insidetextorientation = 'radial') %>%
        layout(title = 'Risk Level Distribution Among HCWs')
    })
  
  ##Cadre categorization
    output$cadreTable <- renderDT({
      data <- filtered_data() %>%
        group_by(cadre) %>%
        summarise(Count = n()) %>%
        mutate(Percentage = Count / sum(Count) * 100) %>%
        arrange(desc(Count)) %>%
        mutate(Percentage = sprintf("%.2f%%", Percentage))  
      
      ##Render the table with DT
      datatable(data, 
                options = list(pageLength = 10, autoWidth = TRUE),
                rownames = FALSE) %>%
        formatStyle('Percentage', textAlign = 'right')})
  
  ##Geographical mapping
    output$map <- renderLeaflet({
      
      ##Shape file
      shapefile_data <- st_read("~/WSU_dashboard/gadm41_KEN_shp/gadm41_KEN_3.shp")
      
      ##Filter for Kakamega county
      kakamega_data <- shapefile_data %>%
        filter(NAME_1 == "Kakamega") %>% 
        select(NAME_2, geometry) 
      
      ##Merging HCWs data with Kakamega shapefile data
      merged_subcounty_data <- left_join(HCWs_data, kakamega_data, by = c("subcounty" = "NAME_2"))
      
      hcw_counts_by_subcounty <- merged_subcounty_data %>%
        group_by(subcounty) %>%
        summarise(vaccinated_hcw_count = n())  # Count the number of rows (HCWs) per subcounty
      
      mapped_data <- merged_subcounty_data %>%
        distinct(subcounty, .keep_all = TRUE) %>%
        left_join(hcw_counts_by_subcounty, by = "subcounty")
      
      ##Convert mapped_data to an sf object
      mapped_data <- st_as_sf(mapped_data, sf_column_name = "geometry") 
      
      ##Use the pre-processed and merged sf object for the leaflet map
      leaflet(data = mapped_data) %>%
        addTiles() %>%
        addPolygons(
          fillColor = ~colorQuantile("YlOrRd", vaccinated_hcw_count)(vaccinated_hcw_count),
          color = "#BDBDC3",
          weight = 1,
          fillOpacity = 0.7,
          popup = ~paste(subcounty, ": ", vaccinated_hcw_count, " vaccinated HCWs")
        ) %>%
        addLegend(
          position = "bottomright",
          pal = colorQuantile("YlOrRd", mapped_data$vaccinated_hcw_count),
          values = ~vaccinated_hcw_count,
          title = "HCWs Vaccinated",
          opacity = 1
        )
    })
    
  ##Progress tracking
  output$progressPlot <- renderPlotly({
    total_vaccinations <- filtered_data() %>%
      nrow()  # Count the total number of vaccinated HCWs
    target <- 7500
    progress_percentage <- min((total_vaccinations / target) * 100, 100)  # Calculate progress as a percentage
    
    plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = total_vaccinations,
      title = list(text = "Progress Towards Vaccination Target"),
      type = "indicator",
      mode = "gauge+number",
      gauge = list(
        axis = list(range = list(0, target), tickwidth = 1, tickcolor = "darkblue"),
        bar = list(color = "darkblue"),
        steps = list(
          list(range = c(0, target * 0.5), color = "lightgray"),
          list(range = c(target * 0.5, target * 0.75), color = "gray"),
          list(range = c(target * 0.75, target), color = "darkgray")
        ),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = target
        )
      ),
      number = list(
        valueformat = ",",
        suffix = " HCWs",
        font = list(size = 20)
      )
    ) %>%
      layout(
        margin = list(t = 50, r = 25, l = 25, b = 25),
        annotations = list(
          list(
            x = 0.5,
            y = 0.2,
            text = paste(round(progress_percentage, 2), "% of target"),
            showarrow = FALSE,
            font = list(size = 16))))})
  
    ##Secure access setup
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials))
}

shinyApp(ui = ui, server = server)
