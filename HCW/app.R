##Required packages
library(shiny)
library(shinymanager)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(DT)
library(sf)
library(shinyWidgets)
library(highcharter)

##Data loading
HCWs_data <- readRDS("HCWDashboard.rds")

##Authentication setup
credentials <- data.frame(
  user = c("user1", "admin"),
  password = c("pass1", "adminpass"), 
  stringsAsFactors = FALSE)

ui <- secure_app(fluidPage(
  tags$head(
    tags$style(HTML("
      /* General background */
      body { background-color: #E6F2FF; }
      
      /* Titles and text */
      .navbar, .navbar-default {
        background-color: #337AB7;
        border-color: #337AB7;
      }
      .navbar-default .navbar-nav > li > a,
      .navbar-default .navbar-brand {
        color: #FFF;
      }
      h1, h2, h3, h4, h5, h6, .shiny-text-output {
        color: #20568F;
      }
      
      /* Sidebar panel styling */
      .sidebar {
        background-color: #D9E3F0;
        border-color: #D9E3F0;
      }
      
      /* Main panel styling */
      .main-panel, .tab-content {
        background-color: #F7FBFF;
        border-color: #B0C4DE;
      }
      
      /* Input fields and select boxes */
      .form-control, .selectize-input, .selectize-control.single .selectize-input.input-active {
        border-color: #6C8EBF;
      }
      
      /* Action buttons */
      .btn-primary {
        background-color: #337AB7;
        border-color: #2E6DA4;
      }
      .btn-primary:hover, .btn-primary:active, .btn-primary.hover {
        background-color: #286090;
        border-color: #204D74;
      }
      
      /* Tabs and navigation */
      .nav-tabs > li > a {
        color: #337AB7;
      }
      .nav-tabs > li.active > a, .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover {
        color: #FFF;
        background-color: #337AB7;
        border-color: #337AB7;
      }
      
      /* Tables */
      .table > thead > tr > th {
        background-color: #D9E3F0;
        color: #20568F;
      }
    "))
  ),
  titlePanel("HCWs Vaccination in Kakamega County", windowTitle = "HCWs Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("teamInput", "Select Team", choices = NULL),  # Choices updated in server
      selectInput("subcountyInput", "Subcounty", choices = NULL),  # Choices updated in server
      uiOutput("genderDistributionBox"),  # Gender distribution as a value box
      highchartOutput("riskLevelPlot")  # Risk level visualization
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Vaccination Counts", plotlyOutput("vaccinationPlot")),
        tabPanel("Progress Tracking", highchartOutput("progressPlot")),
        tabPanel("Cadre Categorization", DTOutput("cadreTable")),
        tabPanel("Geographical Mapping", leafletOutput("map"))
      )
    )
    
    )
  )
)



server <- function(input, output, session) {

  ## Reactive value to store filtered data
  filtered_data <- reactiveVal()
  
  ## Populate team dropdown
  observe({
    updateSelectInput(session, "teamInput", choices = unique(HCWs_data$team))
  })
  
  ## Update subcountyInput based on teamInput selection
  observeEvent(input$teamInput, {
    ## Find subcounties associated with the selected team
    associated_subcounties <- HCWs_data %>%
      filter(team == input$teamInput) %>%
      .$subcounty %>%
      unique()
    
    ## Update subcountyInput choices
    updateSelectInput(session, "subcountyInput", choices = associated_subcounties)
  })
  
  ## Update filtered data based on selections or special categorization for CHMT in Lurambi
  observe({
    req(input$teamInput, input$subcountyInput)  ## Ensure inputs are available
    
    ## Filter data based on selections
    temp_filtered_data <- HCWs_data %>%
      filter(team %in% input$teamInput,
             subcounty %in% input$subcountyInput)
    
    ## Special handling for CHMT within Lurambi subcounty
    if ("CHMT" %in% input$teamInput && "Lurambi" %in% input$subcountyInput) {
      temp_filtered_data <- temp_filtered_data %>%
        mutate(subcounty = if_else(subcounty == "CHMT", "Lurambi", subcounty))
    }
    
    ## Update the reactive value
    filtered_data(temp_filtered_data)
  })
  
    ##Vaccination counts plot  
    output$vaccinationPlot <- renderPlotly({
      req(filtered_data())
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
    output$genderDistributionBox <- renderUI({
      req(filtered_data())
      data <- filtered_data()
      
      # Counts for each gender
      female_count <- sum(data$sex == "Female", na.rm = TRUE)
      male_count <- sum(data$sex == "Male", na.rm = TRUE)
      total_count <- nrow(data)
      
      # Proportions for each gender
      proportion_female <- ifelse(total_count > 0, female_count / total_count, 0)
      proportion_male <- ifelse(total_count > 0, male_count / total_count, 0)
      
      div(
        class = "value-box",
        style = "padding: 10px; margin: 10px 0; background-color: #f2f2f2; border-radius: 5px;",
        h3("Gender Distribution"),
        div(
          style = "margin-bottom: 5px;",
          tags$i(class = "fa fa-male", style = "color: #337ab7;"),  # Male icon
          span(" Male HCWs: ", strong(male_count), 
               " (", strong(sprintf("%.2f%%", proportion_male * 100)), ")")
        ),
        div(
          tags$i(class = "fa fa-female", style = "color: #d9534f;"),  # Female icon
          span(" Female HCWs: ", strong(female_count),
               " (", strong(sprintf("%.2f%%", proportion_female * 100)), ")")
        )
      )
    })
    
  
 ##Risk level visualization
    output$riskLevelPlot <- renderHighchart({
      req(filtered_data())
      data <- filtered_data() %>%
        group_by(risk_level) %>%
        summarise(count = n()) %>%
        arrange(desc(count))
      
      # Check if the data frame is empty
      if(nrow(data) == 0) {
        # Return a default Highcharter object or a message
        return(highchart() %>%
                 hc_title(text = "No data available for the selected filters"))
      }
      
      # Proceed with rendering the Highcharter widget if data is available
      hchart(data, "column", hcaes(x = risk_level, y = count)) %>%
        hc_title(text = "Risk Level Distribution") %>%
        hc_xAxis(title = list(text = "Risk Level")) %>%
        hc_yAxis(title = list(text = "Count")) %>%
        hc_plotOptions(column = list(borderColor = 'rgba(50, 171, 96, 1.0)', color = 'rgba(50, 171, 96, 0.7)')) %>%
        hc_add_theme(hc_theme_flat())
    })
    
    
  
  ##Cadre categorization
    output$cadreTable <- renderDT({
      req(filtered_data())
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
      shapefile_data <- st_read("gadm41_KEN_shp/gadm41_KEN_2.shp")
      
      ##Filter for Kakamega county
      kakamega_data <- shapefile_data %>%
        filter(NAME_1 == "Kakamega") %>%
        select(NAME_2, geometry)
      
      #omitting the CHMT subcounty by changing it to : Lurambi
      HCWs_data <- HCWs_data |>
        mutate(subcounty = ifelse(subcounty == "CHMT", "Lurambi", subcounty))
      
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
          popup = ~paste(subcounty, ": ", vaccinated_hcw_count, " vaccinated HCWs"),
          label = ~paste(subcounty, ": ", vaccinated_hcw_count, " vaccinated HCWs"),  # Hover text
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "13px",
            direction = "auto"
          )
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
    output$progressPlot <- renderHighchart({
      req(filtered_data())
      total_vaccinations <- filtered_data() %>%
        nrow()  # Count the total number of vaccinated HCWs
      target <- 7500
      progress_percentage <- min((total_vaccinations / target) * 100, 100)  # Calculate progress as a percentage
      
      highchart() %>%
        hc_chart(type = "solidgauge") %>%
        hc_title(text = "Progress Towards Vaccination Target") %>%
        hc_pane(
          startAngle = -90,
          endAngle = 90,
          background = list(
            innerRadius = '60%',
            outerRadius = '100%',
            shape = 'arc'
          )
        ) %>%
        hc_yAxis(
          min = 0,
          max = target,
          lineWidth = 0,
          tickPositions = NULL,
          title = list(text = ''),
          labels = list(enabled = FALSE)
        ) %>%
        hc_series(
          list(
            name = "Vaccinated HCWs",
            data = list(list(y = total_vaccinations, color = '#337ab7')),
            dataLabels = list(
              format = paste('<div style="text-align:center; margin-top:30px;">',  # Increase margin-top for more spacing
                             '<span style="font-size:25px;color:black">{y}</span>',
                             '<span style="font-size:12px;opacity:0.4"><br>HCWs</span><br/>',  # Added <br> for spacing
                             '<span style="font-size:20px;color:black">', round(progress_percentage, 2), '%</span>',
                             '</div>'),
              borderWidth = 0,
              y = 25  # Increase y offset to move text further below the gauge
            ),
            tooltip = list(valueSuffix = ' HCWs')
          )
        ) %>%
        hc_tooltip(
          pointFormat = '<span style="color:{point.color}">\u25CF</span> <b>{point.y}</b>/{series.userOptions.yAxis.max} HCWs'
        ) %>%
        hc_plotOptions(
          solidgauge = list(
            innerRadius = '60%',
            dataLabels = list(y = 5),
            linecap = 'round',
            stickyTracking = FALSE
          )
        ) %>%
        hc_add_theme(
          hc_theme_elementary(
            base = '#FFF',
            text = list(color = '#333')
          )
        )
    })
    
  
    ##Secure access setup
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials))
}

shinyApp(ui = ui, server = server)
