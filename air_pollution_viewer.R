library(shiny)
library(shinythemes)
library(dplyr)
library(leaflet)
library(DT)
library(data.table)


data <- fread("gap.csv")

# cleaning data
colnames(data) <- gsub(" ", "_", colnames(data))
head(data)

if (!"Latitude" %in% names(data)) data$Latitude <- runif(nrow(data), -90, 90)
if (!"Longitude" %in% names(data)) data$Longitude <- runif(nrow(data), -180, 180)



ui <- fluidPage(theme = shinytheme("cerulean"),
                
                navbarPage(
                  "Global Air Pollution Viewer",
                  
                  ############FIRST TAB#############
                  tabPanel(
                    "Pollution Data",
                    titlePanel("Explore Air Pollution Data"),
                    sidebarLayout(
                      sidebarPanel(
                        
                        # country and city selection
                        selectizeInput(
                          "select_country", 
                          "Select Country:", 
                          choices = unique(data$Country), 
                          options = list(placeholder = "Type to search country")
                        ),
                        selectizeInput(
                          "select_city", 
                          "Select City:", 
                          choices = NULL, 
                          multiple = TRUE, 
                          options = list(placeholder = "Type to search city", maxOptions = 500)
                        ),
                        checkboxInput("select_all_cities", "Select All Cities", value = FALSE),
                        checkboxInput("mean_values", "Show Mean Values for Country", value = FALSE)
                      ),
                      mainPanel(
                        
                        # data visualization
                        leafletOutput("map", height = 600),
                        h4("Pollution Data"),
                        DTOutput("pollution_table")
                      )
                    )
                  ),
                  
                  ############SECOND TAB#############
                  tabPanel(
                    "Histograms",
                    titlePanel("Air Quality Pollutants"),
                    p(HTML("Choose a pollutant and adjust the number of bins to visualize the data. Below are the descriptions of the pollutants.")),
                    hr(),
                    
                    # pollutant descriptions
                    p(HTML("Air pollutant's description with histogram visualizing <br>
          <ul>
          <li> <b>NO2(Nitrogen Dioxide:</b> A reddish-brown gas from vehicles and factories. It can harm the lungs and cause smog and acid rain. </li>
          <li> <b>O3(Ozone)</b>: A molecule made up of three oxygen atoms, formed when pollutants mix with sunlight. It can make breathing harder. </li>
          <li> <b>Carbon Monoxide(CO)</b>: A colorless, odorless gas from burning fuels. High levels can be deadly; low levels can make tired or sick. </li>
          <li> <b>PM2.5(Particular Matter)</b>: Tiny particles or droplets in the air from smoke, dust and pollution. They can get into lungs and cause health problems. </li>")),
                    hr(),
                    
                    
                    fluidRow(
                      column(
                        width = 6,
                        
                        # pollutant selection
                        radioButtons("pollutant", label = h4("Select Pollutant"),
                                     choices = c("All pollutants" = "Overall AQI", "NO2", "O3", "CO", "PM2.5"),
                                     selected = "Overall AQI")
                      ),
                      
                      column(
                        width = 6,
                        selectizeInput("select_country_hist", label = h4("Select Country:"),
                                       choices = unique(data$Country), 
                                       options = list(placeholder = "Select country"))
                      )
                    ),
                    
                    hr(),
                    
                    # bins
                    sidebarLayout(
                      sidebarPanel(
                        sliderInput(inputId = "bins_hist",
                                    label = "Number of bins:",
                                    min = 1,
                                    max = 50,
                                    value = 30),
                        
                        actionButton("save_btn_hist", "Save Histogram"),
                        hr(),
                        h4("Saved Histograms"),
                        uiOutput("saved_plots_hist"),
                      ),
                      
                      
                      mainPanel( 
                        plotOutput(outputId = "distPlot"))
                    )
                  ),
                  
                  ############THIRD TAB#############
                  tabPanel(
                    "Regression Model",
                    titlePanel("Air Pollution Regression Analysis"),
                    
                    fluidRow(
                      column(
                        width = 4,
                        selectizeInput("select_country", label = h4("Select Country"),
                                       choices = unique(data$Country),
                                       selected = "Kazakhstan",
                                       options = list(
                                         placeholder = "Choose a Country"
                                       ))
                      ),
                      
                      # instructions section
                        column(
                        width = 12,
                        h3("Welcome to the Air Pollution Regression Analysis App!"),
                        p("This app allows you to run regression models to analyze the relationship between AQI and various pollutants in different countries."),
                        p("Click 'Run Regression' to see the model summary and diagnostic plot."),
                        
                      ),
                      
                      # target variable
                      column(
                        width = 4,
                        selectizeInput("select_regression_target", label = h4("Select Target Variable"),
                                       choices = c("AQI_Value", "CO_AQI_Value", "NO2_AQI_Value", "PM2.5_AQI_Value", "Ozone_AQI_Value"),
                                       selected = "AQI_Value",
                                       options = list(
                                         placeholder = "Choose Target Variable"
                                       ))
                      ),
                      
                      # predictor
                      column(
                        width = 4,
                        selectizeInput("select_regression_predictors", label = h4("Select Predictors (multiple allowed)"),
                                       choices = c("CO_AQI_Value", "NO2_AQI_Value", "PM2.5_AQI_Value", "Ozone_AQI_Value"),
                                       selected = c("CO_AQI_Value", "NO2_AQI_Value"),
                                       multiple = TRUE,
                                       options = list(
                                         placeholder = "Choose Predictors",
                                         plugins = list("remove_button")  # adding delete "X" to tags
                                       ))
                      )
                    ),
                    
                    # action button
                    fluidRow(
                      column(
                        width = 12,
                        actionButton("run_regression", "Run Regression", class = "btn-primary", style = "width: 100%; font-size: 18px;")
                      )
                    ),
                    
                    # results 
                    fluidRow(
                      column(
                        width = 6,
                        wellPanel(
                          h4("Regression Summary"),
                          verbatimTextOutput("regression_summary")
                        )
                      ),
                      
                      column(
                        width = 6,
                        wellPanel(
                          h4("Regression Diagnostics"),
                          plotOutput("regression_plot")
                        )
                      )
                    )
                  )
                ))


#######################################################################################################
server <- function(input, output, session) {

  ############FIRST TAB#############
  # updating cities based on selected country
  observeEvent(input$select_country, {
    selected_country <- input$select_country
    cities <- unique(data[data$Country == selected_country, City])
    
    if (length(cities) == 0) {
      updateSelectizeInput(session, "select_city", choices = NULL, server = TRUE)
      showModal(modalDialog("No information available for the selected country."))
    } else {
      updateSelectizeInput(session, "select_city", 
                           choices = c("Select All" = "all", cities), 
                           server = TRUE)
    }
  })
  
  # automatically selecting all cities if "Select All" is choosed
  observeEvent(input$select_all_cities, {
    if (input$select_all_cities && input$select_country != "") {
      cities <- unique(data[data$Country == input$select_country, City])
      updateSelectizeInput(session, "select_city", selected = cities)
    }
  })
  
  # filtering data based on selection
  selected_data <- reactive({
    req(input$select_country)
    
    if (input$mean_values) {
      
      # calculating mean values
      mean_data <- data[data$Country == input$select_country, 
                        lapply(.SD, mean, na.rm = TRUE), 
                        .SDcols = grep("Value", names(data), value = TRUE)]
      mean_data <- cbind(City = "Country Mean", mean_data)
      return(mean_data)
    } else {
      
      # filtering data by selected cities
      cities <- input$select_city
      if (is.null(cities) || "all" %in% cities) {
        cities <- unique(data[data$Country == input$select_country, City])
      }
      filtered_data <- data[data$Country == input$select_country & City %in% cities, ]
      
      if (nrow(filtered_data) == 0) {
        showModal(modalDialog("No information available for the selected city."))
      }
      
      return(filtered_data)
    }
  })
  

  output$map <- renderLeaflet({
    leaflet(data) %>% 
      addTiles() %>% 
      setView(lng = 0, lat = 20, zoom = 2)
  })
  

  observe({
    req(input$select_country)
    country_data <- data[data$NormalizedCountry == input$select_country, ]
    
    leafletProxy("map") %>% 
      clearMarkers() %>% 
      addCircleMarkers(data = country_data, 
                       lng = ~Longitude, lat = ~Latitude, 
                       popup = ~paste0("<b>City:</b> ", City, 
                                       "<br><b>AQI_Value:</b> ", AQI_Value),
                       radius = 5, color = "blue", fillOpacity = 0.7) %>% 
      setView(lng = mean(country_data$Longitude, na.rm = TRUE), 
              lat = mean(country_data$Latitude, na.rm = TRUE), 
              zoom = 5)
    
    if (!is.null(input$select_city) && !("all" %in% input$select_city)) {
      city_data <- data[data$City %in% input$select_city, ]
      if (nrow(city_data) > 0) {
        leafletProxy("map") %>% 
          addCircleMarkers(data = city_data, 
                           lng = ~Longitude, lat = ~Latitude, 
                           color = "red", radius = 6, fillOpacity = 0.9)
      }
    }
  })
  
  # showing detailed information of selected city/country
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    clicked_city <- data[Longitude == click$lng & Latitude == click$lat, ]
    
    if (nrow(clicked_city) > 0) {
      showModal(modalDialog(
        title = paste("Information for", clicked_city$City[1]),
        paste("Country:", clicked_city$Country[1]),
        paste("AQI_Value:", clicked_city$AQI_Value[1]),
        paste("CO_AQI_Value:", clicked_city$CO_AQI_Value[1]),
        paste("NO2_AQI_Value:", clicked_city$NO2_AQI_Value[1]),
        paste("PM2.5_AQI_Value:", clicked_city$PM2.5_AQI_Value[1]),
        paste("Ozone_AQI_Value:", clicked_city$Ozone_AQI_Value[1])
      ))
    }
  })
  

  output$pollution_table <- renderDT({
    req(selected_data())
    datatable(selected_data(), options = list(pageLength = 10, dom = "tp"))
  })
  
  
  
  ############SECOND TAB#############
  ##modifications##
  selected_data_hist <- reactive({
    req(input$select_country_hist)
    selected_country <- input$select_country_hist
    pollutant_column <- input$pollutant
    
    filtered_data <- data[data$Country == selected_country, ]
    
    if (pollutant_column %in% names(filtered_data)) {
      return(filtered_data[[pollutant_column]])
    } else {
      return(NULL)
    }
  })
##
  
  # creating histograms
  output$distPlot <- renderPlot({

    pollutant_data <- selected_data_hist()
    
    # removing non-finite values
    pollutant_data <- pollutant_data[is.finite(pollutant_data)]
    
    if (length(pollutant_data) == 0) {
      showModal(modalDialog("No valid data available for the selected pollutant."))
      return(NULL)
    }
    
    # create histogram bins
    bins <- seq(min(pollutant_data), max(pollutant_data), length.out = input$bins_hist + 1)
    

    hist(pollutant_data, breaks = bins, col = "skyblue", 
         xlab = input$pollutant, main = paste("Histogram of", input$pollutant))
  })
  

  # initializing saved_histograms 
  saved_histograms <- reactiveVal({
    if (file.exists("saved_histograms.rds")) {
      readRDS("saved_histograms.rds")
    } else {
      list()  # returning empty list if there are no saved histograms
    }
  })
  
  # saving histogram
  observeEvent(input$save_btn_hist, {
    showModal(modalDialog(
      title = "Save Histogram",
      textInput("histogram_name_hist", "Enter a name for the histogram", value = "Histogram name"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_save_hist", "Save")
      )
    ))
  })
  

  observeEvent(input$confirm_save_hist, {
    removeModal()  
    
    current_hist <- list(
      name = input$histogram_name_hist,
      pollutant = input$pollutant,
      bins = input$bins_hist
    )
    
    hist_list <- saved_histograms()
    hist_list[[length(hist_list) + 1]] <- current_hist
    saved_histograms(hist_list)
    
    
    saveRDS(hist_list, "saved_histograms.rds")
  })
  
  
  
  # creating buttons for saved histograms
  output$saved_plots_hist <- renderUI({
    hist_list <- saved_histograms()
    if (length(hist_list) == 0) {
      return(h4("No saved histograms."))
    }
    
    buttons <- lapply(1:length(hist_list), function(i) {
      hist_data <- hist_list[[i]]
      actionButton(paste0("load_hist_", i), hist_data$name)
    })
    
    do.call(tagList, buttons)
  })
  
  # displaying saved histogram
  observe({
    hist_list <- saved_histograms()
    lapply(1:length(hist_list), function(i) {
      observeEvent(input[[paste0("load_hist_", i)]], {
        hist_data <- hist_list[[i]]
        
        updateRadioButtons(session, "pollutant", selected = hist_data$pollutant)
        updateSliderInput(session, "bins_hist", value = hist_data$bins)
      })
    })
  })
  
  
  ############THIRD TAB#############

  observeEvent(input$run_regression, {
    
    # making sure the country is selected
    if (input$select_country == "") {
      showNotification("Please select a country.", type = "error")
      return()
    }
    
    # filtering a cheching data based on selected country
    filtered_data <- subset(data, Country == input$select_country)
    
  
    if (nrow(filtered_data) == 0) {
      showNotification("No data available for the selected country.", type = "error")
      return()
    }
    
    target_var <- input$select_regression_target
    predictors <- input$select_regression_predictors
    
    # formula for the regression model
    formula_str <- paste(target_var, "~", paste(predictors, collapse = " + "))
    formula <- as.formula(formula_str)
    
 
    model <- lm(formula, data = filtered_data)
    
  
    output$regression_summary <- renderPrint({
      summary(model)
    })
    
    # plotting
    output$regression_plot <- renderPlot({
      
      par(mfrow = c(2, 2))  # arranging multiple plots
      
    
      plot(model, main = "Regression Diagnostics", col = "darkblue", pch = 16)
    })
  })
  
  
  
}
shinyApp(ui, server)



