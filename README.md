# project_IWV
# Global Air Pollution Viewer

This is a Shiny app that allows users to explore global air pollution data, including visualizations of air quality indices (AQI) and pollutants like NO2, Ozone, CO, and PM2.5. 

## Features

- Interactive map to explore air pollution data by country and city.
- View pollution data in table format.
- Visualize pollutant distributions with histograms.
- Run regression models for air quality prediction.

## Requirements

- R 4.4.2
- Required R packages:
  - shiny
  - shinythemes
  - dplyr
  - leaflet
  - DT
  - data.table

## Installation

To run this app locally, clone this repository and run the following R code:

```R
# Install required packages
install.packages(c("shiny", "shinythemes", "dplyr", "leaflet", "DT", "data.table"))

# Run the app
shiny::runApp("path_to_your_shiny_app")
