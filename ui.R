# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.

library(shiny)
library(tidyverse)
library(lubridate)
library(leaflet)
library(EDIutils)

# Define UI for data viewer application
shinyUI(fluidPage(
  
  # Application title
  # titlePanel("NTL-LTER data viewer"),
  titlePanel(title=div(img(src="https://lter.limnology.wisc.edu/sites/default/files/ntl/logos/NTL_logo_notext.png",
                           height="5%", width = "5%"),
                       "NTL-LTER data viewer")),
  
  helpText("North Temperate Lakes Long-Term Ecological Research"),
  
  
  # Sidebar with user inputs
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = 'input.lake', 
                  label = 'Choose lake', 
                  choices = list(NorthernLakes = c('All northern lakes',
                                                   'Allequash',
                                                   'Big Musky',
                                                   'Crystal',
                                                   'Crystal Bog',
                                                   'Sparkling',
                                                   'Trout',
                                                   'Trout Bog'),
                                 `Southern Lakes` = c(
                                   'All southern lakes',
                                   'Mendota',
                                   'Monona',
                                   'Fish',
                                   'Wingra'))),
      selectInput(inputId = 'input.vars', 
                  label = 'Choose variable', 
                  choices = list(Physical = c('Water Temperature (°C)',
                                              'Dissolved Oxygen (mg/L)',
                                              'Dissolved Oxygen (% sat)'),
                                 Nutrients = c(
                                   'Dissolved Organic Carbon (mg/L)',
                                   'Dissolved Inorganic Carbon (mg/L)',
                                   'Total Organic Carbon (mg/L)',
                                   'Total Inorganic Carbon (mg/L)',
                                   'Nitrate + Nitrite (µg/L)',
                                   'Ammonium (µg/L)',
                                   'Total Nitrogen unfiltered (µg/L)',
                                   'Total Nitrogen filtered (µg/L)',
                                   'Dissolved Reactive Phosphorus (µg/L)',
                                   'Total Phosphorus unfiltered (µg/L)',
                                   'Total Phosphorus filtered (µg/L)',
                                   'Dissolved Reactive Silica (µg/L)'),
                                 Ions = c(
                                   'pH',
                                   'Alkalinity (ueq/L)',
                                   'Calcium (mg/L)',
                                   'Magnesium (mg/L)',
                                   'Sodium (mg/L)',
                                   'Potassium (mg/L)',
                                   'Sulfate (mg/L)',
                                   'Chloride (mg/L)',
                                   'Specific Conductance (µS/cm)'),
                                 Secchi = c(
                                   'Secchi with viewer',
                                   'Secchi without viewer')),
                  selected = 'Water Temperature (degC)'),
      
      uiOutput("datadepths"),
      radioButtons("plottype", "Plot type:",
                   c("Time-Series" = "plot.ts",
                     "Annual Means" = "plot.am",
                     "Monthly Boxplots" = "plot.mb"),
                   selected = 'plot.ts'),
      # selectInput(inputId = 'input.depth', 
      #             label = 'Choose depth', 
      #             choices = c(0:20)),
      # checkboxInput(inputId = "scales", label = 'Free y-axis', value = FALSE),
      checkboxGroupInput(inputId = "scales", label = c('Options'), choices = c('Free y-axis', 'Log y-axis')),
      
      downloadButton(outputId = 'downloadImage', 'Download plot'),
    ),
    
    # Show the tabs 
    mainPanel(
      tabsetPanel(
        tabPanel("Data", plotOutput("distPlot")),
        tabPanel("Map", leafletOutput("myMap"))
      )
    )
    # mainPanel(
    #     plotOutput("distPlot")
    #     # textOutput("testvar"),
    #     # textOutput("testvar2"),
    # )
  ),
  # Footer
  hr(),
  # print(textOutput("urlname"))
  print(uiOutput("urlname")),
  p("This material is based upon work supported by the National Science Foundation under Cooperative Agreement #DEB-2025982, NTL LTER. Any opinions, findings, conclusions, or recommendations expressed in the material are those of the author(s) and do not necessarily reflect the views of the National Science Foundation.", 
    align="left", style = "font-size:11px; color: #751e04;"),
))
