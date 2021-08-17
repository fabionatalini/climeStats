library(shiny)
library(plotly)

source("www/helpers.R")

shinyUI(fluidPage(
    
    # Application title
    titlePanel("climeStats"),
    
    # Layout
    sidebarLayout(
        # Inputs
        sidebarPanel(
            fileInput("file_preci","Upload precipitation dataset"),
            fileInput("file_tempe","Upload temperature dataset"),
            br(),
            p("Created by ", a(href="https://fabionatalini.wixsite.com/fabio", "Fabio Natalini", target="_blank")),
            HTML('<a rel="license" href="http://creativecommons.org/licenses/by/4.0/" target="_blank"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by/4.0/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by/4.0/" target="_blank">Creative Commons Attribution 4.0 International License</a>.')
        ),
        # Outputs
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Instructions",
                                 br(),
                                 h3("Welcome to ClimaViz"),
                                 h5("an interactive interface to analyse climatic data"),
                                 hr(),
                                 h4("Input files"),
                                     p("You will need two input files: a daily precipitation data file and a daily temperature data file. Both must be plain files (.txt or .csv) with tab-separated columns like the following:"),
                                     dataTableOutput("demo_table"),
                                     p("The 4th column contains the daily climate records. The 1st, 2nd and 3rd columns store the time reference of each record, i.e. year, month and day of the month respectively. The table in the example above shows a sample of precipitation records of the meteorological station 'Ronda del este' located in Huelva, Spain. So, for instance, on the 1st of January 1920 there were 2.1 mm of rain."),
                                     p("You can download the precipitation and temperature data of the meteorological station 'Ronda del este' here:"),
                                     p("Download ",a(href="peca3937.csv", "precipitation demo data", download=NA, target="_blank")),
                                     p("Download ",a(href="teca418.csv", "temperature demo data", download=NA, target="_blank")),
                                     p("Both files were retrieved from the international climate databank hosted by the Regional Climate Centre of the World Meteorological Organization at the Royal Netherlands Meteorological Institute",a(href="https://climexp.knmi.nl","https://climexp.knmi.nl",target="_blank"),"."),
                                     hr(),
                                 h4("Climograph"),
                                    p("After uploading the precipitation and temperature input files, in the tab 'Climograph' you can visualize a climograph following the standard Bagnouls-Gaussen diagram
                                      where the scale of the precipitation data is double the scale of the temperature data."),
                                    hr(),
                                 h4("P trends and T trends"),
                                    p("After uploading the precipitation data input file, in the tab 'P trends' you can visualize the series of monthly sums of precipitations and the series of annual sums of precipitations. 
                                      A polynomial line is fitted to both series, and you are asked to select the degree of the polynomial (a 1-degree polynomial, i.e. a linear regression line, is configured as a default)."),
                                    p("Moreover, the program asks you to select the first month of the hydrological year, i.e. the period of 12 months to calculate the annual sums of precipitations. 
                                      October is the default, so the annual value will be computed as the sum of precipitations from the 1st of October to the 30th of September of the next year. 
                                      Please note that the next year is the reference to label the hydrological year, e.g. the hydrological year 2019 is from 2018-10-01 to 2019-09-30. 
                                      If you select January as the first month, the calendar year will be calculated instead."),
                                    p("Similarly, after uploading the daily temperature data input file, in the tab 'T trends' you can visualize the series of monthly and annual means of temperature."),
                                    hr(),
                                 h4("P stats and T stats"),
                                    p("After uploading the daily data input files, in the tabs 'P stats' and 'T stats' you can visualize the ranked monthly values, the ranked annual values and the ranked daily records."),
                                    p("For the ranked monthly values, there is one table per month showing the ordered climate values (one value per year). For example, if you use the precipitation data from Huelva indicated above, you will see one table per month with the sums of precipitations in each year starting from 1920."),
                                    p("For the ranked annual values, the annual sums of the precipitation data and the annual means of the temperature data will be calculated and showed in decreasing order. The program will ask you to select the first month of the hydrological year."),
                                    p("Finally, for the ranked daily records, the program first orders both input data files, and then it shows the highest 100 and the lowest 100 daily records in two tables."),
                                    br()
                                 ),
                        tabPanel("Climograph",
                                 br(),
                                 uiOutput("ui1"),
                                 br(),
                                 plotlyOutput("plot1"),#,width="70%",height="400px"),
                                 br()
                                 ),
                        tabPanel("P trends",
                                 br(),
                                 uiOutput("ui2"),
                                 br(),
                                 uiOutput("ui3"),
                                 br(),
                                 uiOutput("ui4"),
                                 br(),
                                 plotlyOutput("plot2"),
                                 br(),
                                 plotlyOutput("plot3"),
                                 br()
                                 ),
                        tabPanel("T trends",
                                 br(),
                                 uiOutput("ui5"),
                                 br(),
                                 uiOutput("ui6"),
                                 br(),
                                 uiOutput("ui7"),
                                 br(),
                                 plotlyOutput("plot4"),
                                 br(),
                                 plotlyOutput("plot5"),
                                 br()
                                 ),
                        tabPanel("P stats",
                                 hr(),hr(),
                                 h3("Ranked annual values and daily records"),
                                 br(),
                                 uiOutput("ui8"),
                                 br(),
                                 uiOutput("ui9"),
                                 br(),
                                 fluidRow(
                                     column(4,dataTableOutput("ps25")),
                                     column(4,dataTableOutput("ps26")),
                                     column(4,dataTableOutput("ps27"))),
                                 br(),
                                 hr(),hr(),
                                 h3("Ranked monthly values"),
                                 fluidRow(
                                     column(3,dataTableOutput("ps1")),
                                     column(3,dataTableOutput("ps2")),
                                     column(3,dataTableOutput("ps3")),
                                     column(3,dataTableOutput("ps4"))),
                                 fluidRow(
                                     column(3,dataTableOutput("ps5")),
                                     column(3,dataTableOutput("ps6")),
                                     column(3,dataTableOutput("ps7")),
                                     column(3,dataTableOutput("ps8"))),
                                 fluidRow(
                                     column(3,dataTableOutput("ps9")),
                                     column(3,dataTableOutput("ps10")),
                                     column(3,dataTableOutput("ps11")),
                                     column(3,dataTableOutput("ps12")))
                                 ),
                        tabPanel("T stats",
                                 hr(),hr(),
                                 h3("Ranked annual values and daily records"),
                                 br(),
                                 uiOutput("ui10"),
                                 br(),
                                 uiOutput("ui11"),
                                 br(),
                                 fluidRow(
                                     column(4,dataTableOutput("ps28")),
                                     column(4,dataTableOutput("ps29")),
                                     column(4,dataTableOutput("ps30"))),
                                 br(),
                                 hr(),hr(),
                                 h3("Ranked monthly values"),
                                 br(),
                                 fluidRow(
                                     column(3,dataTableOutput("ps13")),
                                     column(3,dataTableOutput("ps14")),
                                     column(3,dataTableOutput("ps15")),
                                     column(3,dataTableOutput("ps16"))),
                                 fluidRow(
                                     column(3,dataTableOutput("ps17")),
                                     column(3,dataTableOutput("ps18")),
                                     column(3,dataTableOutput("ps19")),
                                     column(3,dataTableOutput("ps20"))),
                                 fluidRow(
                                     column(3,dataTableOutput("ps21")),
                                     column(3,dataTableOutput("ps22")),
                                     column(3,dataTableOutput("ps23")),
                                     column(3,dataTableOutput("ps24")))
                                 ),
                        tabPanel("Custom period",
                                 br(),
                                 uiOutput("ui12"),
                                 br(),
                                 uiOutput("ui13"),
                                 br(),
                                 uiOutput("ui14")
                                 )
                        )
            )
    )
))


