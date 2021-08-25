library(shiny)
library(plotly)
library(DT)

source("www/helpers.R")
testo_istruzioni <- paste(readLines("www/instruction_tab.html"), collapse=" ")

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
            HTML('<p style="font-size:80%;">
                 <a rel="license" href="http://creativecommons.org/licenses/by/4.0/" target="_blank"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by/4.0/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by/4.0/" target="_blank">Creative Commons Attribution 4.0 International License</a>
                 </p>'),
            br(),
            actionButton("showDisclaimer", "Disclaimer", 
                         style='border: none; color: grey; 
                                padding: 1px; font-size: 80%;
                                text-align: center; margin: 1px')
        ),
        # Outputs
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Instructions",
                                 br(),
                                 h3("Welcome to climeStats"),
                                 h5("an interactive interface to analyse climatic data"),
                                 hr(),
                                 h4("Input files"),
                                     p("You will need two input files: a daily precipitation data file and a daily temperature data file. Upload them on the sidebar. Both must be plain files (.txt or .csv) with tab-separated columns like the following:"),
                                     dataTableOutput("demo_table"),
                                     p("The 4th column contains the daily climate records. The 1st, 2nd and 3rd columns store the time reference of each record, i.e. year, month and day of the month respectively. The table in the example above shows a sample of precipitation records of the meteorological station 'Ronda del este' located in Huelva, Spain. So, for instance, on the 1st of January 1920 there were 2.1 mm of rain."),
                                     p("To test the program, you can download the precipitation and temperature data of the meteorological station 'Ronda del este' here:"),
                                     # HTML('<a href="peca3937.txt" download><p>Download precipitation demo data</p></a>'),
                                     # HTML('<a href="teca418.txt" download><p>Download temperature demo data</p></a>'),
                                     p("Download ",a(href="peca3937.txt", "precipitation demo data", download=NA, target="_blank")),
                                     p("Download ",a(href="teca418.txt", "temperature demo data", download=NA, target="_blank")),
                                     p("Both files were retrieved from the international climate databank hosted by the Regional Climate Centre of the World Meteorological Organization at the Royal Netherlands Meteorological Institute",a(href="https://climexp.knmi.nl","https://climexp.knmi.nl",target="_blank"),"."),
                                     HTML('<p><b>NOTE: </b><i>the program does not handle missing data and takes the input files "as they are"</i></p>'),
                                     hr(),
                                 HTML(testo_istruzioni)
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
                                 uiOutput("ui14"),
                                 br(),
                                 uiOutput("ui15"),
                                 br(),
                                 uiOutput("ui16"),
                                 br(),
                                 plotlyOutput("plot6"),
                                 br(),
                                 plotlyOutput("plot7"),
                                 )
                        )
            )
    )
))


