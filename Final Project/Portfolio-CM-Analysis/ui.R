
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)

library(DT)
library(dygraphs)
library(plotly)
library(Quandl)

library(fPortfolio)
library(quantmod)
library(PerformanceAnalytics)
library(SIT)

library(fGarch)
library(FinTS)
library(tseries)
library(forecast)
library(smooth)

Quandl.api_key("ZQfFzzwASL4_NbCPSPVn")

iso_data_import = read.csv('data/country-iso.csv')


iso_data = data.frame(ISO = iso_data_import$ISO, 
                      row.names = iso_data_import$Country)

# Define UI for application that draws a histogram
shinyUI(dashboardPage(skin = "blue",
                      dashboardHeader(title = "Portfolio & Capital Market Analysis",
                                      titleWidth = 350),
                      dashboardSidebar(
                        width = 350,
                        useShinyjs(),
                        sidebarMenu(
                          
                          tags$head(tags$style(".inactiveLink {
                            pointer-events: none;
                           cursor: default;
                           }")),
                          tags$head(tags$style(HTML("hr {width: 100px;}"))),
                          
                          
                          id = "menu",
                          
                          menuItem("About", tabName = "about", icon = icon("question")),
                          
                          hr(),
                          
                          menuItem("Portfolio Analysis", tabName = "port-anal", icon = NULL,
                          
                          textInput(inputId = "symbols", 
                                    label = "Ticker Symbols", 
                                    placeholder = "GOOG, AAPL, FB, ... etc.",
                                    value = ""),
                                
                          dateRangeInput("dates", 
                                         "Select Date Range",
                                         start = "2013-01-01", end = "2016-12-31",
                                         min = '2005-01-01', max = Sys.Date()),
                          
                          awesomeRadio(inputId = "period",
                                       label = "Period",
                                       choices = c('Daily' = 'days',
                                                   'Monthly' = 'months',
                                                   'Quarterly' = 'quarters',
                                                   'Yearly' = 'years'),
                                       selected  = 'months',
                                       inline = TRUE,
                                       width = '100%'
                            
                          ),
                          
                          actionButton("import", " Import Data", icon = icon("paper-plane")),
                          
                          hr(),
                          
                          menuItem("Stock Charts & Data", tabName = "stock", icon = icon("chart-line")),
                          menuItem("Portfolio Simulation", tabName = "simulation", icon = icon("chart-pie")),
                          #menuItem("Portfolio Risk & Ratio Analysis", tabName = "risk", icon = icon("chart-pie")),
                          
                          
                                   # menuSubItem("One-Factor", tabName = "one", icon = icon("exclamation-triangle")),
                                   # menuSubItem("Fama-French", tabName = "ff", icon = icon("divide")),
                                   # startExpanded = FALSE
                          
                          menuItem("Portfolio Fitting & Forecasting", tabName = "forecast", icon = icon('sliders-h'),
                                   uiOutput('training'),
                                   #menuItem("CAPM", tabName = "capm", icon = icon("folder-open")),
                                   #menuItem("Fama-French", tabName = "fama", icon = icon("folder-open")),
                                   menuSubItem("Naive", tabName = "naive"),
                                   menuSubItem("Moving Average", tabName = "ma"),
                                   menuSubItem("Exponential Smoothing", tabName = "es"),
                                   menuSubItem("Holt", tabName = "holt"),
                                   menuSubItem("Holt-Winters", tabName = "holtw"),
                                   menuSubItem("ARIMA", tabName = "arima"),
                                   menuSubItem("Accuracy & Model Selection", tabName = "model"),
                                   startExpanded = FALSE
                            ),
                          startExpanded = TRUE
                          ),
                          
                          hr(),
                          
                          menuItem("Capital Market Analysis", tabName = "cm", icon = NULL,
                          
                          selectizeInput(inputId = "country", 
                                         label = "Select a Country or Region", 
                                         choices = iso_data_import$Country,
                                         selected = NULL,
                                         multiple = TRUE,
                                         options = list(maxItems = 1)
                          ),
                          
                          dateRangeInput("dates2", 
                                         "Date Range",
                                         start = "1998-12-31", end = "2016-12-31",
                                         min = "1960-12-31", max = "2016-12-31",
                                         format = "yyyy-mm-dd"
                          ),
                          
                          
                          actionButton("import2", "Import Data", icon = icon("paper-plane")),
                          
                          
                          hr(),
                          
                          menuSubItem("Economic Indicators", tabName = "indicators", icon = icon("chart-bar")
                          ),
                          
                          menuItem("Trade Accounts", tabName = "trade", icon = icon("plane"),
                                   menuSubItem("Balance of Payments", tabName = "bop", icon = icon("balance-scale")),
                                   menuSubItem("Current Account", tabName = "ca", icon = icon("exchange-alt"))
                          ),
                          
                          menuSubItem("Savings & Investments", tabName = "savings", icon = icon("piggy-bank")
                          ),
                          
                          menuSubItem("Currency", tabName = "currency", icon = icon("coins")
                          ),
                          
                          menuSubItem("Tariffs", tabName = "tariffs", icon = icon("globe-americas")),
                          startExpanded = FALSE
                          
                          )
                        )
                      ),
                      dashboardBody(
                        tags$style(type="text/css",
                                   ".shiny-output-error { visibility: hidden; }",
                                   ".shiny-output-error:before { visibility: hidden; }"),
                        tabItems(
                          tabItem(tabName = "about",
                                  h1("Welcome!"),
                                  fluidRow(
                                    box(title = NULL,
                                        width = 12,
                                        status = "primary",
                                        p(strong("Hello and welcome to the interactive Portfolio & Capital Markets Analysis tool. 
                                                 This application was designed with the intention of exploring financial stock data, simulating portfolios, 
                                                 running regression and regression analysis, as well as looking at country data from around the world. Begin by typing
                                                 in the tickers for stock symbols on the left hand side and press import. For more information on the application
                                                 feel free to contact the author below.")),
                                        br(),
                                        p(strong("This application was designed using RShiny. Data is imported from the Yahoo Finance and the World Bank."))
                                        )
                                        ),
                                  
                                  fluidRow(
                                    box(title = "Contact the Author",
                                        width = 12,
                                        status = "primary",
                                        background = "orange",
                                        collapsible = TRUE,
                                        collapsed = TRUE,
                                        fluidRow(
                                          box(
                                            width = 6,
                                            background = "orange",
                                            img(src = 'Jared.jpg', width = "100%", height = "50%")
                                          ),
                                          
                                          box(title = "Jared Johnson",
                                              width = 6,
                                              background = "orange",
                                              "Jared completed his undergraduate degree from UC Berkeley and has continued his education in the 
                                              Masters in Finance Program at USC Marshall School of Business.",
                                              br(),
                                              br(),
                                              "Connect on",
                                              a("LinkedIn!", href = "https://www.linkedin.com/in/jared-b-johnson/")
                                          )
                                        )
                                    )
                                  )
                                  
                                  
                                        ),
                          
                          tabItem(tabName = "stock",
                                  h4(textOutput("stock_header")),
                                  fluidRow(height = '100%',
                                    box(  
                                      width = 12,
                                      background = "light-blue",
                                      
                                    box(title = textOutput("stock_box"),
                                        width = 9,
                                        status = "primary",
                                        plotOutput("stock_graph")
                                    ),
                                    
                                    box(title = " Stock Options",
                                        width = 3,
                                        background = "light-blue",
                                        
                                        uiOutput("select-stock"),
                                        
                                        materialSwitch(
                                          inputId = 'isreturns',
                                          strong('Stock Returns?'),
                                          value = FALSE,
                                          status = 'warning',
                                          right = TRUE,
                                          inline = TRUE
                                        ),
                                        
                                        hr(),
                                        
                                        selectInput("chart_type",
                                                    label = "Chart Type",
                                                    choices = c("Line" = "line",
                                                                "Candlestick" = "candlesticks",
                                                                "Matchstick" = "matchsticks",
                                                                "Bar" = "bars"
                                                    ),
                                                    selected = "Line"),
                                        
                                        checkboxInput(inputId = "log_y",
                                                      label = "LOG Y Axis (Prices ONLY)",
                                                      value = FALSE),
                                        hr(),
                                        
                                        p(strong("Technical Analysis")),
                                          checkboxInput("ta_vol", label = "Volume", value = FALSE),
                                          checkboxInput("ta_sma", label = "Simple Moving Average",
                                                        value = FALSE),
                                          checkboxInput("ta_ema", label = "Exponential Moving Average",
                                                        value = FALSE),
                                          checkboxInput("ta_wma", label = "Weighted Moving Average",
                                                        value = FALSE),
                                          checkboxInput("ta_bb", label = "Bolinger Bands",
                                                        value = FALSE),
                                          checkboxInput("ta_momentum", label = "Momentum",
                                                        value = FALSE),
                                        br(),
                                        
                                        actionButton("TA", "Add/Remove Technical Analysis")
                                    )
                                  )
                                ),
                                fluidRow(
                                box(
                                  width = 12,
                                  height = "100%",
                                  status = "primary",
                                  dataTableOutput("stock-table", width = "100%")
                                )
                              )
                          ),
                          
                          tabItem(tabName = "simulation",
                                  fluidRow(
                                    box(title = 'Portfolio Parameters',
                                        width = 12,
                                        height = '100%',
                                        status = "warning",
                                        fluidRow(
                                          column( h4('Model'),
                                               width = 6,
                                               awesomeRadio(
                                                  inputId = 'model',
                                                  'Select a Model Type',
                                                  choices = c('Efficient Portfolio' = 'efficient',
                                                              'Tangency Portfolio' = 'tangency',
                                                              'Minimum Variance Portfolio' = 'minvar',
                                                              'Minimum Risk Portfolio' = 'minrisk',
                                                              'Maximum Return Portfolio' = 'maxreturn'
                                                            ),
                                                  selected = 'tangency'
                                               ),
                                               
                                               h4('Optimization'),
                                               
                                               materialSwitch(
                                                  inputId = 'long',
                                                  strong('Allow Shorting?'),
                                                  value = FALSE,
                                                  status = 'primary'
                                               ),
                                               
                                               radioGroupButtons(
                                                 inputId = "minrisk",
                                                 label = NULL,
                                                 choices = c("Minimize Risk" = 'minRisk', 
                                                             "Maximize Return" = 'maxReturn'),
                                                 justified = TRUE
                                               ),
                                               
                                               
                                               h4('Target Risk & Return'),
                                               numericInput('return',
                                                            'Target Return (%)',
                                                            value = NULL
                                               ),
                                               
                                               numericInput('risk',
                                                            'Target Risk (%)',
                                                            value = NULL
                                                 
                                               )
                                          ),
                                          
                                          column(
                                            width = 6,
                                            
                                            h4('Specifications'),
                                            numericInput('riskfreerate',
                                                         'Risk-Free Rate (%)',
                                                         value = 0.0002
                                                         
                                            ),
                                            
                                            sliderInput(
                                              'numpoints',
                                              'Number of Frontier Points',
                                              min = 50,
                                              max = 200,
                                              value = 100
                                            ),
                                            
                                            h4('Simulation'),

                                            
                                            numericInput('initialvalue',
                                                         'Initial Portfolio Value',
                                                         min = 1,
                                                         value = 100),
                                            
                                            uiOutput('allocationstart'),
                                            
                                            radioGroupButtons(
                                              inputId = "quickdate",
                                              label = "Quickly Change Allocation Range",
                                              choices = c("1 mo." = "one",
                                                          "3 mo." = "three",
                                                          "6 mo." = "six", 
                                                          "1 yr." = "year",
                                                          "2 yr." = "two"),
                                              checkIcon = list(
                                                yes = icon("ok",
                                                           lib = "glyphicon")),
                                              selected = "two"
                                            ),
                                            
                                            materialSwitch(
                                              inputId = 'rebalance',
                                              strong('Allow Rebalancing?'),
                                              value = FALSE,
                                              status = 'primary'
                                            ),
                                            
                                            actionBttn(
                                              inputId = 'generate',
                                              'Generate Portfolio',
                                              style = 'unite',
                                              block = TRUE
                                            )
                                            
                                            
                                          )
                                        )
                                       )
                                  ),
                                  
                                  p(h3(textOutput("portgen_title"))),
                                  
                                  uiOutput("simulation_output1"),
                                  
                                  uiOutput("simulation_output2"),
                                  
                                  p(h3(textOutput("portalloc_title"))),
                                  
                                  uiOutput("simulation_output3"),
                                  
                                  uiOutput("simulation_output4")
                                  
                                ),
                          
                          tabItem( tabName = 'capm',
h4('One-Factor CAPM'),
                                   fluidRow(
                                     box(title = 'SML',
                                         width = 8,
                                         height = '100%',
                                         status = "warning",
                                         plotOutput("capm-sml")
                                         
                                         
                                     ),
                                     
                                     box(
                                       title = "Model Summary",
                                       width = 4,
                                       status = "warning",
                                       verbatimTextOutput('one-factor')
                                       
                                     )
                                   ),
                                   fluidRow(
                                     box(
                                       title  = 'Forecast Graph',
                                       width = 8,
                                       status = "danger",
                                       plotOutput("capm-plot")
                                     )
                                   ),
                                   fluidRow(
                                     box(
                                       title = 'Forecast Summary',
                                       width = 8,
                                       status = "danger",
                                       plotOutput("capm-forecast")
                                       
                                     )
                                   ),
                                  fluidRow(
                                     box(
                                       title = 'Forecast Accuracy',
                                       width = 8,
                                       status = "danger",
                                       verbatimTextOutput("capm-accuracy")
                                     )
                                   ),
                                   fluidRow(
                                     box(
                                       title  = 'CAPM Data',
                                       width = 8,
                                       status = "danger",
                                       collapsible = TRUE,
                                       collapsed = TRUE, 
                                       dataTableOutput("capm-data")
                                       
                                     )
                                   )
                                ),

                    tabItem( tabName = 'fama',
h4('Fama-French'),
                                   
                                  fluidRow(
                                    box(title = 'Forecast Plot',
                                        width = 8,
                                        height = '100%',
                                        status = "danger",
                                        plotOutput("ff-plot")
                                    ),
                                    
                                    box(
                                      title = "Model Summary",
                                      width = 4,
                                      status = "danger",
                                      verbatimTextOutput('ff-model')
                                      
                                    )
                                  ),

                                  fluidRow(
                                    box(
                                      title = 'Forecast Summary',
                                      width = 4,
                                      status = "danger",
                                      plotOutput("ff-forecast")
                                      
                                    ), 
                                    box(
                                      title = 'Forecast Accuracy',
                                      width = 4, offset = 4,
                                      status = "danger",
                                      verbatimTextOutput("ff-accuracy")
                                    )
                                  ),
                                  fluidRow(
                                    box(
                                      title  = 'Fama-French Data',
                                      width = 8,
                                      status = "danger",
                                      collapsible = TRUE,
                                      collapsed = TRUE, 
                                      dataTableOutput("ff-data")
                                      
                                    )
                                  )
                                   
                                   
                            
                          ),
                          
                          tabItem( tabName = 'naive',
h4('Naive'),
                                   
                                  fluidRow(
                                    box(title = 'Forecast Plot',
                                        width = 12,
                                        height = '100%',
                                        status = "danger",
                                        plotOutput("naive-plot")
                                    )
                                  ),
                                  
                                  fluidRow(
                                    box(
                                      title = 'Forecast Summary',
                                      width = 12,
                                      status = "danger",
                                      verbatimTextOutput("naive-forecast")
                                      
                                    )
                                  ),
                                  fluidRow(
                                    box(
                                      title = 'Forecast Accuracy',
                                      width = 12,
                                      status = "danger",
                                      verbatimTextOutput("naive-accuracy")
                                    )
                                  )
                            
                          ),
                          
                          tabItem( tabName = 'ma',
h4('Moving Average'),
                                   fluidRow(
                                     box(title = 'Forecast Plot',
                                         width = 12,
                                         height = '100%',
                                         status = "danger",
                                         fluidRow(
                                           column(
                                             8,
                                             plotOutput("ma-plot")
                                           ),
                                           column(
                                             4,
                                             sliderInput('ma-period',
                                                         'Moving Average Period',
                                                         min = 1,
                                                         max = 20,
                                                         value = 3,
                                                         ticks = TRUE)
                                           )
                                        )
                                      )   
                                     ),
                                   
                                   fluidRow(
                                     box(
                                       title = 'Forecast Summary',
                                       width = 12,
                                       status = "danger",
                                       verbatimTextOutput("ma-forecast")
                                       
                                     )
                                   ),
                                   fluidRow(
                                     box(
                                       title = 'Forecast Accuracy',
                                       width = 12,
                                       status = "danger",
                                       verbatimTextOutput("ma-accuracy")
                                     )
                                   )
                                   
                          ),
                          
                          tabItem( tabName = 'es',
h4('Exponential Smoothing'),
                                   fluidRow(
                                     box(title = 'Forecast Plot',
                                         width = 12,
                                         height = '100%',
                                         status = "danger",
                                         plotOutput("es-plot")
                                     )
                                   ),
                                   
                                   fluidRow(
                                     box(
                                       title = 'Forecast Summary',
                                       width = 12,
                                       status = "danger",
                                       verbatimTextOutput("es-forecast")
                                       
                                     )
                                   ),
                                   fluidRow(
                                     box(
                                       title = 'Forecast Accuracy',
                                       width = 12,
                                       status = "danger",
                                       verbatimTextOutput("es-accuracy")
                                     )
                                   )
                                   
                          ),
                          
                          tabItem( tabName = 'holt',
h4('Holt'),
                                   fluidRow(
                                     box(title = 'Forecast Plot',
                                         width = 12,
                                         height = '100%',
                                         status = "danger",
                                         plotOutput("h-plot")
                                     )
                                   ),
                                   
                                   fluidRow(
                                     box(
                                       title = 'Forecast Summary',
                                       width = 12,
                                       status = "danger",
                                       verbatimTextOutput("h-forecast")
                                       
                                     )
                                   ),
                                   fluidRow(
                                     box(
                                       title = 'Forecast Accuracy',
                                       width = 12,
                                       status = "danger",
                                       verbatimTextOutput("h-accuracy")
                                     )
                                   )
                              ),
                          tabItem( tabName = 'holtw',
h4('Holt-Winters'),
                                   
                                   fluidRow(
                                     box(title = 'Forecast Plot',
                                         width = 12,
                                         height = '100%',
                                         status = "danger",
                                         plotOutput("hw-plot")
                                     )
                                   ),
                                   
                                   fluidRow(
                                     box(
                                       title = 'Forecast Summary',
                                       width = 12,
                                       status = "danger",
                                       verbatimTextOutput("hw-forecast")
                                       
                                     )
                                   ),
                                   fluidRow(
                                     box(
                                       title = 'Forecast Accuracy',
                                       width = 12,
                                       status = "danger",
                                       verbatimTextOutput("hw-accuracy")
                                     )
                                   )
                                   
                          ),
                          
                          tabItem( tabName = 'arima',
h4('ARIMA'),
                                   fluidRow(
                                     box(title = 'ADF Test, ACF, & PACF',
                                         width = 7,
                                         height = '100%',
                                         status = "danger",
                                         strong(textOutput("diff-text")),
                                          verbatimTextOutput("adf-test"),
                                          plotOutput('ggts')
                                     ),

                                    box( title = 'Model Parameters',
                                        width = 5,
                                        height = '100%',
                                        status = 'danger',
                                        h4('Difference the Data (Max 2 Times)'),
                                        actionBttn('difference',
                                                   "Difference",
                                                   style = 'bordered',
                                                   color = "danger",
                                                   block = TRUE),
                                        br(),
                                        
                                        actionBttn('undifference',
                                                   "Un-Difference",
                                                   style = 'bordered',
                                                   color = "warning",
                                                   block = TRUE),
                                        br(),
                                        
h4("ARIMA Model"),
                                    fluidRow(
                                          column(
                                            4,
                                            numericInput('p',
                                                         'p',
                                                         value = 0,
                                                         min = 0,
                                                         max = NA)
                                          ),
                                          column(
                                            4,
                                            numericInput('d',
                                                         'd',
                                                         value = 0,
                                                         min = 0,
                                                         max = NA)
                                          ),

                                          column(
                                            4,
                                            numericInput('q',
                                                         'q',
                                                         value = 0,
                                                         min = 0,
                                                         max = NA)
                                          )
                                        ),
                                  fluidRow(
                                          column(
                                            12,
                                          
                                            actionBttn('arimafit', 
                                                       label = 'Update ARIMA Fit & Forecast', 
                                                       style = 'bordered', 
                                                       color = "success", 
                                                       block = TRUE)
                                          )
                                        )
#                                         br(),
# 
# h4("ARMA + GARCH Model"),
#                                         fluidRow(
#                                           column(
#                                             3,
#                                             p(strong("ARMA"))
#                                           ),
#                                           column(
#                                             3, offset = 3,
#                                             p(strong("GARCH"))
#                                           )
#                                         ),
#                                         fluidRow(
#                                           column(
#                                             3,
#                                             numericInput('p2',
#                                                          'p',
#                                                          value = 0,
#                                                          min = 0,
#                                                          max = NA)
#                                           ),
#                                           column(
#                                             3,
#                                             numericInput('q2',
#                                                          'q',
#                                                          value = 0,
#                                                          min = 0,
#                                                          max = NA)
#                                           ),
# 
#                                           column(
#                                             3,
#                                             numericInput('p3',
#                                                          'p',
#                                                          value = 0,
#                                                          min = 0,
#                                                          max = NA)
#                                           ),
#                                           column(
#                                             3,
#                                             numericInput('q3',
#                                                          'q',
#                                                          value = 0,
#                                                          min = 0,
#                                                          max = NA)
#                                           )
#                                         ),
#                                         fluidRow(
#                                           column(
#                                             8, offset = 4,
#                                             actionBttn('garchfit', label = 'Fit ARMA + GARCH', style = 'bordered', color = "primary", block = TRUE)
#                                           )
#                                         )
                                      )
                                   ),

                              fluidRow(
                                     box(
                                       title = 'Model Summary',
                                       width = 12,
                                       status = "danger",
                                       fluidRow(
                                         column(
                                           6,
                                           verbatimTextOutput("arima-model")
                                         ),
                                         column(
                                           6,
                                           plotOutput("ggts.resid")
                                         )
                                       
                                       )
                                     )
                                   ),
                                   fluidRow(
                                     box(
                                       title = 'Forecast Plot',
                                       width = 6,
                                       status = "danger",
                                       plotOutput("arima-plot")
                                     ),
                                     box(
                                       title = 'Forecast Summary',
                                       width = 6,
                                       status = "danger",
                                       verbatimTextOutput("arima-forecast")
                                     )
                                   ),
                                  fluidRow(
                                    box(
                                      title = 'Forecast Accuracy',
                                      width = 12,
                                      status = "danger",
                                      verbatimTextOutput("arima-accuracy")
                                    )
                                  )
                          ),
                          
                          tabItem( tabName = 'model',
                                   
h4('Forecast Summary'),
                                   fluidRow(
                                     box(title = 'Accuracy Summary',
                                         width = 12,
                                         height = '100%',
                                         status = "danger",
                                         dataTableOutput("acc-table", width = '100%')
                                     )
                                   )
                            
                          ),
tabItem(tabName = "indicators",
        h4(textOutput("indicator_header")),
        fluidRow(
          box(title = textOutput("indicator_box"),
              width = 9,
              height = 500,
              status = "primary",
              dygraphOutput("indicator_graph")
          ),
          
          box(title = "Time Series Options",
              width = 3,
              height = 500,
              background = "light-blue",
              selectizeInput("add_ind",
                             "Add/Change Indicators",
                             choices = c("GDP" = "GDP",
                                         "GDP Per Capita" = "GDPPC",
                                         "GNI" = "GNI",
                                         "GNI Per Capita (Atlas Method)" = "GNIPC",
                                         "Consumer Price Index" = "CPI",
                                         "Unemployment Rate" = "Unemployment"
                             ),
                             selected = "GDP",
                             multiple = TRUE),
              
              selectInput("unit_ind",
                          "Units",
                          choices = c("Dollars (USD)" = "dollar",
                                      "Change, Dollars (USD)" = "diff",
                                      "Percent Change" = "rdiff")),
              hr(),
              
              selectInput("type_ind",
                          "Chart Options",
                          choices = c("Line Graph" = "line",
                                      "Bar Chart" = "bar"),
                          selected = "line"),
              
              checkboxInput("point_ind",
                            "Add/Remove Points",
                            value = FALSE),
              
              checkboxInput("stack_ind",
                            "Stacked Graph",
                            value = FALSE)
              
          )
        )
),

tabItem(tabName = "bop",
        h4(textOutput("bop_header")),
        fluidRow(
          box(title = textOutput("bop_box"),
              width = 9,
              height = 500,
              status = "warning",
              dygraphOutput("bop_graph")
          ),
          
          box(title = "Time Series Options",
              width = 3,
              height = 500,
              background = "yellow",
              selectizeInput("add_bop",
                             "Add/Change Accounts",
                             choices = c("Balance of Payments" = "BoP",
                                         "Current Account" = "Current",
                                         "Capital Account" = "Capital"
                             ),
                             selected = "BoP",
                             multiple = TRUE),
              
              selectInput("unit_bop",
                          "Units",
                          choices = c("Dollars (USD)" = "dollar",
                                      "Change, Dollars (USD)" = "diff",
                                      "Percent Change" = "rdiff")),
              
              hr(),
              
              selectInput("type_bop",
                          "Chart Options",
                          choices = c("Line Graph" = "line",
                                      "Bar Chart" = "bar"),
                          selected = "bar"),
              
              checkboxInput("point_bop",
                            "Add/Remove Points",
                            value = FALSE),
              
              checkboxInput("stack_bop",
                            "Stacked Graph",
                            value = FALSE)
              
          )
        ),
        
        fluidRow(
          box(title = "Pie Chart",
              width = 4,
              background = "yellow",
              "Displays a decomposition of the Balance of Payments in the last year selected."
              
          ),
          box(title = textOutput("bop_box2"),
              width = 8,
              status = "warning",
              plotlyOutput("bop_pie")
          )
        )
),

tabItem(tabName = "ca",
        h4(textOutput("ca_header")),
        fluidRow(
          box(title = textOutput("ca_box"),
              width = 9,
              height = 500,
              status = "danger",
              dygraphOutput("ca_graph")
          ),
          
          box(title = "Time Series Options",
              width = 3,
              height = 500,
              background = "red",
              selectizeInput("add_ca",
                             "Add/Change Accounts",
                             choices = c("Current Account" = "Current",
                                         "Exports" = "Exports",
                                         "Imports" = "Imports",
                                         "Balance of Trade" = "BoT",
                                         "Net Income" = "NI",
                                         "Net Captial Transfers" = "NCT"
                             ),
                             selected = "Current",
                             multiple = TRUE),
              
              selectInput("unit_ca",
                          "Units",
                          choices = c("Dollars (USD)" = "dollar",
                                      "Change, Dollars (USD)" = "diff",
                                      "Percent Change" = "rdiff",
                                      "Percent GDP" = "pgdp")),
              hr(),
              
              selectInput("type_ca",
                          "Chart Options",
                          choices = c("Line Graph" = "line",
                                      "Bar Chart" = "bar"),
                          selected = "bar"),
              
              checkboxInput("point_ca",
                            "Add/Remove Points",
                            value = FALSE),
              
              checkboxInput("stack_ca",
                            "Stacked Graph",
                            value = FALSE)
              
          )
        ),
        
        fluidRow(
          box(title = "Pie Chart Options",
              width = 4,
              background = "red",
              "Displays a decomposition of the Current Account or Balance of Trade in the last year selected.",
              selectInput("ca_pie_option",
                          label = NULL,
                          choices = c("Current Account" = "current",
                                      "Balance of Trade" = "bot"),
                          selected = "current")
              
          ),
          
          box(title = textOutput("ca_box2"),
              width = 8,
              status = "danger",
              plotlyOutput("ca_pie")
          )
        )
),

tabItem(tabName = "savings",
        h4(textOutput("savings_header")),
        fluidRow(
          box(title = textOutput("savings_box"),
              width = 9,
              height = 500,
              status = "primary",
              dygraphOutput("savings_graph")
          ),
          
          box(title = "Time Series Options",
              width = 3,
              height = 500,
              background = "light-blue",
              selectizeInput("add_savings",
                             "Add/Change Accounts",
                             choices = c("Savings" = "Savings",
                                         "Investments" = "Investments",
                                         "Current Account" = "Current",
                                         "Net National Savings/Borrowing" = "NNSB"
                             ),
                             selected = "Savings",
                             multiple = TRUE),
              
              selectInput("unit_savings",
                          "Units",
                          choices = c("Dollars (USD)" = "dollar",
                                      "Change, Dollars (USD)" = "diff",
                                      "Percent Change" = "rdiff")),
              hr(),
              
              selectInput("type_savings",
                          "Chart Options",
                          choices = c("Line Graph" = "line",
                                      "Bar Chart" = "bar"),
                          selected = "bar"),
              
              checkboxInput("point_savings",
                            "Add/Remove Points",
                            value = FALSE),
              
              checkboxInput("stack_savings",
                            "Stacked Graph",
                            value = FALSE)
          )
        )
        
),

tabItem(tabName = "currency",
        h4(textOutput("currency_header")),
        fluidRow(
          box(title = textOutput("currency_box"),
              width = 9,
              height = 500,
              status = "warning",
              dygraphOutput("currency_graph")
          ),
          
          box(title = "Time Series Options",
              width = 3,
              height = 500,
              background = "yellow",
              selectizeInput("add_currency",
                             "Add/Change Indices",
                             choices = c("Official Exchange Rate" = "Exchange",
                                         "PPP to Market Exchange" = "PPP2E",
                                         "Real Effective Exchange Rate Index" = "REED",
                                         "Interest Rate" = "Interest"
                             ),
                             selected = "Exchange",
                             multiple = TRUE),
              
              selectInput("unit_currency",
                          "Units",
                          choices = c("Base Index" = "index",
                                      "Change, Base Index" = "diff",
                                      "Percent Change" = "rdiff")),
              hr(),
              
              p(strong("Chart Options")),
              
              checkboxInput("point_currency",
                            "Add/Remove Points",
                            value = FALSE),
              
              checkboxInput("stack_currency",
                            "Stacked Graph",
                            value = FALSE),
              
              checkboxInput("step_currency",
                            "Step Plot",
                            value = TRUE)
          )
        )
        
),

tabItem(tabName = "tariffs",
        h4(textOutput("tariffs_header")),
        fluidRow(
          box(title = textOutput("tariffs_box"),
              width = 9,
              height = 500,
              status = "danger",
              dygraphOutput("tariffs_graph")
          ),
          
          box(title = "Time Series Options",
              width = 3,
              height = 500,
              background = "red",
              selectizeInput("add_tariffs",
                             "Add/Change Accounts",
                             choices = c("All Products" = "All",
                                         "Manufactured Products" = "Manufactured",
                                         "Primary Products" = "Primary"
                             ),
                             selected = "All",
                             multiple = TRUE),
              
              selectInput("nations_tariffs",
                          "Nations",
                          choices = c("Applied" = "AR",
                                      "Favored Nations" = "FN")),
              
              selectInput("unit_tariffs",
                          "Units",
                          choices = c("Base Index" = "index",
                                      "Change, Base Index" = "diff",
                                      "Percent Change" = "rdiff")),
              
              selectInput("average_tariffs",
                          "Units",
                          choices = c("Simple Mean" = "SM",
                                      "Weighted Mean" = "WM")),
              hr(),
              
              p(strong("Chart Options")),
              
              checkboxInput("point_tariffs",
                            "Add/Remove Points",
                            value = FALSE),
              
              checkboxInput("stack_tariffs",
                            "Stacked Graph",
                            value = FALSE),
              
              checkboxInput("step_tariffs",
                            "Step Plot",
                            value = TRUE)
          )
        )
)
                          
                          )
                      )

))
