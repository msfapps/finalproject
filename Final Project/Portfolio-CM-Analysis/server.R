#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)

# url = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_daily_CSV.zip"
# 
# download.file(url, "F-F_Research_Data_Factors_daily_CSV.zip")
# unzip("F-F_Research_Data_Factors_daily_CSV.zip")
# file.remove("F-F_Research_Data_Factors_daily_CSV.zip")
# 
# f <- read.csv("F-F_Research_Data_Factors_daily.CSV", header=FALSE, skip=5)
# f = f[-nrow(f),]
# f$V1 = as.Date(f$V1, format = "%Y%m%d")
# 
# ff <- data.frame(MRP = f$V2, SMB = f$V3, HML = f$V4, RF = f$V5)
# 
# ff.ts <- xts(ff, order.by = f$V1)
# 
# head(f$V1)
# 
# head(ff)

Quandl.api_key("ZQfFzzwASL4_NbCPSPVn")

iso_data_import = read.csv('data/country-iso.csv')


iso_data = data.frame(ISO = iso_data_import$ISO, 
                      row.names = iso_data_import$Country)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  # addCssClass(selector = "a[data-value='stock']", class = "inactiveLink")
  # addCssClass(selector = "a[data-value='simulation']", class = "inactiveLink")
  # #addCssClass(selector = "a[data-value='capm']", class = "inactiveLink")
  # #addCssClass(selector = "a[data-value='one']", class = "inactiveLink")
  # #addCssClass(selector = "a[data-value='ff']", class = "inactiveLink")
  # addCssClass(selector = "a[data-value='forecast']", class = "inactiveLink")
  # addCssClass(selector = "a[data-value='naive']", class = "inactiveLink")
  # addCssClass(selector = "a[data-value='ma']", class = "inactiveLink")
  # addCssClass(selector = "a[data-value='es']", class = "inactiveLink")
  # addCssClass(selector = "a[data-value='holt']", class = "inactiveLink")
  # addCssClass(selector = "a[data-value='holtw']", class = "inactiveLink")
  # addCssClass(selector = "a[data-value='arima']", class = "inactiveLink")
  # addCssClass(selector = "a[data-value='model']", class = "inactiveLink")
  # #addCssClass(selector = "a[data-value='risk']", class = "inactiveLink")
  # 
  # observe({
  #   if(input$symbols == ""){
  #     addCssClass(selector = "a[data-value='stock']", class = "inactiveLink")
  #     addCssClass(selector = "a[data-value='simulation']", class = "inactiveLink")
  #     #addCssClass(selector = "a[data-value='capm']", class = "inactiveLink")
  #     #addCssClass(selector = "a[data-value='one']", class = "inactiveLink")
  #     #addCssClass(selector = "a[data-value='ff']", class = "inactiveLink")
  #     addCssClass(selector = "a[data-value='forecast']", class = "inactiveLink")
  #     addCssClass(selector = "a[data-value='naive']", class = "inactiveLink")
  #     addCssClass(selector = "a[data-value='ma']", class = "inactiveLink")
  #     addCssClass(selector = "a[data-value='es']", class = "inactiveLink")
  #     addCssClass(selector = "a[data-value='holt']", class = "inactiveLink")
  #     addCssClass(selector = "a[data-value='holtw']", class = "inactiveLink")
  #     addCssClass(selector = "a[data-value='arima']", class = "inactiveLink")
  #     addCssClass(selector = "a[data-value='model']", class = "inactiveLink")
  #     #addCssClass(selector = "a[data-value='risk']", class = "inactiveLink")
  #   }
  # 
  #   else if (length(symbols())<=2){
  #     addCssClass(selector = "a[data-value='stock']", class = "inactiveLink")
  #     addCssClass(selector = "a[data-value='simulation']", class = "inactiveLink")
  #     #addCssClass(selector = "a[data-value='capm']", class = "inactiveLink")
  #     #addCssClass(selector = "a[data-value='one']", class = "inactiveLink")
  #     #addCssClass(selector = "a[data-value='ff']", class = "inactiveLink")
  #     addCssClass(selector = "a[data-value='forecast']", class = "inactiveLink")
  #     addCssClass(selector = "a[data-value='naive']", class = "inactiveLink")
  #     addCssClass(selector = "a[data-value='ma']", class = "inactiveLink")
  #     addCssClass(selector = "a[data-value='es']", class = "inactiveLink")
  #     addCssClass(selector = "a[data-value='holt']", class = "inactiveLink")
  #     addCssClass(selector = "a[data-value='holtw']", class = "inactiveLink")
  #     addCssClass(selector = "a[data-value='arima']", class = "inactiveLink")
  #     addCssClass(selector = "a[data-value='model']", class = "inactiveLink")
  #     #addCssClass(selector = "a[data-value='risk']", class = "inactiveLink")
  #   }
  #   
  #   else i{
  #     removeCssClass(selector = "a[data-value='stock']", class = "inactiveLink")
  #     removeCssClass(selector = "a[data-value='simulation']", class = "inactiveLink")
  #   }
  #   
  #   if(input$generate >= 1){
  #     #removeCssClass(selector = "a[data-value='capm']", class = "inactiveLink")
  #     #removeCssClass(selector = "a[data-value='one']", class = "inactiveLink")
  #     #removeCssClass(selector = "a[data-value='ff']", class = "inactiveLink")
  #     removeCssClass(selector = "a[data-value='forecast']", class = "inactiveLink")
  #     removeCssClass(selector = "a[data-value='naive']", class = "inactiveLink")
  #     removeCssClass(selector = "a[data-value='ma']", class = "inactiveLink")
  #     removeCssClass(selector = "a[data-value='es']", class = "inactiveLink")
  #     removeCssClass(selector = "a[data-value='holt']", class = "inactiveLink")
  #     removeCssClass(selector = "a[data-value='holtw']", class = "inactiveLink")
  #     removeCssClass(selector = "a[data-value='arima']", class = "inactiveLink")
  #     removeCssClass(selector = "a[data-value='model']", class = "inactiveLink")
  #     #removeCssClass(selector = "a[data-value='risk']", class = "inactiveLink")
  #   }
  # })
  

# Render UIs
  
  # Render Allocation & Port Date Range -------------------------------------------------------
  
  output$training <- renderUI({
    if (input$generate < 1){
      start = seq(Sys.Date(), length = 2, by = "-189 days")[2]
      end = Sys.Date()
      
      min = NULL
      max = NULL
    }
    else{
    start_date = input$portrange[1]
    end_date = input$portrange[2]
    
    min = seq(end_date, length = 2, by = "+2 days")[2]
    max = Sys.Date()
    
    start = seq(end_date, length = 2, by = "+2 days")[2]
    end = Sys.Date()
  }
    
    dateRangeInput('trainingrange',
                   'Training Set for Forecasting',
                   min = min,
                   max = max,
                   start = start,
                   end = end
                   )
                    
  })

  
  output$allocationstart <- renderUI({
    quick_jump = input$quickdate
    start = seq(end_date(), length = 2, by =  "+1 days")[2]
    
    if (quick_jump == "one")
      final = seq(end_date(), length = 2, by = "+29 days")[2]
    
    else if (quick_jump == "three")
      final = seq(end_date(), length = 2, by = "+89 days")[2]
    
    else if (quick_jump == "six")
      final = seq(end_date(), length = 2, by = "+181 days")[2]
    
    else if (quick_jump == "year")
      final = seq(end_date(), length = 2, by = "+364 days")[2]
    
    else if (quick_jump == "two")
      final = seq(end_date(), length = 2, by = "+729 days")[2]
    
    
    dateRangeInput(
      'portrange',
      'Portfolio Allocation Date Range',
      min = end_date(),
      max = Sys.Date(),
      start = start,
      end = final
    )
  })
  
  # Create Portfolio Simulation UI ---------------------------------------------------
  
  rcode_title <- eventReactive(input$generate, {"Print Out"})
  weights_title <- eventReactive(input$generate, {"Key Stats"})
  frontier_title <- eventReactive(input$generate, {"Frontier"})
  portgraph_title <- eventReactive(input$generate, {"Portfolio Graph"})
  allocvalue_title <- eventReactive(input$generate, {"Allocation Value"})
  allocreturn_title <- eventReactive(input$generate, {"Allocation Returns"})
  allocdata_title <- eventReactive(input$generate, {"Allocation Data"})
  
  portgenerate_title <- eventReactive(input$generate, {"Generated Portfolio"})
  portallocation_title <- eventReactive(input$generate, {"Portfolio Allocation"})
  
  output$portgen_title <- renderText({portgenerate_title()})
  output$portalloc_title <- renderText({portallocation_title()})
  
  output$`select-stock` <- renderUI({
    selectInput("stockindex",
              label = "Select Stock",
              choices = symbols(),
              selected = symbols()[1]
    )
  })
  
  output$simulation_output1 <- renderUI({
    
    fluidRow(
      box(
        title = rcode_title(),
        width = 4,
        height = '100%',
        status = 'warning',
        verbatimTextOutput('port_output')
      ),
      
      box(
        title = weights_title(),
        width = 8,
        height = '100%',
        status = 'warning',
        fluidRow(
          column(
            12, 
            plotOutput('pie')
          )
        ),
        fluidRow(
            column(
              2, offset = 2,
              strong('Mean'),
              h2(textOutput('port_mean'))
            ),
            column(
              2,
              strong('Variance'),
              h2(textOutput('port_risk'))
            ),
            column(
              2,
              strong('CVaR'),
              h2(textOutput('port_cvar'))
            ),
            column(
              2,
              strong('VaR'),
              h2(textOutput('port_var'))
            )
        )
      )
    )
  })
    
  output$simulation_output2 <- renderUI({
    
    fluidRow(
      box(
        title = frontier_title(),
        width = 12,
        height = '100%',
        status = 'warning',
        plotOutput('frontier')
      )
    )
    
  })
  
  output$simulation_output3 <- renderUI({
    fluidRow(
      box(
        title = allocvalue_title(),
        width = 6,
        height = '100%',
        status = 'success',
        plotlyOutput('alloc_values')
      ),
      
      box(
        title = allocreturn_title(),
        width = 6,
        height = '100%',
        status = 'success',
        plotlyOutput('alloc_returns')
      )
    )
    
  })
  
  output$simulation_output4 <- renderUI({
    fluidRow(
      box(
        title = allocdata_title(),
        width = 12,
        height = '100%',
        status = 'success',
        dataTableOutput('stock_allocation', width = '100%')
      )
    )
    
  })
  
  output$`stock-table` <- DT::renderDataTable({
    stock = input$stockindex
    data = select.ohlc()
    
    if(input$isreturns == "TRUE")
      stock = paste0(stock,"R")
    
    select_stock = round(as.timeSeries(data[[as.character(stock)]]), 3)
      colnames(select_stock) = c(paste(stock, "Open"), 
                                 paste(stock, "High"), 
                                 paste(stock, "Low"), 
                                 paste(stock, "Close"), 
                                 paste(stock, "Volume"), 
                                 paste(stock, "Adjusted"))
    
    datatable(select_stock, rownames = TRUE)
  })
  

  
# Render Outputs 
  # Portfolio Simulation ----------------------------------------------------
 
  output$port_output <- renderPrint({
    portfolio()
  }) 
  
  output$frontier <- renderPlot({
     frontier = portfolio_frontier()
      
     plot(frontier, c(1,2,3))
     
   })
    
   output$pie <- renderPlot({
      portfolio = portfolio()
      
      weightsPie(portfolio)
    })
    
   output$port_mean <- renderText({
      portfolio = portfolio()
      
      return = as.numeric(round(getTargetReturn(portfolio)[1], 4))
      
      paste0(return*100,'%')
      
    })
    
  output$port_risk <- renderText({
      portfolio = portfolio()
      
      risk = as.numeric(round(getTargetRisk(portfolio)[1], 4))
      
      paste0(risk*100,'%')
      
    })
  
  output$port_cvar <- renderText({
    portfolio = portfolio()
    
    cvar = as.numeric(round(getTargetRisk(portfolio)[3], 4))
    
    paste0(cvar*100,'%')
    
  })
  
  output$port_var <- renderText({
    portfolio = portfolio()
    
    var = as.numeric(round(getTargetRisk(portfolio)[4], 4))
    
    paste0(var*100,'%')
    
  })
  
  output$alloc_values <- renderPlotly({
    port.data = as.data.frame(port.values())
    
    x = as.Date(row.names((port.data)))
    y = port.data$Portfolio
    
    plot_ly(x = ~x, y = ~y, mode = 'lines', type = 'scatter')%>%
      layout(
        xaxis = list(title = 'Date',
                     rangeslider = list(type = "date")),
        yaxis = list(title = 'Value')
      )
    
  })
  
  output$alloc_returns <- renderPlotly({
    port.data = as.data.frame(port.returns())
    
    x = as.Date(row.names(port.data))
    y = port.data$Portfolio
    
    plot_ly(x = ~x, y = ~y, mode = 'lines', type = 'scatter')%>%
      layout(
        xaxis = list(title = 'Date',
                     rangeslider = list(type = "date")),
        yaxis = list(title = 'Returns')
      )
    
  })
    
  output$stock_allocation <- DT::renderDataTable({
      
    start = input$portrange[1]
    end = input$portrange[2]
    
    port.alloc = window(port.alloc(), start = start, end = end)
      
      
    datatable(as.data.frame(port.alloc), rownames = TRUE)
    
    
    })
  
  # Portfolio Forecasting
    
# Grab Dates --------------------------------------------------------------
start_date = eventReactive(input$import, {input$dates[1]})
end_date = eventReactive(input$import, {input$dates[2]})

dates <- eventReactive(input$import, {paste0(input$dates[1], "::", input$dates[2])})
  
# Import & Format Data -------------------------------------------------------------
symbol.env <- new.env()

symbols <- eventReactive(input$import, {trimws(spl(toupper(input$symbols)))})

all.stock.data<- reactive({
  data <- new.env()
  
  
  for (s in symbols()){
    if (is.null(symbol.env[[s]]))
      tryCatch({
        symbol.env[[s]] = getSymbols(s, src="yahoo", auto.assign = FALSE)
      }, error = function(e) { stop(paste('Problem getting prices for',s)) })
    
    data[[s]] = symbol.env[[s]]
    data[[s]] = adjustOHLC(data[[s]], use.Adjusted = TRUE)
    
    data[[paste0(s,"R")]] = Return.calculate(data[[s]], method = "log")[-1, ]
  }
  data
})


    # Select Stock Data For Portfolio Creation -------------------------------------------------------

    select.ohlc <- reactive({
          stock.data = all.stock.data()
          port.data = new.env()
          period = input$period
          
          for (s in symbols()){
            period.stock = to.period(stock.data[[s]], period = period, indexAt = 'startof')
            stock = window(period.stock, start = start_date(), end = end_date())
            port.data[[s]] = stock
            port.data[[paste0(s,"R")]] = Return.calculate(stock, method = "log")[-1,]
          }
          
          port.data
          
        })  

    port.data <- reactive({
        stock.data = all.stock.data()
        port.data = new.env()
        period = input$period
        
        for (s in symbols()){
          period.stock = to.period(stock.data[[s]], period = period, indexAt = 'startof')
          stock = window(period.stock, start = start_date(), end = end_date())
          port.data[[s]] = stock[,6]
        }
        
        port.data = as.data.frame(as.list(port.data))
        colnames(port.data) = symbols()
        
        port.data
      })
    
    port.return.data <- reactive({
      port.data = port.data()
      Return.calculate(port.data, method = "log")[-1,]
    })
    
    
    # Stock Allocation Data --------------------------------------------
    
    alloc.data <- eventReactive(input$generate, {
      stock.data = all.stock.data()
      adj.data = new.env()
      period = input$period
      
       start = input$portrange[1]
       end = input$portrange[2]
      
      for (s in symbols()){
        period.stock = to.period(stock.data[[s]], period = period, indexAt = 'startof')
        stock = window(period.stock, start = start, end = Sys.Date())
        adj.data[[s]] = stock[,6] 
      }
      
      alloc.data = as.data.frame(as.list(adj.data))
        colnames(alloc.data) = symbols()
        
      alloc.data
    })
    
    alloc.return.data <- reactive({
      adj.data = alloc.data()
      Return.calculate(adj.data, method = "log")[-1,]
    })
    
    
    
    # Portfolio Allocation Data -----------------------------------------------
    
    port.alloc <- eventReactive(input$generate, {
      portfolio = portfolio()
      weights = getWeights(portfolio)
      
      initial = input$initialvalue
      AD = alloc.return.data()
      
      port.alloc = as.data.frame(AD*0)
      
      port.initial = initial * weights
      
      port.alloc[1,] = port.initial  
      
      if (isTRUE(input$rebalance)){
        port.alloc$Portfolio = rowSums(port.alloc[,])
        
        for (i in 2:nrow(port.alloc))
        {
          port.ret = sum(AD[i,]*weights)
          
          nextport = port.alloc$Portfolio[i-1]*(1+port.ret)
          nextalloc = nextport*weights
          port.alloc[i,] = c(nextalloc,nextport)
        }
      }
      else{
        for (i in 2:nrow(port.alloc))
        {
          nextalloc = port.alloc[i-1, ]*(1+AD[i,])
          port.alloc[i,] = nextalloc
        }
        
        port.alloc$Portfolio = rowSums(port.alloc[,])
        
      }
      
      order = zoo::as.Date(row.names(port.alloc))
      
      round(xts(port.alloc, order.by = order),3)
     
    })
    
    
    # Just Portfolio Values ---------------------------------------------------
    
    port.values <- eventReactive(input$generate, {
      
      start = input$portrange[1]
      end = input$portrange[2]
      
      port.data = window(port.alloc(), start = start, end = end)
      
      port.data$Portfolio
      
    })
    
    port.returns <- eventReactive(input$generate, {
      port.returns = Return.calculate(port.values(), method = 'log')[-1,]
      
      port.returns
    })
    
    
# Forecast Training Set ---------------------------------------------------
training.values <- reactive({
  start = input$trainingrange[1]
  end = input$trainingrange[2]
  
  training.data = window(port.alloc(), start = start, end = end)
  
  training.data$Portfolio
  
})
    
training.returns <- reactive({
  training.returns = Return.calculate(training.values(), method = 'log')[-1,]
  
  training.returns
})

fperiod <- reactive({
  length(training.returns())
})

# Market Data -------------------------------------------------------------

    market.data <- eventReactive(input$generate, {
  start = input$portrange[1]
  end = input$portrange[2]
  
  getSymbols("^GSPC", src = "yahoo", from = start, to = end)
  market.adj = GSPC[,6]
  
  market.r = Return.calculate(market.adj, method = 'log')[-1,]
  
  market.r
  
  
})


# Fama French Data ----------------------------------------------------------





# Switch Tabs (Stock & Portfolio) -----------------------------------------
  observeEvent(input$import,{
    if(length(symbols())>= 2)
    updateTabItems(session, "menu", "stock")
  })

observeEvent(input$import2,{
  updateTabItems(session, "menu", "indicators")
})


# Stock Charts ------------------------------------------------------------

  
  TAInput <- reactive({
    if (input$TA == 0)
      return("NULL")

    tas <- isolate({c(input$ta_vol, input$ta_sma, input$ta_ema,
                      input$ta_wma,input$ta_bb, input$ta_momentum)})
    funcs <- c(addVo(), addSMA(), addEMA(), addWMA(),
               addBBands(), addMomentum())

    if (any(tas)) funcs[tas]
    else "NULL"
  })


  
  
output$stock_graph <- renderPlot({

    stock = as.character(input$stockindex)
    data = all.stock.data()
    
    if(input$isreturns == "TRUE")
      stock = paste0(stock,"R")
    
    chartSeries(get(stock, data),
                name = input$stockindex,
                theme = "white",
                type = input$chart_type,
                subset = paste0(start_date(),"::",end_date()),
                log.scale = input$log_y,
                TA = TAInput()
    )
    

  })
  

# Portfolio Simulation ----------------------------------------------------
  
  # Create Portfolio Spec
  
  portSpec <- reactive({
    
    return = input$return
    risk = input$risk
    rfr = input$riskfreerate
    nfp = input$numpoints
    long = input$long
    min_max = input$minrisk
    
    
    long_short = 'solveRquadprog'
    
    if(isTRUE(long)){
       long_short = 'solveRshortExact'
    }
    
    portfolioSpec(
      model = list(
        type = "MV", optimize = min_max,           
        estimator = "covEstimator", tailRisk = list(),
        params = list(alpha = 0.05)),
      portfolio = list(
        weights = NULL, targetReturn = return,
        targetRisk = risk, riskFreeRate = rfr, nFrontierPoints = nfp,
        status = NA),
      optim = list(
        solver = long_short, 
        objective = c("portfolioObjective", "portfolioReturn", "portfolioRisk"),
        options = list(meq = 2), control = list(), trace = FALSE),
      messages = list(
        messages = FALSE, note = ""),
      ampl = list(
        ampl = FALSE, project = "ampl", solver = "ipopt",
        protocol = FALSE, trace = FALSE)
    )
  })

  constraint <- reactive({
    if(isTRUE(input$long))
      'Short'
    else
      'LongOnly'
  })

  portfolio <- eventReactive(input$generate, {
    model = input$model
    data = as.timeSeries(port.return.data())
    portSpec = portSpec()
    long = input$long
    constraint = constraint()
    
    if (model == "efficient")
      efficientPortfolio(data, spec = portSpec, constraints = constraint)

    else if (model == "tangency")
      tangencyPortfolio(data, spec = portSpec, constraints = constraint)
    
    else if (model == "minvar")
      minvariancePortfolio(data, spec = portSpec, constraints = constraint)
    
    else if (model == "maxreturn")
      maxreturnPortfolio(data, spec = portSpec, constraints = constraint)
    
    else if (model == "minrisk")
      minriskPortfolio(data, spec = portSpec, constraints = constraint)
    
  })
  
  portfolio_frontier <- eventReactive(input$generate, {
    data = as.timeSeries(port.return.data())
    portSpec = portSpec()
    long = input$long
    constraint = constraint()
    
    portfolioFrontier(data, spec = portSpec, constraints = constraint)
  
  })
  

# CAPM --------------------------------------------------------------------


  # One Factor --------------------------------------------------------------
  one_capm <- reactive({
    returns = port.returns()
    
    market = market.data()
    rfr = rfr.data()
    RP = returns - rfr
    MRP = market - rfr
    
    capm = lm(Returns ~ MRP)
    
    summary = summary(capm)
    
    summary
  })
  
  coefficients <- reactive({
    capm = one_capm()
    
    beta = capm.summary$coefficients
    
    beta
  })
  

  # Fama French -------------------------------------------------------------
  
# Naive
  naive <- reactive({
    data = port.returns()
    h = fperiod()
    
    naive = rwf(data, drift = TRUE, h = h)
    
    naive
  })
  
  naive.acc <- reactive({
    accuracy(naive()$mean, training.returns())
  })
  
  output$`naive-plot` <- renderPlot({
    plot(naive(), xlab = "Time")
    
  })
  
  output$`naive-forecast` <- renderPrint({
    summary(naive())
  })
  
  output$`naive-accuracy` <- renderPrint({
    naive.acc()
  })
  
  
  
  
# MA 
  
  ma <- reactive({
    data = port.returns()
    h = fperiod()
    
    sma = forecast::ma(data, order = input$`ma-period`)
    sma = suppressWarnings(forecast(sma, h = h))
    
    sma
  })
  
  ma.acc <- reactive({
    accuracy(ma()$mean, training.returns())
  })
  
  output$`ma-plot` <- renderPlot({
    plot(ma(), xlab = "Time", ylab = "Returns")

  })

  output$`ma-forecast` <- renderPrint({
    summary(ma())
  })
  

  output$`ma-accuracy` <- renderPrint({
    ma.acc()
  })

 
# ES
  ses <- reactive({
    data = port.returns()
    h = fperiod()
    
    ses = forecast::ses(data, h = h)
    
    ses
  })
  
  ses.acc <- reactive({
    accuracy(ses()$mean, training.returns())
  })
  
  output$`es-plot` <- renderPlot({
    plot(ses(), xlab = "Time", ylab = "Returns")
    
  })
  
  output$`es-forecast` <- renderPrint({
    summary(ses())
  })
  
  output$`es-accuracy` <- renderPrint({
    ses.acc()
  })
  
# Holt
  holt <- reactive({
    data = port.returns()
    h = fperiod()
    
    holt = forecast::holt(data, h = h)
    
    holt
  })
  
  holt.acc <- reactive({
    accuracy(holt()$mean, training.returns())
  })
  
  output$`h-plot` <- renderPlot({
    plot(holt(), xlab = "Time", ylab = "Returns")
    
  })
  
  output$`h-forecast` <- renderPrint({
    summary(holt())
  })
  
  output$`h-accuracy` <- renderPrint({
    holt.acc()
  })


# Holt Winters
  holtw <- reactive({
    data = port.returns()
    h = fperiod()
    
    holtw = HoltWinters(data, beta = TRUE, gamma = FALSE)
    holtw = forecast::forecast(holtw, h = h)
    
    holtw
  })
  
  holtw.acc <- reactive({
    accuracy(holtw()$mean, training.returns())
  })
  
  output$`hw-plot` <- renderPlot({
    plot(holtw(), xlab = "Time", ylab = "Returns")
    
  })
  
  output$`hw-forecast` <- renderPrint({
    summary(holtw())
  })
  
  output$`hw-accuracy` <- renderPrint({
    holtw.acc()
  })

# ARIMA
  diff.index <- reactiveVal(0)
  observeEvent(input$difference,{
    new = min(diff.index() + 1,2)
    diff.index(new)
  })
  observeEvent(input$undifference,{
    new = max(diff.index() - 1,0)
    diff.index(new)
  })
  observeEvent(input$import,{
    new = 0
    diff.index(new)
  })
  
  output$`diff-text` <- renderText({
    index = min(diff.index(), 2)
    
    paste('Times Difference:', index)
  })
  output$`adf-test` <- renderPrint({
    if (diff.index() == 0)
      adf.test(port.returns())

    else if (diff.index() == 1)
      adf.test(diff.xts(port.returns(), difference = 1)[-1,])
    
    else 
      adf.test(diff.xts(port.returns(), difference = 2)[-1:-2,])
  })
  
  output$`arch-test` <- renderPrint({
      ArchTest(port.returns())
  })
  
  output$ggts <- renderPlot({
    if (diff.index() == 0)
      ggtsdisplay(port.returns(), main = "Portfolio Returns")
    
    else if (diff.index() == 1)
      ggtsdisplay(diff.xts(port.returns(), difference = 1)[-1,])
    
    else 
      ggtsdisplay(diff.xts(port.returns(), difference = 2)[-1:-2,])
  })
  
  
  arima.order <- reactive({
    if(input$arimafit == 0){
      p = 0
      d = 0
      q = 0
    }
    else {
      p = input$p
      d = input$d
      q = input$q
    }
    
    c(p,d,q)
      
  })
  
  arima <- reactive({
    stats::arima(port.returns(), order = arima.order())
  })
  
  output$`arima-model` <- renderPrint({
    arima()
  })
  
  output$ggts.resid <- renderPlot({
    ggtsdisplay(arima()$residuals, main = "ARIMA Residuals")
    
  })
  
  arima.f <- reactive({
    h = fperiod()
    arima = forecast::forecast(arima(), h = h)
    
    arima
  })
  
  output$`arima-plot` <- renderPlot({
    plot(arima.f(), xlab = "Time", ylab = "Returns")
    
  })
  
  output$`arima-forecast` <- renderPrint({
    summary(arima.f())
  })
  
  arima.acc <- reactive({
    accuracy(arima.f()$mean, training.returns())
  })
  
  output$`arima-accuracy` <- renderPrint({
    arima.acc()
  })
  
# Accuracy Summary
  
  output$`acc-table` <-  renderDataTable({
    
    
    forecast.acc = rbind(naive.acc(), ma.acc(), ses.acc(), holt.acc(), holtw.acc(), arima.acc())
      rownames(forecast.acc) = c("NAIVE","MA","SES", "Holt","Holt-Winters", "ARIMA")
      
    datatable(round(forecast.acc,3), width = '100%', height = '100%')
    
  })
  
  # Grab Country Name, ISO & Dates -------------------------------------------------
  
  country <- eventReactive(input$import2, {input$country})
  
  iso <- reactive({as.character(iso_data[country(),])})
  
  start_date1 <- eventReactive(input$import2, {input$dates2[1]})
  end_date1 <- eventReactive(input$import2, {input$dates2[2]})
  
  start_year <- reactive({substr(start_date1(),1,4)})
  end_year <- reactive({substr(end_date1(),1,4)})
  
  range_year <- reactive({
    
    start = as.numeric(start_year())
    end = as.numeric(end_year())
    
    as.character(seq(start, end, 1))
    
  })
  
  
  # Import Data -------------------------------------------------------------
  
  # Economic Indicators
  econ_data <- reactive({
    
    i = iso()
    data = new.env()
    
    if(input$unit_ind == "dollar")
      t = NULL
    else
      t = input$unit_ind
    
    #Keys
    gdp = paste0("WWDI/",i,"_NY_GDP_MKTP_CD")
    gdppc = paste0("WWDI/",i,"_NY_GDP_PCAP_CD")
    
    gni = paste0("WWDI/",i,"_NY_GNP_MKTP_CD")
    gnipc = paste0("WWDI/",i,"_NY_GNP_PCAP_CD")
    
    cpi = paste0("WWDI/",i,"_FP_CPI_TOTL")
    
    unemp = paste0("WWDI/",i,"_SL_UEM_TOTL_ZS")
    
    #Get Data
    data[["GDP"]] = Quandl(gdp, type = "xts", transform = t, collapse = "annual", start_date = start_date1(), end_date = end_date1())
    data[["GDPPC"]] = Quandl(gdppc, type = "xts", transform = t, collapse = "annual", start_date = start_date1(), end_date = end_date1())
    
    data[["GNI"]] = Quandl(gni, type = "xts", transform = t, collapse = "annual", start_date = start_date1(), end_date = end_date1())
    data[["GNIPC"]] = Quandl(gnipc, type = "xts", transform = t, collapse = "annual", start_date = start_date1(), end_date = end_date1())
    
    data[["CPI"]] = Quandl(cpi, type = "xts", transform = t, collapse = "annual", start_date = start_date1(), end_date = end_date1())
    
    data[["Unemployment"]] = Quandl(unemp, type = "xts", transform = t, collapse = "annual", start_date = start_date1(), end_date = end_date1())
    
    data = as.data.frame(as.list(data))
    
    indices = input$add_ind
    
    data = data[indices]
    
  })
  
  # Trade Accounts
  
  # Balance of Payments
  bop_data <- reactive({
    
    i = iso()
    data = new.env()
    
    if(input$unit_bop == "dollar")
      t = NULL
    else
      t = input$unit_bop
    
    #Keys
    bop = paste0("WWDI/",i,"_BN_GSR_GNFS_CD")
    ca = paste0("WWDI/",i,"_BN_CAB_XOKA_CD")
    cap = paste0("WWDI/",i,"_BN_TRF_KOGT_CD")
    
    #Get Data
    data[["BoP"]] = Quandl(bop, type = "xts", transform = t, collapse = "annual", start_date = start_date1(), end_date = end_date1())
    data[["Current"]] = Quandl(ca, type = "xts", transform = t, collapse = "annual", start_date = start_date1(), end_date = end_date1())
    data[["Capital"]] = Quandl(cap, type = "xts", transform = t, collapse = "annual", start_date = start_date1(), end_date = end_date1())
    data[["Other"]] = data[["BoP"]] - (data[["Current"]] + data[["Capital"]])
    
    data = as.data.frame(as.list(data))
    
  })
  
  bop_pie_data <- reactive({
    bop_data = bop_data()
    range_year = range_year()
    
    account_data = t(data.frame("Current" = bop_data[["Current"]],
                                "Capital"  = bop_data[["Capital"]],
                                "Other" = bop_data[["Other"]],
                                row.names = range_year))
    
    account_data[, end_year()]
    
  })
  
  
  # Current Account
  ca_data <- reactive({
    i = iso()
    data = new.env()
    
    if(input$unit_ca == "dollar")
      t = NULL
    else
      t = input$unit_ca
    
    # Keys
    ca = paste0("WWDI/",i,"_BN_CAB_XOKA_CD")
    exports = paste0("WWDI/",i,"_BX_GSR_GNFS_CD")
    imports = paste0("WWDI/",i,"_BM_GSR_GNFS_CD")
    ni = paste0("WWDI/",i,"_BN_GSR_FCTY_CD")
    nct = paste0("WWDI/",i,"_BN_TRF_CURR_CD")
    bot = paste0("WWDI/",i,"_BN_GSR_GNFS_CD")
    
    # Get Data
    data[["Current"]] = Quandl(ca, type = "xts", transform = t, collapse = "annual", start_date = start_date1(), end_date = end_date1())
    data[["Exports"]] = Quandl(exports, type = "xts", transform = t, collapse = "annual", start_date = start_date1(), end_date = end_date1())
    data[["Imports"]] = Quandl(imports, type = "xts", transform = t, collapse = "annual", start_date = start_date1(), end_date = end_date1())
    data[["NI"]] = Quandl(ni, type = "xts", transform = t, collapse = "annual", start_date = start_date1(), end_date = end_date1())
    data[["NCT"]] = Quandl(nct, type = "xts", transform = t, collapse = "annual", start_date = start_date1(), end_date = end_date1())
    data[["BoT"]] = Quandl(bot, type = "xts", transform = t, collapse = "annual", start_date = start_date1(), end_date = end_date1())
    
    data = as.data.frame(as.list(data))
    
  })
  
  ca_pgdp_data <- reactive({
    i = iso()
    data = new.env()
    
    # Keys
    gdp = paste0("WWDI/",i,"_NY_GDP_MKTP_CD")
    
    ca = paste0("WWDI/",i,"_BN_CAB_XOKA_GD_ZS")
    exports = paste0("WWDI/",i,"_NE_EXP_GNFS_ZS")
    imports = paste0("WWDI/",i,"_NE_IMP_GNFS_ZS")
    
    ni = paste0("WWDI/",i,"_BN_GSR_FCTY_CD")
    nct = paste0("WWDI/",i,"_BN_TRF_CURR_CD")
    bot = paste0("WWDI/",i,"_BN_GSR_GNFS_CD")
    
    # Get Data
    
    data[["GDP"]] = Quandl(gdp, type = "xts", collapse = "annual", start_date = start_date1(), end_date = end_date1())
    
    data[["Current"]] = Quandl(ca, type = "xts", collapse = "annual", start_date = start_date1(), end_date = end_date1())
    data[["Exports"]] = Quandl(exports, type = "xts", collapse = "annual", start_date = start_date1(), end_date = end_date1())
    data[["Imports"]] = Quandl(imports, type = "xts", collapse = "annual", start_date = start_date1(), end_date = end_date1())
    
    data[["NI"]] = Quandl(ni, type = "xts", collapse = "annual", start_date = start_date1(), end_date = end_date1())/data[["GDP"]]
    data[["NCT"]] = Quandl(nct, type = "xts", collapse = "annual", start_date = start_date1(), end_date = end_date1())/data[["GDP"]]
    data[["BoT"]] = Quandl(bot, type = "xts", collapse = "annual", start_date = start_date1(), end_date = end_date1())/data[["GDP"]]
    
    data = as.data.frame(as.list(data))
    
  })
  
  ca_pie_data <- reactive({
    ca_data = ca_data()
    range_year = range_year()
    
    chart = input$ca_pie_option
    
    if(chart == "current")
      account_data = t(data.frame("Balance of Trade" = ca_data[["BoT"]],
                                  "Net Income"  = ca_data[["NI"]],
                                  "Net Current Transfers" = ca_data[["NCT"]],
                                  row.names = range_year))
    else
      account_data = t(data.frame("Exports" = ca_data[["Exports"]],
                                  "Imports"  = ca_data[["Imports"]],
                                  row.names = range_year))
    
    
    
    account_data[, end_year()]
    
  })
  
  
  # Savings & Investments
  savings_data <- reactive({
    
    i = iso()
    data = new.env()
    
    if(input$unit_savings == "dollar")
      t = NULL
    else
      t = input$unit_savings
    
    #Keys
    savings = paste0("WWDI/",i,"_NY_GDS_TOTL_CD")
    investments = paste0("WWDI/",i,"_NE_GDI_TOTL_CD")
    
    ca = paste0("WWDI/",i,"_BN_CAB_XOKA_CD")
    
    #Get Data
    data[["Savings"]] = Quandl(savings, type = "xts", transform = t, collapse = "annual", start_date = start_date1(), end_date = end_date1())
    data[["Investments"]] = Quandl(investments, type = "xts", transform = t, collapse = "annual", start_date = start_date1(), end_date = end_date1())
    
    data[["Current"]] = Quandl(ca, type = "xts", transform = t, collapse = "annual", start_date = start_date1(), end_date = end_date1())
    
    data[["NNSB"]] = data[["Savings"]] - data[["Investments"]]
    
    data = as.data.frame(as.list(data))
    
    indices = input$add_savings
    
    data = data[indices]
    
  })
  
  # Currency
  currency_data <- reactive({
    
    i = iso()
    data = new.env()
    
    if(input$unit_currency == "index")
      t = NULL
    else
      t = input$unit_currency
    
    #Keys
    exchange = paste0("WWDI/",i,"_PA_NUS_FCRF")
    ppp2exchange = paste0("WWDI/",i,"_PA_NUS_PPPC_RF")
    reed = paste0("WWDI/",i,"_PX_REX_REER")
    interest = paste0("WWDI/",i,"_FR_INR_RINR")
    
    #Get Data
    data[["Exchange"]] = Quandl(exchange, type = "xts", transform = t, collapse = "annual", start_date = start_date1(), end_date = end_date1())
    data[["PPP2E"]] = Quandl(ppp2exchange, type = "xts", transform = t, collapse = "annual", start_date = start_date1(), end_date = end_date1())
    data[["REED"]] = Quandl(reed, type = "xts", transform = t, collapse = "annual", start_date = start_date1(), end_date = end_date1())
    data[["Interest"]] = Quandl(interest, type = "xts", transform = t, collapse = "annual", start_date = start_date1(), end_date = end_date1())
    
    data = as.data.frame(as.list(data))
    
    indices = input$add_currency
    
    data = data[indices]
    
  })  
  
  # Tariffs
  
  tariffs_data <- reactive({
    
    i = iso()
    data = new.env()
    
    mean = input$average_tariffs
    nations = input$nations_tariffs
    
    
    if(input$unit_tariffs == "index")
      t = NULL
    else
      t = input$unit_tariffs
    
    #Keys
    all = paste0("WWDI/",i,"_TM_TAX_MRCH_",mean,"_",nations,"_ZS")
    manufactured = paste0("WWDI/",i,"_TM_TAX_MANF_",mean,"_",nations,"_ZS")
    primary = paste0("WWDI/",i,"_TM_TAX_TCOM_",mean,"_",nations,"_ZS")
    
    #Get Data
    data[["All"]] = Quandl(all, type = "xts", transform = t, collapse = "annual", start_date = start_date1(), end_date = end_date1())
    data[["Manufactured"]] = Quandl(manufactured, type = "xts", transform = t, collapse = "annual", start_date = start_date1(), end_date = end_date1())
    data[["Primary"]] = Quandl(primary, type = "xts", transform = t, collapse = "annual", start_date = start_date1(), end_date = end_date1())
    
    data = as.data.frame(as.list(data))
    
    indices = input$add_tariffs
    
    data = data[indices]
    
  })  
  
  
  # Output Titles & Graphs --------------------------------------------------
  
  # Economic Indicators
  output$indicator_header <- renderText({country()})
  output$indicator_box <- renderText({"Macroeconomic Indicators"})
  
  output$indicator_graph <- renderDygraph({
    
    points = input$point_ind
    stacked = input$stack_ind
    econ_data = econ_data()
    chart_type = input$type_ind
    
    if (chart_type == "line"){
      dygraph(econ_data) %>%
        dyRangeSelector() %>%
        dyOptions(drawPoints = points, stackedGraph = stacked, drawGrid = TRUE, pointSize = 3)
    }
    
    else {
      dygraph(econ_data) %>%
        dyBarChart() %>%
        dyRangeSelector() %>%
        dyOptions(drawPoints = points, stackedGraph = stacked, drawGrid = TRUE, pointSize = 3)
    }
  })
  
  # Trade Accounts
  # Balance of Payments
  output$bop_header <- renderText({country()})
  output$bop_box <- renderText({"Balance of Payments Breakdown"})
  
  output$bop_graph <- renderDygraph({
    
    points = input$point_bop
    stacked = input$stack_bop
    
    accounts = input$add_bop
    bop_data = bop_data()
    
    bop_data = bop_data[accounts]
    
    chart_type = input$type_bop
    
    if (chart_type == "line"){
      dygraph(bop_data) %>%
        dyRangeSelector() %>%
        dyOptions(drawPoints = points, stackedGraph = stacked, drawGrid = TRUE, pointSize = 3)
    }
    
    else {
      dygraph(bop_data) %>%
        dyBarChart() %>%
        dyRangeSelector() %>%
        dyOptions(drawPoints = points, stackedGraph = stacked, drawGrid = TRUE, pointSize = 3)
    }
  })
  
  output$bop_pie <- renderPlotly({
    data = bop_pie_data()
    
    accounts = c("Current", "Capital", "Other")
    
    pie_data = data.frame("Accounts" = accounts , "Year" = abs(data))
    
    plot_ly(pie_data, labels = ~Accounts, values = ~Year, type = "pie") %>%
      layout(title = paste("Balance of Payments Decomposition:", end_year()),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  
  
  
  
  # Current Account
  output$ca_header <- renderText({country()})
  output$ca_box <- renderText({"Current Account Breakdown"})
  
  output$ca_graph <- renderDygraph({
    
    points = input$point_ca
    stacked = input$stack_ca
    
    if(input$unit_ca == "pgdp")
      ca_data = ca_pgdp_data()
    else
      ca_data = ca_data()
    
    accounts = input$add_ca
    ca_data = ca_data[accounts]
    
    chart_type = input$type_ca
    
    if (chart_type == "line"){
      dygraph(ca_data) %>%
        dyRangeSelector() %>%
        dyOptions(drawPoints = points, stackedGraph = stacked, drawGrid = TRUE, pointSize = 3)
    }
    
    else {
      dygraph(ca_data) %>%
        dyBarChart() %>%
        dyRangeSelector() %>%
        dyOptions(drawPoints = points, stackedGraph = stacked, drawGrid = TRUE, pointSize = 3)
    }
  })
  
  output$ca_pie <- renderPlotly({
    data = ca_pie_data()
    chart = input$ca_pie_option
    
    if (chart == "current"){
      accounts = c("Balance of Trade", "Net Income", "Net Current Transfers")
      trend = "Current Account"}
    
    else {
      accounts = c("Exports", "Imports")
      trend = "Balance of Trade"}
    
    pie_data = data.frame("Accounts" = accounts , "Year" = abs(data))
    
    plot_ly(pie_data, labels = ~Accounts, values = ~Year, type = "pie") %>%
      layout(title = paste(trend,"Decomposition:", end_year()),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  
  # Savings & Investments
  output$savings_header <- renderText({country()})
  output$savings_box <- renderText({"Saving & Investment Accounts"})
  output$savings_graph <- renderDygraph({
    
    points = input$point_savings
    stacked = input$stack_savings
    savings_data = savings_data()
    chart_type = input$type_savings
    
    if (chart_type == "line"){
      dygraph(savings_data) %>%
        dyRangeSelector() %>%
        dyOptions(drawPoints = points, stackedGraph = stacked, drawGrid = TRUE, pointSize = 3)
    }
    
    else {
      dygraph(savings_data) %>%
        dyBarChart() %>%
        dyRangeSelector() %>%
        dyOptions(drawPoints = points, stackedGraph = stacked, drawGrid = TRUE, pointSize = 3)
    }
  })
  
  # Currency
  output$currency_header <- renderText({country()})
  output$currency_box <- renderText({"Currency Indices"})
  
  output$currency_graph <- renderDygraph({
    
    points = input$point_currency
    stacked = input$stack_currency
    step = input$step_currency
    currency_data = currency_data()
    
    dygraph(currency_data) %>%
      dyRangeSelector() %>%
      dyOptions(drawPoints = points, stackedGraph = stacked, drawGrid = TRUE, pointSize = 3, stepPlot = step)
    
  })
  
  # Tariffs
  output$tariffs_header <- renderText({country()})
  output$tariffs_box <- renderText({"Tariffs Profile"})
  
  output$tariffs_graph <- renderDygraph({
    
    points = input$point_tariffs
    stacked = input$stack_tariffs
    step = input$step_tariffs
    tariffs_data = tariffs_data()
    
    dygraph(tariffs_data) %>%
      dyRangeSelector() %>%
      dyOptions(drawPoints = points, stackedGraph = stacked, drawGrid = TRUE, pointSize = 3, stepPlot = step)
    
  })


  
})
