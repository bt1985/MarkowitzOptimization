library(shiny)
library(DT)
library(shinydashboard)
library(shinyjs)
library(dashboardthemes)
library(shinycssloaders)
library(shinyalert)
library(plotly)
library(xts)
library(stringr)
library(httr)
library(dplyr)
library(plotly)
library(zeallot)

source("R/const.R")
source("R/user_portf.R")
source("R/tab_AddInfo.R")
source("R/tab_Dashb.R")
source("R/histstockandfx.R")
source("R/portf_metrics.R")

ui <- dashboardPage(
  dashboardHeader(title = ""),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Start", tabName = "dashboard", icon=icon("play"), selected = T),
      menuItem("Additional Information", tabName = "addInfo", icon=icon("info"))
    )
  ),
  dashboardBody(
    cssUI(),
    useShinyalert(),
    useShinyjs(),
    jsscriptscrollsmoothUI(),
    jsscriptrefreshDT_UI(),
    tags$div(class="drumherum",  
             tabItems( 
               Dashboard_UI("dashboard"),
               additionalInfoUI("addInfo") # Tabitem "addInfo"
           )      
    )
  )
)

server <- function(input, output, session) {
  v <- reactiveValues(newsmb = NULL)
  v$portsymb <- data.frame(Ticker=character(0),Weight=character(0),Delete=character(0))
  v$nameweights <-list()
  
  #Show calc button and options only if two or more stocks are selected
  hideshowcalc(input, output, session,reactive(nrow(v$portsymb)))
  
  #User Stock search results
  User_search_results <-stocksearchInputServer("User_search")

  #User Stock selection 
  observeEvent(input$select_button, {
    if (nrow(v$portsymb)>0){
      v$portsymb$PortfolioWeightValue <- reactiveValuesToList(input)[unlist(v$nameweights)]
      v$portsymb$Weight <- mapply(function(x,y){gsub('value=\\d', paste0("value=",x), y)},x=v$portsymb$PortfolioWeightValue,y=v$portsymb$Weight)
      session$sendCustomMessage('unbind-DT', 'portsmbs')
    }
    selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    if (length(which(v$portsymb[,1] == (User_search_results()[selectedRow,1]))) == 0){
      shinyjs::show("selectedPort")
      shinyjs::show("line1")
      v$portsymb <- isolate(addtoportfolio(v$portsymb,User_search_results()[selectedRow,1]))
      v$nameweights <-isolate(append(v$nameweights, paste0('weight',gsub("\\^","",User_search_results()[selectedRow,1]))))
    }
    else{
      shinyalert("Oops!", "The selected position is already in your portfolio!", type = "error")
    }
  })
  
  #Delete portfolio position
  observeEvent(input$del_button, {
    session$sendCustomMessage('unbind-DT', 'portsmbs')
    selectedRow_del <- strsplit(input$del_button, "_")[[1]][2]
    v$nameweights <-v$nameweights[v$nameweights %in% paste0("weight",gsub("\\^","",selectedRow_del)) == FALSE]
    v$portsymb <- v$portsymb[-which(v$portsymb$Ticker == selectedRow_del),]
  } )
  
  #Optimize portfolio 
  observeEvent(input$bcalcef, {
    session$sendCustomMessage('unbind-DT', 'portsmbs')
    v$portsymb$PortfolioWeightValue <- reactiveValuesToList(input)[unlist(v$nameweights)]
    v$portsymb$Weight <- mapply(function(x,y){gsub('value=\\d', paste0("value=",x), y)},x=v$portsymb$PortfolioWeightValue,y=v$portsymb$Weight)
    v$portsymb_clean <- gsub("\\^","",v$portsymb[,1]) %>% isolate()
    v$stocks<-histstockdataServer("idbla",v$portsymb[,1],input$dateRange)
    
    #Check all Positions
    if (prod(v$portsymb_clean %in% colnames(v$stocks)) == 1){
      
      #show output
      v$load <- showoutputServer(input, output, session,input$ShowInPu)
      
      #FX Convcersion
      v$stocks_fx<-histfxdataServer("FX_conversion",input$curr1,input$dateRange,v$stocks)  
      
      #Log returns,meanreturn, vola, cov, 
      c(v$stock_returns, v$returns_xts, v$meanReturns, v$stock_cov, vola, v$correlation, tickers,v$Stocksdata) %<-% calclogreturnsServer("calclogreturns",v$stocks_fx)
      
      #calc OptProtf
      v$effFront<-calceffFrontServer("calcefffront",v$stock_returns)
      
      #init PieChart maxsharperatio
      v$pieEffFront<-donutportfolioServer("piemaxSharpeRatio",v$effFront,v$portsymb_clean,max(v$effFront$SharpeRatio))

      # User_Portf auf in Efficent Frontier eintragen
      v$portsymb_weighted <- unlist(reactiveValuesToList(input)[unlist(v$nameweights)])
      c(v$user_p, v$portsymb_weighted) %<-% userportoneffFrontServer("userpoerteffFront",v$portsymb_weighted,v$Stocksdata,v$stock_cov)
      
      # Daten Scatterplot
      c(v$d, v$dx, v$dy) %<-% scatterPlotServer("scatterinit",first(as.character(v$portsymb_weighted[,1])),last(as.character(v$portsymb_weighted[,1])),v$stock_returns)
      }
    else{shinyjs::alert(paste0("For ",paste(v$portsymb_clean[!v$portsymb_clean %in% colnames(v$stocks)],collapse=", ")," is no stock data available. Please choose another ticker."))}
  })
  
  #Data PieChart onclick effront
  observeEvent(event_data("plotly_click", source = "EffFront"),{
    v$SharpeRatio<-event_data("plotly_click", source = "EffFront")[["customdata"]]
    v$pieEffFront<-donutportfolioServer("piemaxSharpeRatio",v$effFront,v$portsymb_clean,round(v$SharpeRatio,9))
  })
  
  # Data Scatterplot onclick Heatmap
  observeEvent(event_data("plotly_click", source = "Heat_Map"),{
    v$click_heat_m <-event_data("plotly_click", source = "Heat_Map")
    c(v$d, v$dx, v$dy) %<-% scatterPlotServer("scatterinit",v$click_heat_m[["x"]],v$click_heat_m[["y"]],v$stock_returns)
  })
  
  ## Output generation   
  #Output Suchergebnisse
  searchresultstableServer("usersearchresults",User_search_results)
  #Output Userportf
  output$portsmbs = DT::renderDT(
    if (is.null(v$portsymb))
    {NULL} 
    else if (nrow(v$portsymb)==0)
    {NULL} 
    else
      v$portsymb %>%
      select(Ticker, Weight, Delete),
      selection = 'none', rownames = FALSE,
      server = TRUE,
      escape = FALSE, options = list(pageLength = nrow(v$portsymb),
                                     preDrawCallback = JS('function() {Shiny.unbindAll(this.api().table().node());}'),
                                     drawCallback = JS('function() {Shiny.bindAll(this.api().table().node());}'),
                                     dom = 't'))
  
  #Output EffFront
  effFrontPlotServer("plotEffront",reactive(v$Stocksdata),reactive(v$effFront),reactive(v$user_p),reactive(v$load))
  
  #Output PieChart 
  DonutPortPlotServer("plotDonutPort",reactive(v$pieEffFront),reactive(v$load))

  #Output Heatmap p2$CovMa
  HeatmapPlotServer("plotHeatmap",reactive(v$correlation),reactive(v$load))

  #Output Scatterplot
  ScatterPlotServer("plotscatter",reactive(v$d),reactive(v$dx),reactive(v$dy))

  #Download 
  DownloaddataServer("download","HistoricalDataorig", "downloadStocksData", v$stocks)
  DownloaddataServer("download","Returns", "downloadStocksData_return", v$stock_returns)
  DownloaddataServer("download","Drift", "downloadStocksData_drift", v$meanReturns)
  DownloaddataServer("download","Cov_Matrix", "downloadStocksData_sd", v$stock_cov)
  DownloaddataServer("download","Efficent_Frontier", "downloadstock_effFront", v$effFront)
  
}# End Server

# Run the app
shinyApp(ui, server)