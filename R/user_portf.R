hideshowcalc <- function(input, output, session,nrowuserportf) {
  observe(
    if (nrowuserportf()>=2){
      shinyjs::show("bcalcef")
      shinyjs::show("accordion")
      shinyjs::show("ShowInPu")
      shinyjs::show("dateRange")
      shinyjs::show("curr1")
    }
    else{
      shinyjs::hide("bcalcef")
      shinyjs::hide("accordion")
      shinyjs::hide("ShowInPu")
      shinyjs::hide("dateRange")
      shinyjs::hide("curr1")
    })
}

stocksearchInputUI <- function(id) {
  textInput(NS(id, "stocksearch"), 'Stock search:', value = "",placeholder ="e.g. Tesla, Apple Microsoft")
}

displayresultsUI <- function(id) {
  DT::dataTableOutput(NS(id, "tickersearchresult"))
}

stocksearchInputServer <-function(id){
  moduleServer(id, function(input, output, session) {
    reactive(searchresult(SAMPLE_ticker,input$stocksearch))
  })}

searchresultstableServer <-function(id,results){
  moduleServer(id, function(input, output, session) {
    output$tickersearchresult <- DT::renderDT(  
      if (is.null(results()))
      {NULL} 
      else if (nrow(results())==0)
      {NULL} 
      else  
      {results()},
      server = FALSE, escape = FALSE, selection = 'none',options = list(dom = 't'),rownames = FALSE)
  })}

user_portf <- function(portfo){
  return_p<-portfo["meanReturns"]
  Cova<-portfo[-c(1:6)]
  rel_w<-portfo["rel_weight"]
  mean_return = as.numeric(matrix(t(rel_w)))%*%matrix(t(return_p))
  vola = as.numeric(matrix(t(rel_w)))%*%t(t(Cova))%*%as.numeric(t(matrix(t(rel_w))))
  vola <- sqrt(vola)*sqrt(252)
  return(list("mean_return"= mean_return, "vola"=vola))
}

shinyInput_col <- function(FUN, len, id, ...) {
  
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  inputs
  
}
shinyInput_row <- function(FUN, i, id, ...) {
  inputs <- as.character(FUN(paste0(id, i), ...))
}
searchresult<-function(search_table, searchterm){
  res<- search_table %>%
    filter(grepl(searchterm, ticker)|grepl(searchterm, description)) %>% 
    head(10) %>% 
    select(!(collection))%>%
    mutate(Add = shinyInput_col(actionButton, nrow(.), 'button_', class="btn_addtoportfolio", label = img (src="images/add.svg", width="20", height="20"), onclick = 'Shiny.setInputValue(\"select_button\",  this.id,{priority: \"event\"})'))
  return(res)
}

addtoportfolio<-function(portsymb, selected_Ticker){
  res<-portsymb %>%
    add_row(Ticker = selected_Ticker,
            Weight = paste0('<input type="number" id=weight',gsub("\\^","",selected_Ticker),' value=',1,' name="weight" size="4" />'),
            Delete =shinyInput_row(actionButton, selected_Ticker, 'delbutton_', 
                                   class="btn_delfromportfolio",
                                   label = img (src="images/remove.svg", 
                                                width="20", height="20"),
                                   onclick = 'Shiny.setInputValue(\"del_button\",  this.id,{priority: \"event\"})')
    )
  
  return(res)
  
}

addtoPortfolioPositionServer <-function(id,portsymb,nameweights,select_button,User_search_results){
  moduleServer(id, function(input, output, session) {
    if (nrow(portsymb)>0){
      portsymb$PortfolioWeightValue <- reactiveValuesToList(input)[unlist(nameweights)]
      portsymb$Weight <- mapply(function(x,y){gsub('value=\\d', paste0("value=",x), y)},x=portsymb$PortfolioWeightValue,y=portsymb$Weight)
      session$sendCustomMessage('unbind-DT', 'portsmbs')
    }
    selectedRow <- as.numeric(strsplit(select_button, "_")[[1]][2])
    if (length(which(portsymb[,1] == (User_search_results()[selectedRow,1]))) == 0){
      shinyjs::show("selectedPort")
      shinyjs::show("line1")
      portsymb <- isolate(addtoportfolio(portsymb,User_search_results()[selectedRow,1]))
      nameweights <-isolate(append(nameweights, paste0('weight',gsub("\\^","",User_search_results()[selectedRow,1]))))
    }
    else{
      shinyalert("Oops!", "The selected position is already in your portfolio!", type = "error")
    }
    return(list(portsymb, nameweights))
  })
}












testUsersearch <- function() {
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(stocksearchInputUI("User_search")),
      displayresultsUI("results")
      #mainPanel(DT::dataTableOutput('tickersearchresult'))
    )
  )
  server <- function(input, output, session) {
    
    User_search_results <-stocksearchInputServer("User_search")
    searchresultstableServer("results",User_search_results)
  }
  shinyApp(ui, server)
}

#collection <-c('PNK', 'NMS','NYQ', 'NMS')
#ticker <-c('OEDV', 'AAPL','BAC', 'AMZN')
#description <-c('Osage Exploration and Development, Inc.', 'Apple Inc.','Bank of America Corporation', 'Amazon.com, Inc.')

#SAMPLE_ticker <- data.frame(collection,ticker,description)
#testUsersearch()
