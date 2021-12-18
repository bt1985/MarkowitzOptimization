calclogreturnsServer <-function(id,stockdata){
  moduleServer(id, function(input, output, session) {

    pc_json_shareprice <- list(shareprices = stockdata)
    stock_returns<-POST(paste0(str_serv,"/log_rendite"), body = pc_json_shareprice, encode = "json") %>%
                               content(as="parsed") %>%
                               bind_rows()%>%
                               na.omit()
    returns_xts<-xts(select(stock_returns,-one_of("time")), order.by =as.Date(stock_returns$time))
    meanReturns<-colMeans((returns_xts))*252
    stock_cov<-cov((returns_xts))
    vola <- sqrt(diag(stock_cov))*sqrt(252)
    correlation <- cor(returns_xts)
    tickers <- colnames(returns_xts)
    Stocksdata <- data.frame(
      meanReturns,
      vola,
      tickers)
    return(list(stock_returns,returns_xts,meanReturns,stock_cov,vola, correlation,tickers,Stocksdata))
  })
}

calceffFrontServer <-function(id,stock_returns){
  moduleServer(id, function(input, output, session) {
  pc_json_returns <- list(returns = stock_returns)
  p2 <- POST(paste0(str_serv,"/optimize_stocks_p2?npo=",opt_steps), body = pc_json_returns, encode = "json") %>%
                    content(as="parsed") %>%
                    bind_rows()
  
  effFront <-as.data.frame(p2)
  effFront <- effFront %>%
    rename_at(vars(starts_with("w.")), 
              funs(str_replace(., "w.", ""))) 
  effFront <- effFront %>%
      mutate(mean = mean*252) %>%
      mutate(StdDev = StdDev*sqrt(252)) 
  effFront["SharpeRatio"] <- as.vector(effFront["mean"]/effFront["StdDev"])
  return(effFront)
  })
}

donutportfolioServer <-function(id,effFront,portsymb_clean,position){
  moduleServer(id, function(input, output, session) {
    pieEffFront<- effFront %>%
        dplyr::filter(round(SharpeRatio,9) == round(position,9)) %>%
        gather(Weight, Ratio, as.character(portsymb_clean), factor_key=TRUE) %>%
        mutate(Weight = as.character(Weight))%>%
        arrange(Weight)
    return(pieEffFront)
  })
}

userportoneffFrontServer <-function(id,portsymb_weighted,Stocksdata,stock_cov){
  moduleServer(id, function(input, output, session) {
    
    portsymb_weighted <-cbind(portsymb_weighted,gsub("weight","",rownames(portsymb_weighted)))
    portsymb_weighted <-cbind(portsymb_weighted,gsub("weight","",rownames(portsymb_weighted)))
    portsymb_weighted <-cbind(portsymb_weighted,as.numeric(portsymb_weighted[,1])/sum(as.numeric(portsymb_weighted[,1])))
    colnames(portsymb_weighted)<-c("abs_weight","tickers","rel_weight")
    
    portsymb_weighted<-merge(Stocksdata, portsymb_weighted, by = 'tickers')
    row.names(portsymb_weighted)<-t(portsymb_weighted["tickers"])
    portsymb_weighted<-merge(portsymb_weighted,stock_cov,by=0)
    portsymb_weighted<-portsymb_weighted[match(colnames(portsymb_weighted[-c(1:6)]), portsymb_weighted$Row.names),]
    user_p<-user_portf(portsymb_weighted)
    return(list(user_p, portsymb_weighted))
  })
}
scatterPlotServer <-function(id,dx,dy,stock_returns){
  moduleServer(id, function(input, output, session) {
    vars <- c(dx,dy)
    d <- setNames(as.data.frame(coredata(stock_returns[, vars])),c("x", "y"))
    yhat <- fitted(lm(y ~ x, data = d))
    yhat <- setNames(as.data.frame(yhat), "Estimate")
    time <- setNames(as.data.frame(stock_returns["time"]),"time")
    d <-cbind(d, yhat, time)
    #dx <- first(as.character(portsymb_weighted[,1]))
    #dy <- 
    return(list(d, dx, dy))
  })
}

effFrontPlotServer <- function(id,Stocksdata,effFront,user_p,load){
  moduleServer(id, function(input, output, session) {
    output$EffFront<-renderPlotly({
      if (is.null(load())) return(NULL)
        plot_ly(source = "EffFront") %>% 
        add_data(Stocksdata()) %>% 
        add_trace(x = ~vola, y = ~meanReturns, 
                  mode = "markers", type = "scatter", legendgroup = 'Single Stocks', 
                  name =  ~tickers,  marker = list(color = ~tickers, size = 15,  line = list(color = 'rgb(231, 99, 250)',width = 1)), 
                  hovertemplate = paste(
                    'Vola: %{x:.2%}<br>',
                    'Mean return:     %{y:.2%}<br>'
                  )) %>%  
        add_data(effFront()) %>%
        add_trace(x = ~StdDev, y = ~mean, color = ~SharpeRatio, mode = "lines+markers", customdata = ~SharpeRatio,
                  hovertemplate = paste(
                    'Vola::  %{x:.2%}<br>',
                    'Mean return:      %{y:.2%}<br>',
                    'SharpeRatio: %{customdata:.2f}<br>',
                    '<extra></extra>'
                  ),
                  type = "scatter", name = "Efficient Frontier") %>%
        hide_colorbar() %>%
        add_data(user_p()) %>%
        add_trace(x = ~vola, y = ~mean_return,
                  mode = "markers", type = "scatter",
                  name =  "Your Portfolio",  marker = list(color = "black", size = 15,
                                                           line = list(color = 'rgb(231, 99, 250)',width = 1)),
                  hovertemplate = paste(
                    'Volatility: %{x: .2%}<br>',
                    'Return:     %{y: .2%}<br>'
                  )
        ) %>%
        layout(showlegend = FALSE,
               xaxis = list(title = "Vola"),
               yaxis = list(title = "Mean return"),
               legend = list(orientation = "h",   # show entries horizontally
                             xanchor = "center",  # use center of legend as anchor
                             x = 0.5,
                             y = -25)  
        )
      
    })
  })
}
DonutPortPlotServer <- function(id,pieEffFront,load){
  moduleServer(id, function(input, output, session) {
    output$DonutPort<- renderPlotly({
      if (is.null(load())) {return(NULL)}
      plot_ly() %>%
        add_data(pieEffFront()) %>%
        add_pie(hole = 0.6,labels = ~Weight, values = ~Ratio, sort = FALSE, color ~Weight,
                hovertemplate = '%{label}:<br> Weight:  %{value: .2%}<extra></extra>', showlegend = T) 
    })
  })
}

HeatmapPlotServer <- function(id,correlation,load){
  moduleServer(id, function(input, output, session) {
    output$heat_m<- renderPlotly({ 
      if (is.null(load())) {return(NULL)}
      plot_ly(source = "Heat_Map") %>%
        add_data(correlation()) %>%
        add_heatmap(x=colnames(correlation()), y=rownames(correlation()), 
                    z = correlation(), colors = colorRamp(c("lightgreen", "red")))
    })
  })
}
ScatterPlotServer <- function(id,d,dx,dy){
  moduleServer(id, function(input, output, session) {
    output$scatterplot <- renderPlotly({
      if (is.null(d())) return(NULL)
      
      plot_ly(d(), x = ~x) %>%
        add_markers(y = ~y, customdata = ~time,hovertemplate = paste(
          '<b>Log-Return</b><br>',
          '%{xaxis.title.text}:  %{x: .2%}<br>',
          '%{yaxis.title.text}:  %{y: .2%}<br>',
          'Date:        %{customdata}<br>',
          '<extra></extra>')
        ) %>%
        add_lines(y = ~Estimate,hovertemplate = paste(
          '<b>Regression line</b><br>',
          '%{xaxis.title.text}:  %{x: .2%}<br>',
          '<extra></extra>')
        ) %>%
        layout(
          xaxis = list(title = dx()), 
          yaxis = list(title = dy()), 
          showlegend = FALSE
        )
      
      
    })
  })
}

DownloaddataServer <- function(id,filename,internalname,data){
  moduleServer(id, function(input, output, session) {
    output[[internalname]] <- downloadHandler(
      filename = function() {
        paste(filename, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(data.frame(data), file, row.names = FALSE)
      }
    )
  })
}

PlotlyspinnerUI<- function(id,name) {
  withSpinner(plotlyOutput(NS(id,name)), size = 0.7)
}
downloadButtonUI<- function(id,name,caption) {
  downloadButton(NS(id,name), caption)
}
