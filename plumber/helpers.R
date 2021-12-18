
pcklist<- c('jsonlite', 'httr', 'quantmod', 'timeSeries', 'PortfolioAnalytics', 'PerformanceAnalytics', 'ROI.plugin.glpk', 'ROI.plugin.quadprog', 'ROI', 'timetk', 'kableExtra', 'plotly', 'stringr', 'readr')
for (package in pcklist){
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}




library(jsonlite)


library(httr)
library(dplyr)
library(quantmod)
library(timeSeries)
library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)
library(ROI)
#require(tidyverse)
library(timetk)
library(kableExtra)
library(plotly)
library(stringr)
library(readr)
library(tidyr)
#https://datascienceplus.com/accessing-web-data-json-in-r-using-httr/
#https://help.yahoo.com/kb/exchanges-data-providers-yahoo-finance-sln2310.html

EUROSUFFIX<-c('.VI', '.BR', '.TL', '.HE', '.NX', '.PA', '.BE', '.BM', '.DU', '.F', '.HM', '.HA', '.MU', '.SG', '.DE', '.AT', '.IR', '.TI', '.MI', '.RG', '.VS', '.AS', '.LS', '.MC')
USDSUFFIX<-c('.CBT', '.CME', '.NYB', '.CMX', '.NYM', '.USA')

ticker_info_suchen <- function(suche) {
  suche2 <- gsub(" ", "%20", suche)
  bla <- GET(paste0("http://autoc.finance.yahoo.com/autoc?query=", suche2, "&region=EU&lang=en-GB"))
  jsonRespParsed<- content(bla,as="parsed") 
  res<- jsonRespParsed$ResultSet$Result%>%
    bind_rows()
    #{ ifelse(!is.null(.) ,filter(.,!str_detect(symbol,"\\.")), "no results" ) }
  if (nrow(res)==0){
    res
  }else{
    filter(res,!str_detect(symbol,"\\."))
  }
    
  
}

calc_f <- function(data, len1) {
  adj.close <- tail(data, len1)
}

calc_drift<- function(data) {
  colMeans(data)
}

calc_vola<- function(data) {
  cov(data)
}


getstocks <- function(symbol,hist.len){
  symbol<-unlist(strsplit(symbol, split=" "))
  hist.len<-as.Date(unlist(strsplit(hist.len, split=" ")))
  symbol <- c(symbol) #Liste aus der Inputmatrix machen
  dataInputEnv <- new.env()
  getSymbols(symbol, from=hist.len[1], to = hist.len[2], src = "yahoo", env=dataInputEnv)
  plist <- eapply(dataInputEnv, Ad)
  pframe <- do.call(merge, plist)
  pframe <- na.omit(pframe)
  colnames(pframe) <- gsub(".Adjusted","",colnames(pframe))
  df <- data.frame(time = time(pframe),coredata(pframe))
  df
}


  
  
optimize_stocks_p2<- function(req, n.po) {
  n.po<-as.numeric(n.po)
  returns<-list()
  returns<-(jsonlite::fromJSON(req$postBody))
  optimize_stocks_p2_calc(returns, n.po)
}

optimize_stocks_p2_calc <-function(returns, n.po){
  pframe_returns<-xts(select(returns$returns,-one_of("time")), order.by =as.Date(returns$returns$time))
  tickers <-colnames(pframe_returns)
  pspec2 <- portfolio.spec(assets=colnames(pframe_returns))
  pspec2 <- add.constraint(portfolio=pspec2,
                           type="full_investment") # weights sum to 1
  pspec2 <- add.constraint(portfolio=pspec2,
                           type="box",
                           min = 0.000,
                           max = +1)
  pspec2 <- add.objective(pspec2, type = "risk", name = "var")
  eff.frontier <- create.EfficientFrontier(R=pframe_returns, portfolio=pspec2, type="mean-sd",n.portfolios = n.po)
  data.frame(eff.frontier$frontier[,])
  # meanReturns <- colMeans(pframe_returns)
  # Cova <- cov(pframe_returns)
  # vola <- sqrt(diag(Cova))
  # Korrelation <- cor(pframe_returns)
  # df.stocks <- data.frame(
  #   meanReturns,
  #   vola,
  #   tickers
  # )
  
  #return(list("Stocksdata" = df.stocks,"Frontier" = eff.frontier, "CovMa" = Cova, "Korrelation" = Korrelation))
  
}

eff_front_plot_dat<- function(Weights_F, tickers_F) {
  
  d_F <- as.tibble(Weights_F)%>%tidyr::gather(tickers_F,Weights_F)%>%add_column(Index=rep(1:length(Weights_F[,1]),length(Weights_F)))
  return(d_F)
}


log_rendite <- function(req){
  shareprices<-list()
  shareprices<-(jsonlite::fromJSON(req$postBody))
  ergeb<-log_rendite_calc(shareprices)
}
log_rendite_calc <-function(shareprices) {
  shareprices_xts<-xts(select(shareprices$shareprices,-one_of("time")), order.by =as.Date(shareprices$shareprices$time))
  returns_xts<-ROC(shareprices_xts)
  df <- data.frame(time = time(returns_xts),coredata(returns_xts))
}
fx_fx <- function(curr, hist.len){
  library(quantmod)
  curr<-unlist(strsplit(curr, split=" "))
  hist.len<-as.Date(unlist(strsplit(hist.len, split=" ")))
  dataInputEnvFX <- new.env()
  getSymbols(paste0(curr[1], curr[2], "=X"), from = hist.len[1], to = hist.len[2], env=dataInputEnvFX)
  plistFX <- eapply(dataInputEnvFX, Ad)
  pframeFX <- do.call(merge, plistFX)
  pframeFX <- na.omit(pframeFX)
  df <- data.frame(time = time(pframeFX),coredata(pframeFX))
  
}

all_in_one <-function(symbol,hist.len,curr){

  #Aktienkurse
  stocks<- getstocks(symbol, hist.len) %>%
            na.omit()
  
  #FX-Umrechnung
  if (curr == "USD"){
    CurrSuffix<-EUROSUFFIX # nehm alle Boersen in Euro und rechne nach Dollar
    FX_Col<-"EURUSD"
  } else {
    CurrSuffix<-USDSUFFIX # nehm alle Boersen in Dollar und rechne nach EUR
    FX_Col<-"USDEUR"
  }
  FX<-fx_fx(paste0(substr(FX_Col, 0, 3)," ",substr(FX_Col, 4, 6)),hist.len) %>%
    na.omit()
  colnames(FX)<-gsub(".X.Adjusted","",colnames(FX))
  stocks_fx<- merge(stocks, FX, by = 'time')%>%
    na.omit()
  stocks_fx<- stocks_fx %>% 
                mutate_at(vars(contains(CurrSuffix)), "*",as.name(FX_Col))
  if (curr != "USD"){
    stocks_fx_usd<-stocks_fx %>% 
      select(-time) %>% 
      select(-matches("\\w+\\.\\w+")) %>% 
      mutate_at(vars(matches(paste0("[^",FX_Col,"]"))), "*",as.name(FX_Col))
    stocks_fx2<-select(stocks_fx,time)
    stocks_fx2<-cbind(stocks_fx2,select(stocks_fx,matches("\\w+\\.\\w+")))                        
    stocks_fx2<-cbind(stocks_fx2,stocks_fx_usd)
    #col_order <-c("time",portsymb[,1],FX_Col)
    #stocks_fx2<- stocks_fx2[col_order]
    stocks_fx<-stocks_fx2
  }
  
  stocks_fx<- stocks_fx %>% 
                select(-FX_Col)
  #Rendite berechnen

  
  stock_returns<-log_rendite_calc(list(shareprices = stocks_fx))%>%
                           na.omit()
  #Drift,Cov und Vola
  returns_xts<-xts(select(stock_returns,-one_of("time")), order.by =as.Date(stock_returns$time))
  stock_drift<-colMeans(returns_xts)*252
  stock_cov<-cov(returns_xts)
  stock_vola <- sqrt(diag(stock_cov))*sqrt(252)
  stock_korrelation <- cor(returns_xts)
  #Eff Front
  p2<-optimize_stocks_p2_calc(list(returns = stock_returns),100)%>%
    na.omit()
  effFront <- as.data.frame(p2)
  effFront <- effFront %>%
                rename_at(vars(starts_with("w.")), funs(str_replace(., "w.", ""))) 
  effFront <- effFront %>%
                mutate(mean = mean*252) %>%
                mutate(StdDev = StdDev*sqrt(252))
  effFront["SharpeRatio"] <- as.vector(effFront["mean"]/effFront["StdDev"])
  tickers<- colnames(returns_xts)
  
  # PieChart
  pieEffFront<- effFront %>%
      dplyr::filter(SharpeRatio == max(SharpeRatio)) %>%
      gather(Weight, Ratio, as.character(tickers), factor_key=TRUE) %>%
      mutate(Weight = as.character(Weight))%>%
      arrange(Weight)
  

  df.stocks <- data.frame(
    stock_drift,
    stock_vola,
    tickers)
  
  return(list("Stocksdata" = df.stocks,"Frontier" = effFront, "CovMa" = stock_cov, "Korrelation" = stock_korrelation, "Log_returns"=stock_returns))
  
}

plotly_efffront <-function(symbol,hist.len,curr){
  p3<-all_in_one(symbol,hist.len,curr)
  p4 <- p3$Frontier %>% bind_rows()
  
  plot_ly() %>%
    add_data(p4)%>%
    add_trace(x = ~StdDev, y = ~mean, color = ~SharpeRatio, mode = "lines+markers", customdata = ~SharpeRatio,
              hovertemplate = paste(
                'Vola::  %{x: .2%}<br>',
                'Mean return:      %{y: .2%}<br>',
                'SharpeRatio: %{customdata: .2f}<br>',
                '<extra></extra>'
              ),
              type = "scatter", name = "Efficient Frontier")
}
#test123<- all_in_one("AAPL TSLA MSFT","2016-12-01 2020-11-01", "EUR")
#print(head(test123))
