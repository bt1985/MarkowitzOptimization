showoutputServer <- function(input, output, session,showaddoputput) {
  shinyjs::show("TextEffFront")
  if (showaddoputput){
    shinyjs::show("Text_InPu")
    shinyjs::show("Data_Desc")
    shinyjs::show("Stocks_Desc")
    shinyjs::show("download-downloadStocksData")
    shinyjs::show("Stock_Ret")
    shinyjs::show("download-downloadStocksData_return")
    shinyjs::show("Drift_Desc")
    shinyjs::show("download-downloadStocksData_drift")
    shinyjs::show("Vola_Desc")
    shinyjs::show("download-downloadStocksData_sd")
    shinyjs::show("Eff_front_Desc")
    shinyjs::show("download-downloadstock_effFront")
    shinyjs::show("user_portf")
  }
  shinyjs::show("TextOptPortpie")
  shinyjs::show("Textheat_m")
  session$sendCustomMessage(type = "scrollCallback",1)
  1
}

histstockdataServer <-function(id,stockdata,hist){
  moduleServer(id, function(input, output, session) {
    GET(paste0(str_serv,"/getstocks?symbol=",paste(stockdata,collapse="%20"),"&hist.len=",paste(hist,collapse="%20"),"%20")) %>%
          content(as="parsed") %>%
          bind_rows()%>%
          na.omit()
  })}

histfxdataServer<-function(id,choice_FX,hist,stocks){
  moduleServer(id, function(input, output, session) {

    if (choice_FX == "USD"){
      CurrSuffix<-EUROSUFFIX # nehm alle Boersen in Euro und rechne nach Dollar
      FX_Col<-"EURUSD"
    } else {
      CurrSuffix<-USDSUFFIX # nehm alle Boersen in Dollar und rechne nach EUR
      FX_Col<-"USDEUR"
    }
    FX<-isolate(POST(paste0(str_serv,"/fx?curr=",substr(FX_Col, 0, 3),"%20",substr(FX_Col, 4, 6),"&hist.len=",paste(hist,collapse="%20"))))
    FX<-content(FX,as="parsed") %>%
      bind_rows()%>%
      na.omit() %>%
      isolate()
    colnames(FX)<-gsub(".X.Adjusted","",colnames(FX))%>%
      isolate()
    stocks_fx<-merge(stocks, FX, by = 'time')%>%
      na.omit()%>%
      isolate()
    stocks_fx<-stocks_fx %>% mutate_at(vars(contains(CurrSuffix)), "*",as.name(FX_Col))
    if (choice_FX != "USD"){
      stocks_fx_usd<-stocks_fx %>% 
        select(-time) %>% 
        select(-matches("\\w+\\.\\w+")) %>% 
        mutate_at(vars(matches(paste0("[^",FX_Col,"]"))), "*",as.name(FX_Col))
      stocks_fx2<-select(stocks_fx,time)
      stocks_fx2<-cbind(stocks_fx2,select(stocks_fx,matches("\\w+\\.\\w+")))                        
      stocks_fx2<-cbind(stocks_fx2,stocks_fx_usd)
      #browser()
      #col_order <-c("time",portsymb_clean,FX_Col)
      #stocks_fx2<- stocks_fx2[col_order]
      stocks_fx<-stocks_fx2
    }
    stocks_fx<- stocks_fx %>% select(-FX_Col)
  })
}