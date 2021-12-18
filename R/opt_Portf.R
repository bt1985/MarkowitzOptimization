histstockdataServer <-function(id,stockdata,hist){
  moduleServer(id, function(input, output, session) {
    GET(paste0(str_serv,"/getstocks?symbol=",paste(stockdata,collapse="%20"),"&hist.len=",paste(hist,collapse="%20"),"%20")) %>%
          content(as="parsed") %>%
          bind_rows()%>%
          na.omit()
  })}


                      