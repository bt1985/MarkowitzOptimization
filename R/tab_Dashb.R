

Dashboard_UI<-function(id){
  tabItem(tabName = id,
          titlePanel("Markowitz Optimization"),
          Text_initUI(),
          stocksearchInputUI("User_search"),
          tags$div(class ='suchergebnisse_tbl',displayresultsUI("usersearchresults")),
          br(),
          hidden(tags$hr(id="line1")),
          hidden(Text_PortfUI()),
          DT::dataTableOutput('portsmbs'),
          hidden(tags$div(class="accordion",id="accordion",  HTML('<span>advanced Options</span> <i class="fas fa-angle-down"></i>'))),
          tags$div(class="accpanel", hidden(checkboxInput("ShowInPu", label = "Show downloadbutton for input data and intermediate results", value = FALSE)),
                   hidden(dateRangeInput('dateRange',
                                         label = 'Date range input: Format yyyy-mm-dd',
                                         start = Sys.Date()-hist.len, end = Sys.Date()
                   )),
                   hidden(selectInput("curr1", "Currency:",
                                      c("EUR" = "EUR",
                                        "USD" = "USD"))),
          ),
          jsscriptaccordion_UI(),
          hidden(actionButton("bcalcef", "calculate efficent frontier")),
          hidden(Text_EffFrontUI()),
          PlotlyspinnerUI("plotEffront","EffFront"),
          br(),
          hidden(Text_OptPortpieUI()),
          PlotlyspinnerUI("plotDonutPort","DonutPort"),
          hidden(Text_heatmUI()),
          #withSpinner(plotlyOutput('heat_m'), size = 0.7),
          PlotlyspinnerUI("plotHeatmap","heat_m"),
          PlotlyspinnerUI("plotscatter","scatterplot"),
          #withSpinner(plotlyOutput('scatterplot'), size = 0.7),
          hidden(Text_InPuUI()),
          DT::dataTableOutput('stocks'),
          hidden(downloadButtonUI("download","downloadStocksData","Download Stock Data")),
          hidden(helpText("Stock returns:", id = "Stock_Ret")),
          data_returns=DT::dataTableOutput('stock_returns'),
          hidden(downloadButtonUI("download","downloadStocksData_return","Download Stock Data Returns")),
          hidden(helpText("Stock drift:", id = "Drift_Desc")),
          verbatimTextOutput('stock_drift'),
          hidden(downloadButtonUI("download","downloadStocksData_drift","Download Stock Data Drift")),
          #hidden(downloadButton("downloadStocksData_drift", "Download Stock Data Drift")),
          hidden(helpText("Stock vola:", id = "Vola_Desc")),
          DT::dataTableOutput('stock_sd'),
          #hidden(downloadButton("downloadStocksData_sd", "Download Stock Data Vola")),
          hidden(downloadButtonUI("download","downloadStocksData_sd","Download Stock Data Vola")),
          hidden(helpText("Efficent Frontier", id = "Eff_front_Desc")),
          DT::dataTableOutput('stock_effFront'),
          #hidden(downloadButton("downloadstock_effFront", "Download Efficent Frontier")),
          hidden(downloadButtonUI("download","downloadstock_effFront","Download Efficent Frontier")),
          #hidden(downloadButton("user_portf", "Download Your Portfolio"))
  )
}



Text_initUI<- function(){helpText("Search for ticker by entering the company's name in the input field below. 
                                  Select the desired instruments by clicking on the plus-button in the instrument row.
                                  Information will be collected from Yahoo finance. 
                                  After you have added all your stocks, click 'Optimize Portfolio'. 
                                  The efficent frontier for your portfolio will be calculated.
                                  ", id = "Text_init")}



Text_PortfUI<- function(){helpText("Your selected portfolio. It will be a equal weight of all positions assumed (naive portfolio). ", id ="selectedPort")}
Text_EffFrontUI<-function(){helpText(
  "Below the efficient frontier will be calculated. Click on the efficent frontier to see
                          the portfolio weights constructing the frontier. We also added a naive portfolio, which consists of
                          the equal weight of all positions.",
  id = "TextEffFront")}


Text_OptPortpieUI<-function(){helpText(
  "Pie Chart of the portfolio, selected position on the efficent frontier. 
                          Default is the portfolio with the highest sharpe ratio. 
                          Click on the frontier to select another position.",
  id = "TextOptPortpie")}


Text_heatmUI<-function(){helpText(
  "The heatmap shows the dependence of the selected finance instruments in the past. A high
                          correlation (red) indicates a bad riskspread. Click on a field in the heatmap to see a scatterplot
                          of the two financial instruments.",
  id = "Textheat_m")}


Text_InPuUI<-function(){helpText("Below you find the input data for the calculation of the efficent frontier and the intermediate results.",
                                 id = "Text_InPu")}

cssUI<-function(){
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  )
}

jsscriptscrollsmoothUI<-function(){tags$script(HTML(
  '
          Shiny.addCustomMessageHandler("scrollCallback",
          function(msg2) {
          var objDiv = document.getElementById("TextEffFront");
          objDiv.scrollIntoView({ left: 0, block: "start", behavior: "smooth" });
          }
          );'
  
))}


jsscriptrefreshDT_UI<-function(){tags$script(HTML(
  "Shiny.addCustomMessageHandler('unbind-DT', function(id) {
              Shiny.unbindAll($('#'+id).find('table').DataTable().table().node());
              })"))}

jsscriptaccordion_UI<-function(){tags$script(HTML(
  '
                    var acc = document.getElementsByClassName("accordion");
                      var i;
                      for (i = 0; i ==0; i++) {
                        acc[i].addEventListener("click", function() {
                          this.classList.toggle("active");
                          var panel = this.nextElementSibling;
                          if (panel.style.display === "block") {
                            panel.style.display = "none";
                          } else {
                            panel.style.display = "block";
                          }
                        });
                      }
                    '))}