additionalInfoUI <- function(id) {
  tabItem(tabName = id,
          titlePanel("Additional Information"),
          helpText("The stock data is acquired via yahoo finance. Currently only stock quotes for US Stock Markets are avaible.
                                Please note that all calculated values are not forward looking, they are historical values. 
                                This is in particular relevant for the shown correlation. Please use the advanced options to evaluate a portfolio
                                over diffrent periods of time. The provided information are for educational use only, they are by no means investment advise.
                                The financial data from Yahoo for individual stocks is often messy, even for adjusted stock prices for individual stock positions. 
                                Splits or dividends are not always registered. Please use other data sources for production.", id = "Help_Add_Info")
          
  )
}