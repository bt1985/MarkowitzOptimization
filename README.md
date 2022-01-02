# Markowitz Optimization
A shiny app that uses a simple Markowitz optimization for a given set of stocks. An implemantation of the app can be found [here](https://bt1985.shinyapps.io/Markowitz/).

The user can choose from a given set of stocks (defined in the file collections.csv). 
![SelectPositions](/assets/SelectPositions.PNG)
She can then edit the set of stocks. 
![EditPortfolioweights](/assets/EditPortfolioweights.PNG)
The stock data will be collected from yahoo finance. The financial data from Yahoo for individual stocks is often messy, even for adjusted stock prices for individual stock positions. Splits or dividends are not always registered. 
![advancedoptions_startcalculation](/assets/advancedoptions_startcalculation.PNG)
The app  calculates then the efficent frontier as shown bellow. 
![EffFront](/assets/EffFront.png)
Initial the portfolio with the highest Sharperatio ist diplaied here. By clicking on the efficent frontier the user can display the portfolio constructing the specific point on the efficent frontier.
![Donuts](/assets/Donuts.png)
In this plot the correlations between the different stocks are displayed. 
![correlation](/assets/correlation.png)
By clicking on a cell in the correlation-heat map, the log returns of the two assets lying are displayed in a scatterplot:
![correlation](/assets/scatterplotlogreturns.png)
