# Canadian-bankruptcy-rate-prediction
This project is to forecast the Canadian bankruptcy rate using time series models in R.

## Project Description
Accurately forecasting national bankruptcy rates is of interest to national banks, insurance companies, credit-lenders, politicians etc. The goal of this project is to precisely and accurately forecast monthly bankruptcy rates for Canada. 
### Data
There is the monthly data from January 1987 to December 2014 on the 'train.csv' with the following variables:
- Unemployment Rate (%)
- Population
- Bankruptcy Rate (%)
- Housing Price Index<br />

Use the data in 'test.csv' to forecast the January 2015 – December 2017 bankruptcy rates

## Methods
There are numerous modeling approaches available to forecast bankruptcy rates. Given the condition of time series models, the following questions were considered:
- Overall trend -does bankruptcy rate increase or decrease with time?
- Seasonal trend - does bankruptcy rate have similar fluctuations within each year?
<br />
Considering the questions above, different models were built for this problem.<br />

### Models
- ARIMA/SARIMA 
- Exponential Smoothing/Holt-Winters Approach
- ARIMAX/SARIMAX
- VAR/VARX

## Forecasting
![Forecasting](https://user-images.githubusercontent.com/40928821/54242065-7d79ad00-44e0-11e9-91d7-b109fd3ed4ff.jpg)
