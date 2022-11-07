# install.packages('ggplot2')
# install.packages('pdfetch')
# install.packages('quantmod')
# install.packages('TTR')
# install.packages('Zoo')
# install.packages('statsmodels')
# install.packages('forecast')
# install.packages('prophet')
# install.packages('keras')
# tensorflow::install_tensorflow()
# install.packages('rugarch')
# install.packages('PerformanceAnalytics')
library(zoo)
library(ggplot2)
library(pdfetch)
library(quantmod)
library(TTR)
library(lubridate)
library(plotrix)
library(stats)
library(xts)
library(forecast)
library(prophet)
library(e1071)
library(MLmetrics)
library(keras)
library(rugarch)
library(PerformanceAnalytics)
library(tseries)

#Loading data from yahoo finance...
#BTC-USD, ETH-USD, BSB-USD
get.crypto.data <- function(symbol){
    data <- getSymbols(symbol, src = 'yahoo',auto.assign = FALSE)
    colnames(data) <- c('open','high','low','close','volume','adjusted')
    return(data)
}

data <- get.crypto.data('BTC-USD')

#Chart series in candleStick format data on the month of COVID
chartSeries(data["2020-03"])

#Chart series data line graph
chartSeries(data)

#Calculating returns for further usage....
returns <- CalculateReturns(data$close)
returns<- returns[-1]
#Distribution of returns.
chart.Histogram(returns, methods = c('add.density', 'add.normal'),
                colorset = c('blue','green','red'))
# Checking outliers/white noise in returns...
chartSeries(returns, theme = 'white')

#Annual Volatility
#THis is the formula to calculate volatility...
sd(returns, na.rm = TRUE)
sqrt(252)*sd(returns['2018'])

chart.RollingPerformance(R = returns['2014::2022'], width = 22, FUN ='sd.annualized', scale = 365, main = 'YearlyRolling Volatility')

plot(zoo(data), plot.type = 'multiple',col = c('black','blue','green','yellow','orange','purple'), main = "Every column Trend in dataset")

data$month <- month(as.POSIXlt(index(data), format="%d/%m/%Y"))
data$year <- year(as.POSIXlt(index(data), format = "%d/%m/%Y"))
#Comparison between daily open and close....
ggplot(data = data, aes(x = index(data)))+
    geom_line(aes(y = data$open), colour = 'black')+
    geom_line(aes(y = data$close), colour = 'red')
#Comparison between daily high and low....
ggplot(data = data, aes(x = index(data)))+
    geom_line(aes(y = data$high, color = 'green'))+
    geom_line(aes(y = data$low, color = 'yellow'))

btc.2017 <- data[data$year == 2017]
btc.2018 <- data[data$year == 2018]
btc.2019 <- data[data$year == 2019]
btc.2020 <- data[data$year == 2020]
# Removing leap year leap day 29-02-2020 from the data to match 365 days...
btc.2020 <- btc.2020[!(format(index(btc.2020), format = "%m") =="02" &
             format(index(btc.2020), format = "%d")=="29"),]
btc.2021 <- data[data$year == 2021]
btc.2022 <- data[data$year == 2022]

#Yearly line graph for open values...
ggplot(data = btc.2017, aes(x = as.POSIXct(index(btc.2017), format="%d/%m/%Y")))+
    geom_line(aes(y = btc.2017$open, colour = '2017'))+
    geom_line(aes(y = btc.2018$open, colour = '2018'))+
    geom_line(aes(y = btc.2019$open, colour = '2019'))+
    geom_line(aes(y = btc.2020$open, colour = '2020'))+
    geom_line(aes(y = btc.2021$open, colour = "2021"))+
    # geom_line(aes(y = btc.2022$BTC.USD.Open, colour = "2022"))
    scale_colour_manual(name = "year", aesthetics = "colour",values = c("2017" = "darkblue", "2018" = "red", '2019' = 'orange', "2020" = 'purple', "2021" = 'green', "2022" = 'pink'))+
    scale_x_datetime(date_labels = "%b")+
    xlab('Month of year')+
    ylab('BTC-USD Open Price')+
    theme_minimal()

#Analysis of covid from 30 JAN 2020 to 5 MAR 2022 
covid.period <- data['20200130/20220505']
ggplot(data = covid.period, aes(x = as.POSIXct(index(covid.period), format="%d/%m/%Y")))+
    geom_line(aes(y = covid.period$open, colour = 'covid'))+
scale_colour_manual(name = "year", aesthetics = "colour",values = c("covid" = "red"))+
    scale_x_datetime(date_labels = "%b", date_breaks = "2 month")+
    xlab('Month of year 2020 - 2022')+
    ylab('BTC-USD Open Price')+
theme_minimal()

#Inprogress....
# components.ts <- decompose(data)
# plot(components.ts)
# -------------------------------------------------

#Auto Correlation Model....
#On the basis of combination of ACF and PACF we will determine if 
# AR, MA, ARMA, or ARIMA model will be used. 
# 2 things to consider..the breakeven point and tail. Which chart is having tail or which chart is having breakeven point determines what model will be used...
model <- lm(open ~ ., data =data)
acf(model$residuals,plot = TRUE, type = 'correlation');
pacf(model$residuals, pl = TRUE)

model <- lm(high ~ ., data =data)
acf(model$residuals,plot = TRUE, type = 'correlation');
pacf(model$residuals, pl = TRUE)
#ARMA Model...

model <- lm(low ~ ., data =data)
acf(model$residuals,plot = TRUE, type = 'correlation');
pacf(model$residuals, pl = TRUE)
#ARMA Model...

model <- lm(open ~ ., data =data)
acf(model$residuals,plot = TRUE, type = 'correlation');
pacf(model$residuals, pl = TRUE)
#AR or ARMA Model...

model <- lm(close ~ ., data =data)
acf(model$residuals,plot = TRUE, type = 'correlation');
pacf(model$residuals, pl = TRUE)
#MA Model...

model <- lm(volume ~ ., data =data)
acf(model$residuals,plot = TRUE, type = 'correlation');
pacf(model$residuals, pl = TRUE)
#MA or ARMA...

#Analysis...
data.before.2022 <- data[data$year < 2022]

# Box.test(model$residuals, lag = 30, type=  'Ljung-Box')

# hist(model$residuals, col = 'red', xlab = 'Error', freq = FALSE)
# lines(density(model$residuals))


# library(ggplot2)
# ggplot2::autoplot(forecast_data)


#Using ARIMA model and comparing it with AR, MA model..
#Using auto.arima it will automatically choose the optimal values of p,d,q based on lowered error metric. 
run.arima.model <- function(dataSeries){
    start.month <- month(index(dataSeries))[1]
    month <- month(index(dataSeries))[length(dataSeries)]
    start.day <- day(index(dataSeries))[1]
    day <- day(index(dataSeries))[length(dataSeries)]
    start.year <- year(index(dataSeries)[1])
    year <- year(index(dataSeries)[length(dataSeries)])
    closedata <- ts(dataSeries, c(as.character(start.year),as.character(start.month),as.character(start.day)), c(as.character(year),as.character(month),as.character(day)),365)
    dataSeries <- closedata
    #Stationary Test...arima model wont test stationary on its own.
    adf.test(dataSeries)
    #Main arima model...this will find optimal value for p,d,q...
    auto.model <- auto.arima(closedata, ic = 'aic', trace = TRUE, D=1)
    print(auto.model)
    print('Forecasting for 1 day...')
    auto.forecast <- forecast(auto.model, level = c(95), h = 1)
    print(auto.forecast)
    print('Forecasting for 90 Days...')
    auto.forecast <- forecast(auto.model, level = c(95), h = 90) 
    print(auto.forecast)
    autoplot(auto.forecast, main = 'Forecast for 90 Days using ARIMA')
    print('Forecasting for 180 Days...')
    auto.forecast <- forecast(auto.model, level = c(95), h = 180) 
    print(auto.forecast)
    autoplot(auto.forecast, main = 'Forecast for 180 days using ARIMA')
    # AR(1) model...
    ar.model <- arima(dataSeries, order = c(1,0,0))
    ar.forecast <- forecast(ar.model , 100)
    # MA(1) model...
    ma.model <- arima(dataSeries, order = c(0,0,1))
    ma.forecast <- forecast(ar.model , 100)
    # Printing accuracies...consider MAPE value 100-MAPE '%' will be the accuracy of model..
    print('auto.forecast Accuracy: ')
    print(accuracy(auto.forecast))
    print('ar.forecast Accuracy: ')
    print(accuracy(ar.forecast))
    print('ma.forecast Accuracy: ')
    print(accuracy(ma.forecast))

    #Plotting fit and forecast....
    #All red coloured lines are arima model lines...
    # Primariy considering arima model..
    # The straight line at the end of time series graph is the mean value provided by arima model
    # And it's the forecasted value provided to us....
    ts.plot(dataSeries)
    points(auto.model$fitted, type = 'l', col = 2, lty = 2)
    points(auto.forecast$lower, type = "l", col = 2, lty = 2)
    points(auto.forecast$upper, type = "l", col = 2, lty = 2)
    points(ar.model$residuals, type = 'l', col = 3, lty = 2)
    points(ma.model$residuals, type = 'l', col = 4, lty = 2)
    points(auto.model$residuals, type= 'l', col=2, lty = 2)
    points(auto.forecast$mean, type = 'l', col = 2, lty = 2)
    points(ar.forecast$mean, type = 'l', col = 5, lty = 2)
    points(ma.forecast$mean, type = 'l', col = 6, lty = 2)
    #acf and pacf plots for auto.model...for evaluation purposes...
    acf(auto.model$residuals, main = 'Correlogram')
    pacf(auto.model$residuals, main = 'partial correlogram')
    #Can also consider AIC and BIC values for model evaluation purposes also...
}

# Won't work properly for volume...as volume value is big in number...
run.arima.model(data$close)

#Prophet is one of the most popular time series model developed by Facebook(META)
#Requires dataset in format ds and y ds = datetime y = the column to evaluate...
run.prophet.pipeline <- function(dataSeries){
    #Creating Prophet Dataset...
    dataSeries <- data$close
    print('creating Dataset..')
    dateData <- index(dataSeries)
    openData <- dataSeries
    
    prophet.dataset <- data.frame(dateData, openData)
    
    colnames(prophet.dataset) <- c('ds','y')
    
    row.names(prophet.dataset) <- seq(1:nrow(prophet.dataset))
    
    #Creating Prophet Model...
    print('Creating Prophet Model...')
    prophet.model <- prophet(prophet.dataset)
    #adding 365 days more to date in prophet model...
    anotherDayFuture <- make_future_dataframe(prophet.model, periods = 1)
    futureInQuarter <- make_future_dataframe(prophet.model, periods = 90)
    HalfFuture <- make_future_dataframe(prophet.model, periods = 180)
    #Forecasting...
    print('Forecasting...')
    forecast <- predict(prophet.model, anotherDayFuture)
    
    #Plotting...
    print(forecast$yhat[length(forecast$yhat)])
    print('Plotting...')
    plot(prophet.model, forecast)
    
    forecast <- predict(prophet.model, futureInQuarter)
    
    #Plotting...
    print(forecast$yhat[length(forecast$yhat)])
    print('Plotting...')
    plot(prophet.model, forecast)
    
    forecast <- predict(prophet.model, HalfFuture)
    
    #Plotting...
    print(forecast$yhat[length(forecast$yhat)])
    print('Plotting...')
    plot(prophet.model, forecast)
    
    prophet_plot_components(prophet.model, forecast)
    print('Plotting Done...')
    #evaluating Model...
    print('Model Evaluation...')
    pred <- forecast$yhat[1:length(dataSeries)]
    actual <- prophet.model$history$y
    plot(actual, pred)
    abline(lm(pred~actual), col = 'red')
    print(summary(lm(pred~actual)))
    print('Cross Validation...')
    cs<-cross_validation(prophet.model, 365, units = 'days')
    print(performance_metrics(cs, rolling_window = 0.1))
    
    plot_cross_validation_metric(cs, metric = 'rmse', rolling_window = 0.1)
    print('done...')
}

run.prophet.pipeline(data$close)


#Support Vector Machine is one of the ML model which works good on Time series model along with Supervised Problems...
# We are using regression algorithm with radial kernel...
run.svm.model <- function(dataSeries){
    svm.model <- svm(dataSeries~index(data), type = 'eps-regression', kernel = 'radial', cost = 0.1, gamma = 1000)
    
    one.nd <- 1:length(dataSeries)+1
    quarter.nd <- 1:length(dataSeries)+90
    half.year.nd <- 1:length(dataSeries)+180
    
    print('one day forecast..')
    predictions <- predict(svm.model, data = data.frame(x=one.nd))
    print(predictions[length(predictions)])
    ylim <- c(min(dataSeries), max(dataSeries))
    xlim <- c(min(one.nd),max(one.nd))
    plot(dataSeries, col="blue", ylim=ylim, xlim=xlim, type="l")
    par(new=TRUE)
    plot(predictions, col="red", ylim=ylim, xlim=xlim, main = 'One Day Forecast (Red forecast Line)')
    
    print('quarter year forecast.')
    predictions <- predict(svm.model, data = data.frame(x=quarter.nd))
    print(predictions[length(predictions)-90:length(predictions)])
    ylim <- c(min(dataSeries), max(dataSeries))
    xlim <- c(min(one.nd),max(one.nd))
    plot(dataSeries, col="blue", ylim=ylim, xlim=xlim, type="l")
    par(new=TRUE)
    plot(predictions, col="red", ylim=ylim, xlim=xlim, main = 'quarter Year Forecast (Red forecast Line)')
    
    print('half yearly forecast.')
    predictions <- predict(svm.model, data = data.frame(x=half.year.nd))
    print(predictions[length(predictions)-180:length(predictions)])
    ylim <- c(min(dataSeries), max(dataSeries))
    xlim <- c(min(one.nd),max(one.nd))
    plot(dataSeries, col="blue", ylim=ylim, xlim=xlim, type="l")
    par(new=TRUE)
    plot(predictions, col="red", ylim=ylim, xlim=xlim, main = 'Half Year Forecast (Red forecast Line)')
    
    RMSE(predictions, dataSeries)
}

run.svm.model(data$close)

# devtools::install_github("berndbischl/ParamHelpers") # version >= 1.11 needed.
#install mlr, lhs, hashids, ParamHelpers first...using install.packages...
# devtools::install_github("jakob-r/mlrHyperopt", dependencies = FALSE)
# library(ParamHelpers)
# 
# library(mlrHyperopt)
# res = hyperopt(data, learner = "classif.svm")
# res
# Tune result:
# Op. pars: cost=1.21e+04; gamma=0.000239
# mmce.test.mean=0.0266667

#Inprogress....................
#RNN Model...

# model <- keras_model_sequential()
# 
# model %>%
#     layer_embedding(input_dim = 500, output_dim = 32) %>%
#     layer_simple_rnn(units = 32) %>%
#     layer_dense(units = 1, activation = 'sigmoid')
# 
# model %>% compile(optimizer = 'rmsprop',
#                   loss = 'binary_crossentropy',
#                   metrics = c('acc'))
# 
# history <- model %>% fit()
#-----------------------------------

#GARCH Model..
#Generalized AutoRegressive Conditional Heteroskedasticity (GARCH) is a statistical model used in #analyzing time-series data where the variance error is believed to be serially autocorrelated. GARCH #models assume that the variance of the error term follows an autoregressive moving average process.
#Another Time series model..
# It works on majorly 3 paramters...mean, variance and distribution type...
# Helps in predicting by keeping in mind the volatility of market...
# https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0245904#:~:text=We%20use%20seven%20GARCH%2Dtype,selected%20crypto%20and%20world%20currencies.

#If Executing the code check the p value/significance metric...
run.garch.model <- function(dataSeries){
    returns <- CalculateReturns(dataSeries)
    returns<- returns[-1]
    #Distribution of returns.
    chart.Histogram(returns, methods = c('add.density', 'add.normal'),
                    colorset = c('blue','green','red'))
    # Checking outliers/white noise in returns...
    chartSeries(returns, theme = 'white')
    sgarch <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                             variance.model = list(model = 'sGARCH'),
                             distribution.model = 'norm')
    
    sgarch.model <- ugarchfit(data = returns, spec = sgarch)
    
    print(sgarch.model)
    
    plot(sgarch.model)
    
    v<- sqrt(365)*sigma(sgarch.model)
    w<- 0.1/v
    plot(merge(v,w),
         multi.panel = T)
    
    
    forecast.ugarch <- ugarchforecast(fitORspec = sgarch.model, n.ahead = 90)
    #fitted values of constant mean model..
    plot(fitted(forecast.ugarch))
    #plotting variability....
    plot(sigma(forecast.ugarch))
}

run.garch.model(data$close)

#Skewed student T distribution...
run.sstd.garch.model <- function(dataSeries){
    returns <- CalculateReturns(dataSeries)
    returns<- returns[-1]
    #Distribution of returns.
    chart.Histogram(returns, methods = c('add.density', 'add.normal'),
                    colorset = c('blue','green','red'))
    # Checking outliers/white noise in returns...
    chartSeries(returns, theme = 'white')
    sgarch.sstd <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                         variance.model = list(model = 'sGARCH'),
                         distribution.model = 'sstd')
    
    sgarch.model.sstd <- ugarchfit(data = returns, spec = sgarch.sstd)
    print(sgarch.model.sstd)
    # we cannot reject the null hypothesis and state that this model is good for residuals...
    
    
    plot(sgarch.model.sstd)
}

run.sstd.garch.model(data$close)

run.gjr.garch.model <- function(dataSeries){
    returns <- CalculateReturns(dataSeries)
    returns<- returns[-1]
    #Distribution of returns.
    chart.Histogram(returns, methods = c('add.density', 'add.normal'),
                    colorset = c('blue','green','red'))
    # Checking outliers/white noise in returns...
    chartSeries(returns, theme = 'white')
    sgarch.gjr <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                              variance.model = list(model = 'gjrGARCH'),
                              distribution.model = 'sstd')
    
    sgarch.model.gjr <- ugarchfit(data = returns, spec = sgarch.gjr)
    print(sgarch.model.gjr)
    plot(sgarch.model.gjr, which = 'all')
}

run.gjr.garch.model(data$close)
#better than previous model...skew and shape...must be lower...

#Simulation...
sfinal <- sgarch.gjr
setfixed(sfinal) <- as.list(coef(sgarch.model.gjr))
# fixedCoef <- as.list(coef(sgarch.model.gjr))

#Execute the following codes on crypto which was launched before 2020...
#Purely for simulation purposes...
f2018<- ugarchforecast(data = returns['/2020-12'],
                       fitORspec = sfinal,
                       n.ahead = 365)

f2022 <- ugarchforecast(data = returns['/2022-09'],
                        fitORspec = sfinal,
                        n.ahead = 100)
par(mfrow = c(2,1))
plot(sigma(f2018), main='volatility')
plot(sigma(f2022))

sim <- ugarchpath(spec = sfinal,
                  m.sim = 3,
                  n.sim = 1*100,
                  rseed = 123)
plot.zoo(fitted(sim))
plot.zoo(sigma(sim))
par(mfrow = c(1,1))
tail(data)
p <- 19153.87*apply(fitted(sim),2,'cumsum')+19153.87
matplot(p, type = 'l', lwd = 3)

#--------------------pipeline functions-----------------------------
data <- get.crypto.data("BSB-USD") #Add Token name...ETH-USD, BTC-USD etc...
run.arima.model(data$close) # add data$columnName...
run.prophet.pipeline(data$close) # add data$columnName...
run.svm.model(data$close) # add data$columnName...
run.garch.model(data$close) # add data$columnName...
run.sstd.garch.model(data$close) # add data$columnName...
run.gjr.garch.model(data$close) # add data$columnName...
#--------------------------------------------------------------------
#Question/Answers:
#Answer1: Yes Indeed. We can see in our analysis our models are having accuracy of 96-98%. Also we are using 95% confidence interval which means we can be 95% confident on the predictions the Models will make.

#Answer2: Yes, We can observe this in charts of ARIMA model that if we are predicting for longer..then horizon of 95% confidence interval will also increase we can see greater HIGH and LOW of Prediction INTERVAL due to increase in time. More TIME More Chances of getting uncertainity so YES.

#Answer3: Yes Indeed, Model Machine Learning Models like GARCH take Volatility of market into consideration which traditional Model dont consider...Also models like Prophet backed by Big Company like Meta...Which is based on Additive model which take into consideration things like non-linear trends fit with yearly, monthly, daily seasonality. It also take into consideration the effect of holidays.

#Answer4: It Depends on the model, the Nature of model, amount of data, seasonality and Volatility of market. to have One common model for all...might be tough call but to say in our scenario we can consider ARIMA or GARCH model because GARCH Model is effective on Volatile market data like Cryto Market. And ARIMA is a model which takes into consideration the Moving Average of Data and ACF and PACF as major points.

#Answer5: Macro Economics directly or indirectly affect the sentiments of Crypto Market. Some Big Events like Involvement of Elon Musk into Crypto Market...COVID-19 and RUSSIA-UKRAINE WAR put direct effect on Crypto Prices...Talking about Covid here...in early phases we see that there was a sudden drop in crypto prices during first lock down...as lockdown pushed off...we can see big surge in crypto prices...during early 2021 and during 2021 when 2nd wave hit. People leaned towards regualting and trading crypto on daily basis leads to high demand and prices of crypto currencies. And during late 2021 and early 2022 we can see drop in crypto prices as several nation's govt imposed crypto ban or strict laws on circulating crypto currency which lead to downfall during jan 2022. 


















