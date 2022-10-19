# install.packages('ggplot2')
# install.packages('pdfetch')
# install.packages('quantmod')
# install.packages('TTR')
# install.packages('Zoo')
# install.packages('statsmodels')
# install.packages('forecast')
# install.packages('prophet')
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

data <- getSymbols('BTC-USD', src = 'yahoo',auto.assign = FALSE)
colnames(data) <- c('open','high','low','close','volume','adjusted')
# data <- zoo(data, order.by = index(data), frequency = 52)

plot(zoo(data), plot.type = 'multiple',col = c('black','blue','green','yellow','orange','purple'))

data$month <- month(as.POSIXlt(index(data), format="%d/%m/%Y"))
data$year <- year(as.POSIXlt(index(data), format = "%d/%m/%Y"))

ggplot(data = data, aes(x = index(data)))+
    geom_line(aes(y = data$open), colour = 'black')+
    geom_line(aes(y = data$close), colour = 'red')

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

covid.period <- data['20200130/20220505']
ggplot(data = covid.period, aes(x = as.POSIXct(index(covid.period), format="%d/%m/%Y")))+
    geom_line(aes(y = covid.period$open, colour = 'covid'))+
scale_colour_manual(name = "year", aesthetics = "colour",values = c("covid" = "red"))+
    scale_x_datetime(date_labels = "%b", date_breaks = "2 month")+
    xlab('Month of year')+
    ylab('BTC-USD Open Price')+
theme_minimal()

#Inprogress....
# components.ts <- decompose(data)
# plot(components.ts)
# -------------------------------------------------

#Auto Correlation Model....
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



analysis <- function(data){
    #Main arima model...this will find optimal value for p,d,q...
    auto.model <- auto.arima(data, seasonal = FALSE)
    print(auto.model)
    auto.forecast <- forecast(auto.model, 100) 
    # AR(1) model...
    ar.model <- arima(data, order = c(1,0,0))
    ar.forecast <- forecast(ar.model , 100)
    # MA(1) model...
    ma.model <- arima(data, order = c(0,0,1))
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
    ts.plot(data)
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
analysis(data$adjusted)

run.prophet.pipeline <- function(dataSeries){
    #Creating Prophet Dataset...
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
    future <- make_future_dataframe(prophet.model, periods = 365)
    #Forecasting...
    print('Forecasting...')
    forecast <- predict(prophet.model, future)
    
    #Plotting...
    print('Plotting...')
    plot(prophet.model, forecast)
    
    prophet_plot_components(prophet.model, forecast)
    print('Plotting Done...')
    #evaluating Model...
    print('Model Evaluation...')
    pred <- forecast$yhat[1:2954]
    
    actual <- prophet.model$history$y
    
    plot(actual, pred)
    abline(lm(pred~actual), col = 'red')
    print(summary(lm(pred~actual)))
    
    cs<-cross_validation(prophet.model, 365, units = 'days')
    print(performance_metrics(cs, rolling_window = 0.1))
    
    plot_cross_validation_metric(cs, metric = 'rmse', rolling_window = 0.1)
    print('done...')
}

run.prophet.pipeline(data$high)

svm.model <- svm(data$high~index(data), type = 'eps-regression', kernel = 'radial', cost = 0.1, gamma = 1000)

nd <- 1:3200


predictions <- predict(svm.model, data = data.frame(x=nd))

ylim <- c(min(data$high), max(data$open))
xlim <- c(min(nd),max(nd))
plot(data$high, col="blue", ylim=ylim, xlim=xlim, type="l")
par(new=TRUE)
plot(predictions, col="red", ylim=ylim, xlim=xlim)

RMSE(predictions, data$high)

