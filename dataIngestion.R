# install.packages('ggplot2')
# install.packages('pdfetch')
# install.packages('quantmod')
# install.packages('TTR')
# install.packages('Zoo')
# install.packages('statsmodels')
library(zoo)
library(ggplot2)
library(pdfetch)
library(quantmod)
library(TTR)
library(lubridate)
library(plotrix)
library(stats)

data = getSymbols('BTC-USD', src = 'yahoo',auto.assign = FALSE)

head(index(data))
plot(zoo(data), plot.type = 'multiple',col = c('black','blue','green','yellow','orange','purple'))

data <- zoo(data, order.by = index(data), frequency = 52)

data$month <- month(as.POSIXlt(index(data), format="%d/%m/%Y"))
data$year <- year(as.POSIXlt(index(data), format = "%d/%m/%Y"))

myarrow=arrow(angle = 15, ends = "both", type = "closed")
ggplot(data = data, aes(x = index(data),))+
    geom_line(aes(y = data$BTC.USD.Open, color = 'green'))+
    geom_line(aes(y = data$BTC.USD.Close, color = 'yellow'))

# ggplot(data = data, aes(x = data$year)+
#     geom_line()
ggplot(data = data, aes(x = index(data),))+
    geom_line(aes(y = data$BTC.USD.High, color = 'green'))+
    geom_line(aes(y = data$BTC.USD.Low, color = 'yellow'))

btc.2017 <- data[data$year == 2017]
btc.2018 <- data[data$year == 2018]
btc.2019 <- data[data$year == 2019]
btc.2020 <- data[data$year == 2020]
# Removing leap year leap day 29-02-2020 from the data to match 365 days...
btc.2020 <- btc.2020[!(format(index(btc.2020), format = "%m") =="02" &
             format(index(btc.2020), format = "%d")=="29"),]
btc.2021 <- data[data$year == 2021]
btc.2021 <- data[data$year == 2021]
btc.2022 <- data[data$year == 2022]

ggplot(data = btc.2017, aes(x = as.POSIXct(index(btc.2017), format="%d/%m/%Y")))+
    geom_line(aes(y = btc.2017$BTC.USD.Open, colour = '2017'))+
    geom_line(aes(y = btc.2018$BTC.USD.Open, colour = '2018'))+
    geom_line(aes(y = btc.2019$BTC.USD.Open, colour = '2019'))+
    geom_line(aes(y = btc.2020$BTC.USD.Open, colour = '2020'))+
    geom_line(aes(y = btc.2021$BTC.USD.Open, colour = "2021"))+
    # geom_line(aes(y = btc.2022$BTC.USD.Open, colour = "2022"))
    scale_colour_manual(name = "year", aesthetics = "colour",values = c("2017" = "darkblue", "2018" = "red", '2019' = 'orange', "2020" = 'purple', "2021" = 'green', "2022" = 'pink'))+
    scale_x_datetime(date_labels = "%b")+
    xlab('Month of year')+
    ylab('BTC-USD Open Price')+
    theme_minimal()

covid.period <- data['20200130/20220505']
ggplot(data = covid.period, aes(x = as.POSIXct(index(covid.period), format="%d/%m/%Y")))+
    geom_line(aes(y = covid.period$BTC.USD.Open, colour = 'covid'))+
scale_colour_manual(name = "year", aesthetics = "colour",values = c("covid" = "red"))+
    scale_x_datetime(date_labels = "%b", date_breaks = "2 month")+
    xlab('Month of year')+
    ylab('BTC-USD Open Price')+
theme_minimal()

#Inprogress....
components.ts <- decompose(data)
plot(components.ts)
# -------------------------------------------------

#Auto Correlation Model....
model <- lm(`BTC-USD.Adjusted` ~ ., data =data)
acf(model$residuals,plot = TRUE, type = 'correlation');
pacf(model$residuals, pl = TRUE)

model <- lm(`BTC-USD.High` ~ ., data =data)
acf(model$residuals,plot = TRUE, type = 'correlation');
pacf(model$residuals, pl = TRUE)

model <- lm(`BTC-USD.Low` ~ ., data =data)
acf(model$residuals,plot = TRUE, type = 'correlation');
pacf(model$residuals, pl = TRUE)

model <- lm(`BTC-USD.Open` ~ ., data =data)
acf(model$residuals,plot = TRUE, type = 'correlation');
pacf(model$residuals, pl = TRUE)

model <- lm(`BTC-USD.Close` ~ ., data =data)
acf(model$residuals,plot = TRUE, type = 'correlation');
pacf(model$residuals, pl = TRUE)

model <- lm(`BTC-USD.Volume` ~ ., data =data)
acf(model$residuals,plot = TRUE, type = 'correlation');
pacf(model$residuals, pl = TRUE)

#Analysis...


