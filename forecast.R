library(forecast)
library(tseries)

count_ts <- ts(daily_data[,c('value)])
daily_data$cnt_ma <- ma(daily_data$clean_cnt, order = 7)
count_ma <- ts(na.omit(daily_data$cnt_ma), frequency = 30)

decomp <- stl(count_ma, s.window = "periodic")

deseasonal_cnt <- seasadj(decomp)
plot(decomp)

adf.test(count_ma, alternative = "stationary")

Acf(count_ma, main = '')

Acf(count_d1, main = 'Acf for differenced series')
Pacf(count_d1)

count_d1 <- diff(deseasonal_cnt, differences = 1)
plot(count_d1)

adf.test(count_d1, alternative = 'Stationary')

#seasonal false
auto.arima(deseasonal_cnt, seasonal = FALSE)

fit <- auto.arima(deseasonal_cnt, seasonal = FALSE)

tsdisplay(residuals(fit), lag.max = 45, main = '(1,1,1) model residuals')

fit2 >- arima(deseasonal_cnt, order = c(1,1,5))

tsdisplay(residuals(fit2) lag.max - 15, main = 'Seasonal Model Residuals')

fcast <- forecast(fit2, h = 60)

plot(fcast)

#seasonal true
fit_w_seasonality <- auto.arima(deseasonal_cnt, seasonal = TRUE)

seas_fcast <- forecast(fit_w_seasonality, h = 30)

plot(seas_fcast)

#hold 
hold <- window(ts(deseasonal_cnt), start = 8)

fit_no_holdout <- arima(ts(deseasonal_cnt[-c(8:10)]), order = c(1,1,7))

fcast_no_holdout <- forecast (fit_No_holdout, h = 25)

plot(fcast_no_holdout, main = " ")

lines(ts(deseasonal_cnt))

