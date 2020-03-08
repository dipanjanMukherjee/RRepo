library(AnomalyDetection)
library(xts)
library(lubridate)

AnomalyDetectionTS(mdeaths, direction = 'both', max_anoms = 0.02, plot = TRUE)

days_tble <- table(cut(df$loggedtime, breaks = "day"))

xtsdays <- xts(x$Freq, as.Date(x$var1)

