library(readxl)
setwd("C:/Vignesh/Studies/Spring 19/Healthcare Analytics/Cases/Fargo Health")
options(scipen = 999)

dt<-read_xlsx("Fargo Health - Data Supplement.xlsx",sheet = 2)
str(dt)

dt.nw<-dt
dt.nw$`Incoming Examinations`<-as.numeric(dt.nw$`Incoming Examinations`)
dt.nw$`Incoming Examinations`[dt.nw$`Incoming Examinations`>9999]<-NA

ts.fargo <- ts(dt.nw$`Incoming Examinations`, frequency=12, start=c(2006,1))
plot.ts(ts.fargo)


# Removing Na -------------------------------------------------------------

dtna<-na.omit(dt.nw)
tsna.fargo <- ts(dtna$`Incoming Examinations`, frequency=12, start=c(2006,1))
plot.ts(tsna.fargo)


# TS & Decomposing --------------------------------------------------------

library(TTR)
fargo.dec.ts<-decompose(tsna.fargo)
fargo.dec.ts$seasonal
plot(fargo.dec.ts)

fargo.dec.ts.adjusted<-tsna.fargo -fargo.dec.ts$seasonal
plot(fargo.dec.ts.adjusted)


#(2) Test the column for normality

# --- Layout
dev.off()
layout(matrix(1:6, 2, 3, byrow = TRUE))

# (2-1) Sharpiro test (p>0.05 is normal) of data, diff(data), and
# diff(log(data)) Requires a vector as input
n1.vtr <- as.numeric(fargo.dec.ts.adjusted)
p1 <- shapiro.test(n1.vtr)$p.value
p2 <- shapiro.test(diff(n1.vtr))$p.value
p3 <- shapiro.test(diff(log(n1.vtr)))$p.value

# (2-2) Plot histogram of data, diff(data), and diff(log(data))
hist(n1.vtr, prob = T, main = c("N.Examinations", paste("Shapiro p=", prettyNum(p1, digits = 2))), 
     xlab = "N.Examinations", xlim = c(1, 45))
lines(density(n1.vtr))
hist(diff(n1.vtr), prob = T, main = c("diff(N.Examinations)", paste("Shapiro p=", prettyNum(p2, 
                                                                                            digits = 2))), xlab = "diff(N.Examinations)")
lines(density(diff(n1.vtr)))
hist(diff(log(n1.vtr)), prob = T, main = c("diff(log(N.Examinations))", paste("Shapiro p=", 
                                                                              prettyNum(p3, digits = 2))), xlab = "diff(log(N.Examinations))")
lines(density(diff(log(n1.vtr))))

# (2-3) QQ plot of the above
qqnorm(n1.zoo)
qqnorm(diff(n1.zoo))
qqnorm(diff(log(n1.zoo)))

#(3) Test the column for autocorrelation
# --- Layout
layout(matrix(1:3, 1, 3, byrow = TRUE))


# (3-1) Acf plot
acf(n1.zoo, main = "N.Examinations",na.action = na.pass )
acf(diff(n1.zoo), main = "diff(N.Examinations)", na.action = na.pass)
acf(diff(log(n1.zoo)), main = "diff(log(N.Examinations))", na.action = na.pass)


# (3-1) PAcf plot
pacf(n1.zoo, main = "N.Examinations",na.action = na.pass)
pacf(diff(n1.zoo), main = "diff(N.Examinations)", na.action = na.pass)
pacf(diff(log(n1.zoo)), main = "diff(log(N.Examinations))", na.action = na.pass)

#(4) Test the column for stationary
# (4-1) Augmented Dickey-Fuller (ADF) test The null-hypothesis for an ADF
# test is that the data is non-stationary Therefore, p>0.05 is
# non-stationary
library(tseries)
# (4-1) Augmented Dickey-Fuller (ADF) test The null-hypothesis for an ADF
# test is that the data is non-stationary Therefore, p>0.05 is
# non-stationary
adf.test(n1.zoo, alternative = "stationary")

adf.test(diff(n1.zoo), alternative = "stationary")

adf.test(diff(log(n1.zoo)), alternative = "stationary")

library(forecast)
#(5) Test the column for seasonality
# (5-1) Seasonal differencing test If ns>0, then seasonal differencing is
# required
nsdiffs(n1.zoo)
nsdiffs(diff(n1.zoo))
nsdiffs(diff(log(n1.zoo)))

library(forecast)
# ARIMA--------------------------------------------------------------
tsna.fargo<-n1.zoo
tsna.fargo.diff<-diff(tsna.fargo,differences = 1) #
plot.ts(tsna.fargo.diff)
plot.ts(tsna.fargo)

auto.arima(tsna.fargo,ic="bic",trace=TRUE)

tsna.fargo.arima <- arima(tsna.fargo, order=c(1,2,1),method = "ML") # 
tsna.fargo.arima

tsna.fargo.arima.forecast <- forecast:::forecast.Arima(tsna.fargo.arima, h=12)
tsna.fargo.arima.forecast
plot(tsna.fargo.arima.forecast)

plot.ts(tsna.fargo.arima.forecast$residuals)
hist(tsna.fargo.arima.forecast$residuals, col="green", freq=FALSE, breaks=20)
lines(density(tsna.fargo.arima.forecast$residuals,na.rm = T),col='blue')
