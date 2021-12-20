library(forecast)
library(zoo)


df<- read.csv("Australianwines.csv")
ac_x<-ts(df$Red,start=c(1980,1),end=c(1994,12),frequency=12)
plot(ac_x)
ac_x

#linear trend model
ac_lt<- tslm(ac_x~trend)
summary(ac_lt)

nvalid<-24
ntrain<-length(ac_x)-nvalid

train_ts <- window(ac_x,start=c(1980,1),end=c(1980,ntrain))
valid_ts <- window(ac_x,start=c(1980,ntrain+1),end=c(1980,ntrain+nvalid))

#Train and model metrics
ac_train_lt<-tslm(train_ts~trend)
summary(ac_train_lt)
ac_ts_pred<- forecast(ac_train_lt,h=nvalid,level=0)
accuracy(ac_ts_pred,valid_ts)

#Visualization 
par(mfrow = c(1, 1))
plot(ac_ts_pred, ylim = c(460, 2700),  ylab = "Red", xlab = "Time", 
     bty = "l", xaxt = "n", xlim = c(1980,1994),main = "", flty = 2)
axis(1, at = seq(1980, 1994, 1), labels = format(seq(1980, 1994, 1)))
lines(ac_ts_pred$fitted, lwd = 2, col = "blue")
lines(valid_ts)


# Seasonal

ac_train_s<-tslm(train_ts~season)
summary(ac_train_s)
ac_s_pred<- forecast(ac_train_s,h=nvalid,level=0)
accuracy(ac_s_pred,valid_ts)

#visualization
par(mfrow = c(1, 1))
plot(ac_s_pred, ylim = c(460, 2700),  ylab = "Red", xlab = "Time", 
     bty = "l", xaxt = "n", xlim = c(1980,1994),main = "", flty = 2)
axis(1, at = seq(1980, 1994, 1), labels = format(seq(1980, 1994, 1)))
lines(ac_s_pred$fitted, lwd = 2, col = "blue")
lines(valid_ts)

#trend + seasonal

ac_train_lt_s<-tslm(train_ts~ trend+ I(trend^2) +season)
summary(ac_train_lt_s)
ac_t_s_pred<- forecast(ac_train_lt_s,h=nvalid, level=0)
accuracy(ac_t_s_pred,valid_ts)

#visualization
par(mfrow = c(1, 1))
plot(ac_t_s_pred, ylim = c(460, 2700),  ylab = "Red", xlab = "Time", 
     bty = "l", xaxt = "n", xlim = c(1980,1994),main = "", flty = 2)
axis(1, at = seq(1980, 1994, 1), labels = format(seq(1980, 1994, 1)))
lines(ac_t_s_pred$fitted, lwd = 2, col = "blue")
lines(valid_ts)

#Simple exponential smoothing
ac_ses <- ses(train_ts, alpha = 0.2, h=24)
autoplot(ac_ses)
accuracy(ac_ses,valid_ts)
# Use ses function to estimate alpha
ac_ses1 <- ses(train_ts, alpha = NULL, h=24)
summary(ac_ses1)
accuracy(ac_ses1,valid_ts)