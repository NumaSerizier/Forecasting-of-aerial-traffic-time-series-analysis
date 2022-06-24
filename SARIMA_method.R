# Adjusting a SARIMA(0,1,1)(0,1,1) model with seasonnality s = 12

model <- arima(logair, order = c(0,1,1), seasonal = list(order=c(0,1,1), period = 12))
print(model)

# Validation of the model
# tsdiag() function in R

tsdiag(model)

# Extracting residus

residus <- model$residuals
qqnorm(residus, main = "Normal Q-Q Plot", xlab = "Theoretical Quantiles", 
       ylab = "Sample Quantiles", plot.it = TRUE)
grid()
qqline(residus, probs=c(0.1,0.9), col="red", lwd=2)

# Back-testing of SARIMA model : we remove the last 12 values that we search to predict
# We compare with the real values of the serie
# We re-estime the values

nair <- length(airline)
airfit <- airline[1:(nair - 12)]	# we remove the last 12 values
logairfit <- ts(log(airfit), start = c(1949,1), freq = 12)

# Ajusting a SARIMA(0,1,1)(0,1,1) model with s = 12 on the learning data 

model <- arima(logairfit, order = c(0,1,1), seasonal = list(order=c(0,1,1), period = 12))
print(model)
tsdiag(model)

residus <- model$residuals
qqnorm(residus, main = "Normal Q-Q Plot", xlab = "Theoretical Quantiles", 
       ylab = "Sample Quantiles", plot.it = TRUE)
grid()
qqline(residus, probs=c(0.1,0.9), col="red", lwd=2)

# forecasting for one year, h =12

logair <- log(airline)
plot(logair, type='o', xlab='Time', ylab='Log nbr passengers',
     main ="SARIMA forecasting and real values", ylim=c(4.5,6.8))
grid()
prevision <- predict(modele, n.ahead=12, prediction.interval=T)

lines( ts(prevision$pred, start=c(1960,1), freq=12), type='o', col='red', lwd=2 )
lines( ts(prevision$pred + 2*prevision$se, start=c(1960,1), freq=12),  col='blue', lwd=2 )
lines( ts(prevision$pred - 2*prevision$se, start=c(1960,1), freq=12),  col='blue', lwd=2 )

# Back-testing on the initail serie (without the logarithm transformation)

a <- matrix(0, nrow = 1, ncol = 12)
b <- matrix(0, nrow = 1, ncol = 12)
c <- matrix(0, nrow = 1, ncol = 12)
for (k in 1:12){
        a[k] <- exp(prevision$pred[k])
        b[k] <- exp(prevision$pred[k] + 2*prevision$se[k])
        c[k] <- exp(prevision$pred[k] - 2*prevision$se[k])
}
plot( airline , type='o', xlab='Year', ylab='Passengers (in thousands)',
     main ="SARIMA prevision of the aerian trafic and real values", ylim=c(100,700))
grid()

# calculating RMSE for comparing methods

ereal <- airline[(nair - 11):nair]
epred <- exp(prevision$pred)
RMSE <- sqrt(sum((ereal-epred)^2)/12)
print(RMSE)
