# Auto Regressive Moving Average with eXogeneous inputs method

# We estime with parametric methods the tendency of the seasonnality of the logarithm of the serie,
# we process the obtain serie like a stationnary serie.
# We work on the private history of the last 12 values to test the quality of the obtain forecasting

airline.data <- read.table("airline.dat")
airline <- airline.data$V1
airline <- ts(airline, start = c(1949,1), freq = 12)

logair <- log(airline)

nair <- length(logair)

# Data to estimate the model (we remove the last 12 values)

logairfit <- logair[1:(nair-12)]
logairfit <- ts(logairfit, start=c(1949,1), freq=12)

# We estimate the seasonnality and the tendency with a linear model

logairfitv <- as.vector(logairfit)

# prédictors with predict1 as the time variable

predict1 <- 1:(nair-12)
predict2 <- cos(2*pi*predict1/12)
predict3 <- sin(2*pi*predict1/12)

# We use the lm function of R pour to estimate the linear model

mod.lm <- lm( logairfit ~ predict1 + predict2 + predict3 )

# We deduct the tendency and visualize

tend <- mod.lm$coef[1] + mod.lm$coef[2]*predict1
tend <- ts(tend, start=c(1949,1), freq=12)

plot(logairfit, type='o', xlab="Temps",
     ylab="Log Nbre Passagers",main="Tendance estimée")
lines(tend, col='red',lwd=2)

# We calculate the seasonnality 

sais <- mod.lm$coef[3]*predict2 + mod.lm$coef[4]*predict3
sais <- ts(sais, start=c(1949,1), freq=12)
lines(tend + sais, col='blue',lwd=2)

# log(airline) serie without tendency and seasonnality

logairdts <- logairfit - (tend + sais)
logairdts <- ts(logairdts, start=c(1949,1), freq=12)
op <- par(cex.lab = 0.8)

plot(logairdts, type='o', main = "Résidus estimés avec le modèle linéaire", xlab="Année")
par(op)
abline(0, 0, col="red", lwd=2)

# ACF and PACF of logairdts 

op <- par(mfrow = c(1,2))
logairdts2 <- as.vector(logairdts)

ro <- acf(logairdts2 , lag=25, ylim = c(-1,1), main = expression("ACF série stationnarisée"),
          xlab="Lag (en mois)", lwd=2)

alpha <- pacf(logairdts2 , lag=25, ylim = c(-1,1), main = expression("PACF"),
          xlab="Lag (en mois)", lwd=2)
par(op)

wt <- arima(logairdts, order = c(2,1,0), seasonal = list(order = c(0,1,1), pariod = 12))
print(wt)
tsdiag(wt)

prevres <- predict(wt, n.ahead=12, prediction.interval=T)

# we predict the last 12 values

t <- (nair-11):nair
t2 <- cos(2*pi*t/12)
t3 <- sin(2*pi*t/12)

a3 <- matrix(0, nrow = 1, ncol = 12)
b3 <- matrix(0, nrow = 1, ncol = 12)
c3 <- matrix(0, nrow = 1, ncol = 12)

for (k in 1:12){
        a3[k] <- exp(mod.lm$coef[1] + mod.lm$coef[2]*t[k] + mod.lm$coef[3]*t2[k] + mod.lm$coef[4]*t3[k] + prevres$pred[k])
        b3[k] <- exp(mod.lm$coef[1] + mod.lm$coef[2]*t[k] + mod.lm$coef[3]*t2[k] + mod.lm$coef[4]*t3[k] + prevres$pred[k] + 2*prevres$se[k])
        c3[k] <- exp(mod.lm$coef[1] + mod.lm$coef[2]*t[k] + mod.lm$coef[3]*t2[k] + mod.lm$coef[4]*t3[k] + prevres$pred[k] - 2*prevres$se[k])
}

plot( airline , type='o', xlab='Year', ylab='Passengers (in thousands)',
      main =" ARMAX prevision of the trafic and real values", ylim=c(100,700))
grid()

lines( ts( a3[1,] , start=c(1960,1), freq=12), type='o', col='red', lwd=2 )
lines( ts( b3[1,] , start=c(1960,1), freq=12),  col='blue', lwd=2 )
lines( ts( c3[1,] , start=c(1960,1), freq=12),  col='blue', lwd=2 )

#Calculation of the RMSE

ereal <- airline[(nair - 11):nair]
epred <- exp(prev13$pred)
RMSE <- sqrt(sum((ereal-a3)^2)/12)
print(RMSE)
