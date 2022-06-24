# We will use un modele MA(13) model to predict Xt (the lest 12 test values)

mod13 <- arima(logairfit, order = c(0,0,13))

print(mod13)

tsdiag(mod13)

residu13 <- mod13$residuals
qqnorm(residu13, main = "Normal Q-Q Plot", xlab = "Theoretical Quantiles", 
       ylab = "Sample Quantiles", plot.it = TRUE)
grid()

qqline(residu13, probs=c(0.1,0.9), col="red", lwd=2)

#Prevision for the logarithm of the serie

plot(logair, type='o', xlab='Time', ylab='Log Nb Passengers',
     main =" MA(13) prevision and real values", ylim=c(4.5,6.8))
grid()

prev13 <- predict(mod13, n.ahead=12, prediction.interval=T)
lines( ts(prev13$pred, start=c(1960,1), freq=12), type='o', col='red', lwd=2 )
lines( ts(prev13$pred + 2*prev13$se, start=c(1960,1), freq=12),  col='blue', lwd=2 )
lines( ts(prev13$pred - 2*prev13$se, start=c(1960,1), freq=12),  col='blue', lwd=2 )

#Prevision for the serie

a13 <- matrix(0, nrow = 1, ncol = 12)
b13 <- matrix(0, nrow = 1, ncol = 12)
c13 <- matrix(0, nrow = 1, ncol = 12)

for (k in 1:12){
        a13[k] <- exp(prev13$pred[k])
        b13[k] <- exp(prev13$pred[k] + 2*prev13$se[k])
        c13[k] <- exp(prev13$pred[k] - 2*prev13$se[k])
}

plot( airline , type='o', xlab='Année', ylab='Passagers (en milliers)',
      main ="MA(13) prevision of aerian trafic and real values", ylim=c(100,700))
grid()

lines( ts( a13[1,] , start=c(1960,1), freq=12), type='o', col='red', lwd=2 )
lines( ts( b13[1,] , start=c(1960,1), freq=12),  col='blue', lwd=2 )
lines( ts( c13[1,] , start=c(1960,1), freq=12),  col='blue', lwd=2 )

#RMSE

ereal <- airline[(nair - 11):nair]
epred <- exp(prev13$pred)
RMSE <- sqrt(sum((ereal-epred)^2)/12)
print(RMSE)
