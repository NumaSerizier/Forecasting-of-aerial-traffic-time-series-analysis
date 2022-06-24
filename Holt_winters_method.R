#We use the Holt Winters method on the logarithm of the temporal serie

HTair <- HoltWinters(logairfit, seasonal = "mult")
htf <- HTair$fitted
htf1 <- HTair$fitted[,1]

predht <- predict(HTair, n.ahead=12, prediction.interval=T)

aht <- matrix(0, nrow = 1, ncol = 12)
bht <- matrix(0, nrow = 1, ncol = 12)
cht <- matrix(0, nrow = 1, ncol = 12)

for (k in 1:12){
  aht[k] <- exp(predht[k,1])
#  bht[k] <- exp(predht$fit[k] + 2*predht$upt[k])
#  cht[k] <- exp(predht$fit[k] - 2*predht$lwr[k])
}

plot( airline , type='o', xlab='Année', ylab='Passagers (en milliers)',
      main ="Prévision Holt-Winters du trafic aérien et valeurs réelles", ylim=c(100,700))
grid()

lines( ts( aht[1,] , start=c(1960,1), freq=12), type='o', col='red', lwd=2 )
#lines( ts( bht[1,] , start=c(1960,1), freq=12),  col='blue', lwd=2 )
#lines( ts( cht[1,] , start=c(1960,1), freq=12),  col='blue', lwd=2 )

ereal <- airline[(nair - 11):nair]
epred <- exp(prev13$pred)
RMSE <- sqrt(sum((ereal-aht)^2)/12)
print(RMSE)
