# We will do a first simple transformation by using the logarithm of the values

logair <- log(airline)
op <- par(mfrow = c(2,1))
plot(airline,type='o', main = "Initial serie")
grid()
plot(logair, type='o', main = "log(x)")
grid()
par(op)

#We remove the linear tendency by simple diffrenciation

difflogair <- diff(logair)
plot(difflogair, type='o', main = "diffenreciated log(x) serie",
     xlab="Temps", ylab = expression(paste("(",I-B,")",log(x[t])) ))
grid()
abline(h=0, col="red", lwd=2)

# We make a seasonal differenciation to remove the perdiodic composant

diff2logair <- diff(difflogair, lag=12)
op <- par(cex.lab = 0.8)
plot(diff2logair, type='o', main = "simple diffrenciation and seasonal differenciation",
     xlab="Time", ylab = expression(paste("(",I-B^12,")","(",I-B,")",log(x[t]))))
grid()
par(op)
abline(h=0, col="red", lwd=2)

# We rename the obtained serie (without tendency and seasonnality = dts) 

airdts <- diff2logair

# ACF and PACF of airdts

op <- par(mfrow = c(1,2))
airdts <- as.vector(airdts)
ro <- acf(airdts , lag=25, ylim = c(-1,1), main = expression("ACF stationnary seri"),
          xlab="Lag (in months)",lwd=2)
grid()
alpha <- pacf(airdts , lag=25, ylim = c(-1,1), main = expression("PACF"), 
              xlab="Lag (in months)", lwd=2)
grid()
par(op)

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

#RMSE <- 0
#for(k in 1:12){
#        RMSE <- RMSE + (airline[nair -12 + k] - a[k])^2 
#}
#RMSE <- RMSE/12
#RMSE

ereal <- airline[(nair - 11):nair]
epred <- exp(prevision$pred)
RMSE <- sqrt(sum((ereal-epred)^2)/12)
print(RMSE)



# Question 2
#On va utliser un modele MA(13) pour la prédiction des Xt

mod13 <- arima(logairfit, order = c(0,0,13))
print(mod13)
tsdiag(mod13)

residu13 <- mod13$residuals
qqnorm(residu13, main = "Normal Q-Q Plot", xlab = "Theoretical Quantiles", 
       ylab = "Sample Quantiles", plot.it = TRUE)
grid()
qqline(residu13, probs=c(0.1,0.9), col="red", lwd=2)

#Prévision pour le logarithme de ma série

plot(logair, type='o', xlab='Temps', ylab='Log Nbre Passagers',
     main ="Prévision MA(13) et valeurs réelles", ylim=c(4.5,6.8))
grid()

prev13 <- predict(mod13, n.ahead=12, prediction.interval=T)

lines( ts(prev13$pred, start=c(1960,1), freq=12), type='o', col='red', lwd=2 )
lines( ts(prev13$pred + 2*prev13$se, start=c(1960,1), freq=12),  col='blue', lwd=2 )
lines( ts(prev13$pred - 2*prev13$se, start=c(1960,1), freq=12),  col='blue', lwd=2 )

#prévision pour la série

a13 <- matrix(0, nrow = 1, ncol = 12)
b13 <- matrix(0, nrow = 1, ncol = 12)
c13 <- matrix(0, nrow = 1, ncol = 12)
for (k in 1:12){
        a13[k] <- exp(prev13$pred[k])
        b13[k] <- exp(prev13$pred[k] + 2*prev13$se[k])
        c13[k] <- exp(prev13$pred[k] - 2*prev13$se[k])
}


plot( airline , type='o', xlab='Année', ylab='Passagers (en milliers)',
      main ="Prévision MA(13) du trafic aérien et valeurs réelles", ylim=c(100,700))
grid()



lines( ts( a13[1,] , start=c(1960,1), freq=12), type='o', col='red', lwd=2 )
lines( ts( b13[1,] , start=c(1960,1), freq=12),  col='blue', lwd=2 )
lines( ts( c13[1,] , start=c(1960,1), freq=12),  col='blue', lwd=2 )

#Calcul du RMSE

RMSE <- 0
for(k in 1:12){
        RMSE <- RMSE + (airline[nair -12 + k] - a13[k])^2 
}
RMSE <- RMSE/12
RMSE

ereal <- airline[(nair - 11):nair]
epred <- exp(prev13$pred)
RMSE <- sqrt(sum((ereal-epred)^2)/12)
print(RMSE)

# Question 3 :
# On estime de manière paramétrique la tendance et saisonnalité du log de la série,
# et on traite la série obtenue comme une série stationnaire. On travaille sur 
# l'historique privé des 12 dernières valeurs pour tester à nouveau la qualité 
# de la prévision obtenue.


rm(list=ls()) # on efface tout

airline.data <- read.table("airline.dat")
airline <- airline.data$V1
airline <- ts(airline, start = c(1949,1), freq = 12)

logair <- log(airline)
nair <- length(logair)

# Données pour estimer le modèle (on enlève les 12 dernières valeurs)

logairfit <- logair[1:(nair-12)]
logairfit <- ts(logairfit, start=c(1949,1), freq=12)

# On estime tendance et saisonnalité à l'aide d'un modèle linéaire

logairfitv <- as.vector(logairfit)

# prédicteurs avec predict1 qui est la variable temps

predict1 <- 1:(nair-12)
predict2 <- cos(2*pi*predict1/12)
predict3 <- sin(2*pi*predict1/12)

# On utilise la fonction lm de R pour estimer le modèle linéaire

mod.lm <- lm( logairfit ~ predict1 + predict2 + predict3 )


# on en déduit la tendance et on visualise

tend <- mod.lm$coef[1] + mod.lm$coef[2]*predict1
tend <- ts(tend, start=c(1949,1), freq=12)
plot(logairfit, type='o', xlab="Temps",
     ylab="Log Nbre Passagers",main="Tendance estimée")
lines(tend, col='red',lwd=2)

# On calcule la saisonnalité 

sais <- mod.lm$coef[3]*predict2 + mod.lm$coef[4]*predict3
sais <- ts(sais, start=c(1949,1), freq=12)
lines(tend + sais, col='blue',lwd=2)

# Série log(airline) sans tendance et composante saisonnière

logairdts <- logairfit - (tend + sais)
logairdts <- ts(logairdts, start=c(1949,1), freq=12)
op <- par(cex.lab = 0.8)
plot(logairdts, type='o', main = "Résidus estimés avec le modèle linéaire", xlab="Année")
par(op)
abline(0, 0, col="red", lwd=2)

# ACF et PACF de logairdts 

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

# on prédit les 12 dernières valeurs

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

plot( airline , type='o', xlab='Année', ylab='Passagers (en milliers)',
      main ="Prévision ARMAX du trafic aérien et valeurs réelles", ylim=c(100,700))
grid()



lines( ts( a3[1,] , start=c(1960,1), freq=12), type='o', col='red', lwd=2 )
lines( ts( b3[1,] , start=c(1960,1), freq=12),  col='blue', lwd=2 )
lines( ts( c3[1,] , start=c(1960,1), freq=12),  col='blue', lwd=2 )

#Calcul du RMSE


ereal <- airline[(nair - 11):nair]
epred <- exp(prev13$pred)
RMSE <- sqrt(sum((ereal-a3)^2)/12)
print(RMSE)


#Question 4

#On va utiliser la méthode de Holt-Winters sur le logarithme de la série temporelle

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
