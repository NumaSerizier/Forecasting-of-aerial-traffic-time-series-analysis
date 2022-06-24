# Charging the data and putting them on Time Series form with ts
# mensual series from january 1949 to december 61, 12*12 = 144 values

airline.data <- read.table("airline.dat")
airline <- airline.data$V1
airline <- ts(airline, start = c(1949,1), freq = 12)

# Chronogram and legends...

plot(airline, type='o', xlab="Date", ylab="Passengers (in thousands)", 
     main="Aerian international trafic from january 1949 to dec. 1960)
grid()

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
