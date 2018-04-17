
setwd("F:/GoogleDrive/WeatherDataPrediction")
mydata <- read.csv("weather_osaka_2016_2017_2018_en.csv", encoding = "JIS", header= TRUE)
mydata$date <- c(1:nrow(mydata))
raindata <- subset(mydata, mydata$precipitation>0)

par(mfrow=c(3,5))
plot(x = mydata$date, y = mydata$AvrTemp, col="red")
plot(x = mydata$date, y = 1/mydata$AverLandPressure, col="purple")
plot(x = raindata$date, y = log(raindata$precipitation+1), col="blue")
plot(x = mydata$date, y = mydata$windspeed, col="green")
plot(x = mydata$date, y = mydata$Humidity, col="green")

utils::str(hist(mydata$AvrTemp, col = "gray", breaks = 20))
utils::str(hist(1/mydata$AverLandPressure, col = "gray", breaks = 20))
utils::str(hist(log(raindata$precipitation+1), col = "gray", breaks = 20))
utils::str(hist(mydata$windspeed, col = "gray",  breaks = 20))
utils::str(hist(mydata$Humidity, col = "gray",breaks = 20))

plot(density(mydata$AvrTemp))
plot(density(1/mydata$AverLandPressure))
plot(density(log(raindata$precipitation+1)))
plot(density(mydata$windspeed))
plot(density(mydata$Humidity))

summary(mydata$AvrTemp)
summary(1/mydata$AverLandPressure)
summary(log(raindata$precipitation+1))
summary(mydata$windspeed)
summary(mydata$Humidity)
#utils::str(hist(log(raindata$precipitation+1), col = "gray", labels = TRUE, breaks = 20))
#utils::str(hist(mydata$windspeed, col = "gray", labels = TRUE, breaks = 20))
#utils::str(hist(mydata$Humidity, col = "gray", labels = TRUE, breaks = 20))
#utils::str(hist(mydata$AverLandPressure, col = "gray", labels = TRUE, breaks = 20))
#
#
par(mfrow=c(1,3))
plot(x = mydata$date, y = mydata$rain, col="blue")
plot(x = mydata$rain, y = mydata$AverLandPressure)
utils::str(hist(mydata$rain, col = "gray",main = 'Possibility of rain is 0.29'))

plot(mydata$date, mydata$rain)
utils::str(hist(mydata$rain, col = "gray",main = 'Possibility of rain is 0.29'))
mean(mydata$rain)



library("rjags")
mod_string = "model{
        for(i in 1: length(y)){
                y[i] ~ dnorm( mumid[i], prec)
                z[i] ~ dunif(1,2)
                mumid[i] = (int +b[1]*pressure_inv[i]+b[2]*precipitation_log[i]
                        +b[3]*windspeed[i]+b[4]*humidity[i])*z[i]
        }
        int ~ dnorm(0.0, 1.0/25.0)
        for (j in 1:4) {
                b[j] ~ ddexp(0.0, sqrt(2.0)) 
        }
        prec ~ dgamma(1.0/2.0, 1.0*1.0/2.0)
        sig = sqrt(1.0/prec)
        }"
data_jags = list( y = mydata$AvrTemp,
                  pressure_inv = 1/mydata$AverLandPressure,
                  precipitation_log = log(mydata$precipitation+1),
                  windspeed = mydata$windspeed,
                  humidity = mydata$Humidity)

params = c( "b", "int","mumid")
mod = jags.model(textConnection(mod_string),
                 data = data_jags,
                 n.chains = 3)
update(mod, 1e3)
mod_sim = coda.samples(model = mod,
                       variable.names = params,
                       n.iter = 5e3)
mod_csim = as.mcmc( do.call(rbind, mod_sim) )

## convergence diagnostics
plot(mod_sim, ask=TRUE)
gelman.diag(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)
effectiveSize(mod_sim)

mod0_string = "model{
        for(i in 1:length(temp)){
        temp[i] ~ dnorm(mu[i], prec)
        mu[i] =b[1] + b[2]*pressure[i]+ b[3]*humidity[i] + b[4]*windspeed[i]
        }
        for(j in 1:4){
                b[j] ~ dnorm(0.0, 1.0/1.0e6)
        }
        prec ~ dgamma(5/2.0, 5*10.0/2.0)}"
data0_jags = list( temp = mydata$AvrTemp,
                   pressure = mydata$AverLandPressure,
                   humidity = mydata$Humidity,
                   windspeed = mydata$windspeed)

params0 = c( "b", "prec")
mod0 = jags.model(textConnection(mod0_string),
                 data = data0_jags,
                 n.chains = 3)
update(mod0, 1e3)
mod0_sim = coda.samples(model = mod0,
                       variable.names = params0,
                       n.iter = 5e3)
mod0_csim = as.mcmc( do.call(rbind, mod_sim) )


## compute DIC
dic = dic.samples(mod, n.iter=1e3)
dic
dic0 = dic.samples(mod0, n.iter=1e3)
dic0

summary(mod_sim)
HPDinterval(mod_csim)
summary(mod0_sim)
HPDinterval(mod0_csim)
