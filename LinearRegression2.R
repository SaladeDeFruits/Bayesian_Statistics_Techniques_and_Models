library("car")
data("Leinhardt")
library("rjags")

Leinhardt$loginfant = log(Leinhardt$infant)
Leinhardt$logincome = log(Leinhardt$income)
dat = na.omit(Leinhardt)

mod1_string = " model {
    for (i in 1:n) {
        y[i] ~ dnorm(mu[i], prec)
        mu[i] = b[1] + b[2]*log_income[i] 
        }
     for (i in 1:2) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
        }
        prec ~ dgamma(5/2.0, 5*10.0/2.0)
        sig2 = 1.0 / prec
        sig = sqrt(sig2)
} "

set.seed(72)
data1_jags = list(y=dat$loginfant, n=nrow(dat), 
                  log_income=dat$logincome)
params1 = c("b", "sig")
inits1 = function() {
        inits = list("b"=rnorm(2,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}
mod1 = jags.model(textConnection(mod1_string), data=data1_jags, inits=inits1, n.chains=3)
update(mod1, 1000)
mod1_sim = coda.samples(model=mod1,
                        variable.names=params1,
                        n.iter=5000)
mod1_csim = do.call(rbind, mod1_sim) # combine multiple chains



mod2_string = "model{
        for(i in 1:length(y)){
                y[i] ~ dnorm(mu[i], prec)
                mu[i] = b[1] + b[2]*log_income[i]+b[3]*is_oil[i]
                }
        for(j in 1:3){
                b[j] ~ dnorm(0.0, 1.0/1.0e6)
                }
# precision comes from gamma distribution
# the syntax is according to JAGS
# 

        prec ~ dgamma(5.0/2.0, 5.0*10.0/2.0)
        sig2 = 1.0/prec
        sig = sqrt(sig2)
}"

set.seed(75)
data2_jags = list(y = dat$loginfant, 
                  n = nrow(dat),
                  log_income = dat$logincome,
                  is_oil= as.numeric(dat$oil == "yes"))

data2_jags$is_oil

#Told which parameters we woul
params2 = c("b", "sig")


inits2 = function(){
        inits = list("b"= rnorm(3, 0.0, 100.0),
                     "prec"= rgamma(1, 1.0, 1.0))
}

# We could run 3 different chains
mod2 = jags.model(textConnection(mod2_string), 
                  data = data2_jags, 
                  inits = inits2, n.chains = 3)

# Run the model for 1000, but not keep the samples
update(mod1, 1000)

# create the acutal simulation which we would like to keep
mod2_sim = coda.samples(model = mod2, variable.names = params2,
                        n.iter = 5e3)

mod2_csim = do.call(rbind, mod2_sim)

summary(mod2_sim)

### Convergence
plot(mod1_sim)
gelman.diag(mod1_sim)

#autocorrelation
autocorr.diag(mod1_sim)

# we test 15,000 data, and the effect data is only around 300
effectiveSize(mod1_sim)

lmod = lm(loginfant ~ logincome, data = Leinhardt)
summary(lmod)

#posterior summary from this model
summary(mod1_sim)
summary(lmod)

#Residuals
lmod0 = lm(infant ~ income, data = Leinhardt)
plot( resid(lmod0))
# the residual shows no pattern on the plot

plot( predict(lmod0), resid(lmod0))
# we would like to see randomness, which is not the case.
qqnorm(resid(lmod0))


# This step is to check the residuals 
X = cbind(rep(1.0, data1_jags$n), data1_jags$log_income)
(pm_params1 = colMeans(mod1_csim)) # posterior mean
yhat1 = drop(X %*% pm_params1[1:2])
resid1 = data1_jags$y - yhat1
plot(resid1) # against data index

X2 = cbind( rep(1.0, data2_jags$n), 
            data2_jags$log_income, 
            data2_jags$is_oil)
head(X2)

# Collect the posterior means for the parameters
# The parenthesis outside the parameter is to display the variables
(pm_params2 = colMeans( mod2_csim ))

# drop method could change the result from a matrix to a vector
yhat2 = drop(X2 %*% pm_params2[1:3])
resid2 = data2_jags$y - yhat2
par(mfrow = c(2,1))
plot(yhat2, resid2)
plot(yhat1, resid1)
sd(resid1)
sd(resid2)

curve( dnorm(x), from=-5, to=5 )
curve( dt(x,1), from=-5, to=5, col="red", add=TRUE
       )
# By comparing the graph, we could see the t-distribution has a thicker tail, it could tolerate more outliers

mod3_string = "model{
        for(i in 1:length(y)){
                y[i] ~ dt( mu[i], tau, df)
                mu[i] = b[1] + b[2]*log_income[i]+b[3]*is_oil[i]
                }
        for(j in 1:3){
                b[j] ~ dnorm(0.0, 1.0/1.0e6)
                }
        nu ~ dexp(1.0)
        df = nu + 2.0
        tau ~ dgamma(5.0/2.0, 5.0*10.0/2.0)
        sig = sqrt(1.0/tau * df/(df - 2.0))
}"

# degree of freedom df is related to the shape of the tail in the t-distribution
# the smaller the degree of freedom, the heavier the tail will be.

# a t distribution does not have a mean and a variance if the degree of freedom is less than 2

# df = nu + 2.0
# nu = dexp(1.0)
# The two lines above will guarantee that the degree of freedom is more than 2.
set.seed(75)
data3_jags = list(y = dat$loginfant, 
                  n = nrow(dat),
                  log_income = dat$logincome,
                  is_oil= as.numeric(dat$oil == "yes"))

data3_jags$is_oil

params3 = c("b","sig")


inits3 = function(){
        inits = list("b"= rnorm(3, 0.0, 100.0),
                     "nu" = rexp(1.0),
                     "tau"= rgamma(1, 1.0, 1.0))
}

# We could run 3 different chains
mod3 = jags.model(textConnection(mod3_string), 
                  data = data3_jags, 
                  inits = inits3,
                  n.chains = 3)
# Run the model for 1000, but not keep the samples
update(mod3, 1000)
# create the acutal simulation which we would like to keep
mod3_sim = coda.samples(model = mod3, 
                        variable.names = params3,
                        n.iter = 5e3)
mod3_csim = as.mcmc(do.call(rbind, mod3_sim)) # combine multiple chains
plot(mod3_sim)
gelman.diag(mod3_sim)
autocorr.diag(mod3_sim)
autocorr.plot(mod3_sim)
effectiveSize(mod3_sim)
summary(mod3_sim)

X3 = cbind( rep(1.0, data3_jags$n), 
            data3_jags$log_income, 
            data3_jags$is_oil)
head(X3)
(pm_params3 = colMeans( mod3_csim ))
yhat3 = drop(X3 %*% pm_params3[1:3])
resid3 = data3_jags$y - yhat3
par(mfrow = c(3,1))
plot(yhat1, resid1)
plot(yhat2, resid2)
plot(yhat3, resid3)
sd(resid1)
sd(resid2)
sd(resid3)


plot(resid1)
qqnorm(resid1)

plot(dat$loginfant ~ dat$logincome)

plot(yhat1, resid1)
# we see two outliers
#head(rownames(dat)[ order(resid1, decreasing = TRUE)]) 
rownames(dat)[ order(resid1, decreasing = TRUE)]

dic.samples(mod1, n.iter = 1e3)
dic.samples(mod2, n.iter = 1e3)
