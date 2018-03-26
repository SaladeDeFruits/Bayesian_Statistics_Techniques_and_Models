library("car")
data("Leinhardt")

Leinhardt$loginfant = log(Leinhardt$infant)
Leinhardt$logincome = log(Leinhardt$income)
dat = na.omit(Leinhardt)

library("rjags")
mod1_string = "model{
        for(i in 1:n){
                y[i] ~ dnorm(mu[i], prec)
                mu[i] = b[1] + b[2]*log_income[i]
        }
        for(j in 1:2){
                b[j] ~ dnorm(0.0, 1.0/1.0e6)
        }
        # precision comes from gamma distribution
        # the syntax is according to JAGS
        # 
        prec ~ dgamma(5.0/2.0, 5.0*10.0/2.0)
        sig2 = 1.0/prec
        sig = sqrt(sig2)
        
}"

mod2_string = "model{
        for(i in 1:length(y)){
        y[i] ~ dnorm(mu[i], prec)
        mu[i] = b[1] + b[2]*log_income[i]+b[3]*is_pil[i]
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



set.seed(72)
data1_jags = list(y = dat$loginfant, 
                  n = nrow(dat),
                  log_income = dat$logincome)

#Told which parameters we woul
params1 = c("b", "sig")


inits1 = function(){
        inits = list("b"= rnorm(2, 0.0, 100.0),
                     "prec"= rgamma(1, 1.0, 1.0))
}

# We could run 3 different chains
mod1 = jags.model(textConnection(mod1_string), data = data1_jags, inits = inits1, n.chains = 3)

# Run the model for 1000, but not keep the samples
update(mod1, 1000)

# create the acutal simulation which we would like to keep
mod1_sim = coda.samples(model = mod1, variable.names = params1,
                        n.iter = 5e3)

mod1_csim = do.call(rbind, mod1_sim)



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



X = cbind( rep(1.0, data1_jags$n), data1_jags$log_income)
pm_params1 = colMeans( mod1_csim )

# drop method could change the result from a matrix to a vector
yhat1 = drop(X %*% pm_params1[1:2])
resid1 = data1_jags$y - yhat1
plot(resid1)

qqnorm(resid1)

plot(yhat1, resid1)
# we see two outliers
# 
#head(rownames(dat)[ order(resid1, decreasing = TRUE)]) 
rownames(dat)[ order(resid1, decreasing = TRUE)]

