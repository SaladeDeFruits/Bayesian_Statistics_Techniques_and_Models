setwd("F:/GoogleDrive/Bayesian_Statistics_Techniques_and_Models")
dat = read.csv(file="callers.csv", header=TRUE)
head(dat)
any(is.na(dat))
hist(dat$calls)
hist(dat$days_active)
hist(dat$isgroup2)
hist(dat$age)

boxplot( dat$calls/dat$days_active ~ dat$age)
boxplot( dat$age ~ dat$isgroup2)
boxplot( dat$calls/dat$days_active ~ dat$isgroup2)
boxplot( dat$calls ~ dat$isgroup2)


library("rjags")
mod_string =" model{
        for(i in 1: length(calls)){
               calls[i] ~ dpois(days_active[i]* lam[i])
                log( lam[i]) = b0+ b[1]*age[i]+b[2]*isgroup2[i]
        }
        b0 ~ dnorm(0.0, 100)
        for(j in 1:2){
                b[j] ~dnorm(0.0, 100)
        }
        }"
data_jags = as.list(dat)
params = c("b0","b")
mod = jags.model(textConnection(mod_string),
                 data = data_jags,
                 n.chain =3)
update(mod, 1e3)
mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))


# convergence diagnostics
plot(mod_sim)
summary( mod_sim)
gelman.diag(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)
effectiveSize(mod_sim)

## compute DIC
dic = dic.samples(mod, n.iter=1e3)
dic


#Residual Checking
X = as.matrix(dat)
X
