library("rjags")
library("car")
data("Anscombe")

dat = na.omit(Anscombe)

mod_string = " model {
for (i in 1:n) {
education[i] ~ dnorm(mu[i], prec)
mu[i] = b0 + b[1]*income[i] + b[2]*young[i] + b[3]*urban[i]
}

b0 ~ dnorm(0.0, 1.0/1.0e6)
for (i in 1:3) {
b[i] ~ dnorm(0.0, 1.0/1.0e6)
}

prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
sig2 = 1.0 / prec
sig = sqrt(sig2)
} "

#data_jags = as.list(Anscombe)

data_jags = list( n = nrow(dat),
                  education = dat$education,
                  income = dat$income,
                  young = dat$young,
                  urban = dat$urban)
params = c("b", "sig")
inits = function(){
        inits = list( "b"= rnorm(3, 0.0, 100.0),
                      "prec" = rgamma(1, 1.0, 1.0))
}
mod = jags.model( textConnection(mod_string),
                  data = data_jags,
                  inits = inits,
                  n.chains = 3)
#update(mod, 1000)

dic.samples(mod, n.iter = 1e5)
mod_sim = coda.samples(model = mod, 
                        variable.names = params,
                        n.iter = 5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))
plot(mod_csim)
summary(mod_csim)



mod_string1 = " model {
        for (i in 1:n) {
                education[i] ~ dnorm(mu[i], prec)
                mu[i] = b0 + b[1]*income[i] + b[2]*young[i] + b[3]*income[i]*young[i]
                }
        b0 ~ dnorm(0.0, 1.0/1.0e6)
        for (i in 1:3) {
                b[i] ~ dnorm(0.0, 1.0/1.0e6)
                }
        prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
        sig2 = 1.0 / prec
        sig = sqrt(sig2)
}  "

data_jags1 = list( y = dat$education,
                  n = nrow(dat),
                  education = dat$education,
                  income = dat$income,
                  young = dat$young)
params1 = c("b", "sig")
inits1 = function(){
        inits = list( "b"= rnorm(3, 0.0, 100.0),
                      "prec" = rgamma(1, 1.0, 1.0))
}
mod1 = jags.model( textConnection(mod_string1),
                  data = data_jags1,
                  inits = inits1,
                  n.chains = 3)
update(mod1, 1000)
dic.samples(mod1, n.iter = 1e5)

mod_string2 = " model {
        for (i in 1:n) {
        education[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + b[1]*income[i] + b[2]*young[i]
        }
        b0 ~ dnorm(0.0, 1.0/1.0e6)
        for (i in 1:2) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
        }
        prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
        sig2 = 1.0 / prec
        sig = sqrt(sig2)
        }  "

data_jags2 = list( n = nrow(dat),
                   education = dat$education,
                   income = dat$income,
                   young = dat$young)
params2 = c("b", "sig")
inits2 = function(){
        inits = list( "b"= rnorm(2, 0.0, 100.0),
                      "prec" = rgamma(1, 1.0, 1.0))
}
mod2 = jags.model( textConnection(mod_string2),
                   data = data_jags2,
                   inits = inits2,
                   n.chains = 3)
update(mod2, 1000)
dic.samples(mod2, n.iter = 1e5)











