library("COUNT")
data("badhealth")
?badhealth
head(badhealth)
any(is.na(badhealth))
hist(badhealth$numvisit, breaks=20)
plot(jitter(log(numvisit)) ~ jitter(age), data=badhealth, subset=badh==0&numvisit >0, xlab="age", ylab="log(visits)")
points(jitter(log(numvisit)) ~ jitter(age), data=badhealth, subset=badh==1&numvisit >0, col="red")



library("rjags")
mod_string = " model {

        for (i in 1:length(numvisit)){
        numvisit[i] ~ dpois(lam[i])
        log(lam[i]) = int + b_badh*badh[i] + b_age*age[i] + b_intx*age[i]*badh[i]
        }

        int ~ dnorm(0.0, 1.0/1e6)
        b_badh ~ dnorm(0.0, 1.0/1e4)
        b_age ~ dnorm(0.0, 1.0/1e4)
        b_intx ~ dnorm(0.0, 1.0/1e4)

        } "

set.seed(102)

data_jags = as.list(badhealth)

params = c("int", "b_badh", "b_age", "b_intx")

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1e3)

mod_sim = coda.samples(model=mod,
                        variable.names=params,
                        n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

## convergence diagnostics
## this is to test if we could accept the posterior distribution
plot(mod_sim)

gelman.diag(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)
effectiveSize(mod_sim)

## compute DIC
dic = dic.samples(mod, n.iter=1e3)
dic


## Residuals
X = as.matrix( badhealth[,-1])
X = cbind( X, with(badhealth, badh*age))
head(X)
tail(X)

pmed_coef = apply(mod_csim, 2, median)
pmed_coef

llam_hat = pmed_coef["int"] + X %*% pmed_coef[c( "b_badh", "b_age", "b_intx" )]
lam_hat = exp(llam_hat)
resid = badhealth$numvisit - lam_hat
plot(resid)

plot(lam_hat, resid, xlim= c(0.8))

plot(lam_hat[which(badhealth$badh == 0)], resid[which(badhealth$badh == 0)], xlim = c(0,8), ylim = range(resid))
points(lam_hat[which(badhealth$badh == 1)], resid[which(badhealth$badh == 1)], col = "red")


var( resid[ which(badhealth$badh == 0)])
var( resid[ which(badhealth$badh == 1)])

summary(mod_sim)

