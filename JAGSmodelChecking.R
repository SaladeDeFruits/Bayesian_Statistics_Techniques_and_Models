dat = read.table(file = "cookies.dat", header = TRUE)
head(dat)
library("rjags")
mod_string =  "model {
        for ( i in 1:length(chips)){
                chips[i] ~ dpois( lam[ location[i]] )
        }
        for( j in 1:max(location)){
                lam[j] ~ dgamma(alpha, beta)
        }
        
        mu ~ dgamma(2.0, 1.0/5.0)  
        sig ~ dexp(1.0)
        
        alpha = mu^2 /sig^2
        beta = mu/ sig^2
        }"

set.seed (113)

data_jags = as.list(dat)
params = c("lam", "mu", "sig")
mod = jags.model( textConnection(mod_string), 
                  data = data_jags,
                  n.chains = 3)
update(mod, 1e3)
mod_sim = coda.samples(model = mod, 
                       variable.names = params, 
                       n.iter = 5e3)
mod_csim = as.mcmc( do.call(rbind, mod_sim))
plot(mod_sim, ask = TRUE)

dic = dic.samples(mod, n.iter = 1e3)

## Model Checking
## 1. Check of the Residuals, the observation levels and the option means level- lambda
## 

pm_params = colMeans( mod_csim )

yhat = rep(pm_params[1:5], each = 30)
resid = dat$chips - yhat

# Plot the residuals to check if there exist some patterns
plot(resid)
plot( jitter(yhat), resid)
#Poission likelihood, the variance increases while the mean increases

# Variance check on observation levels
var(resid[yhat < 7])
var(resid[yhat > 11])


lam_resid = pm_params[1:5] - pm_params["mu"]
plot(lam_resid)
abline(h=0, lty=2)

summary(mod_sim)
