library("rjags")
data("PlantGrowth")
head(PlantGrowth)
boxplot( weight ~ group, data = PlantGrowth)

# This is a linear variable
lmod1 = lm (weight ~ group, data = PlantGrowth)
summary(lmod1)
anova(lmod1)

mod_string1 = " model {
        for (i in 1:length(y)) {
                y[i] ~ dnorm(mu[grp[i]], prec[grp[i]])
                }
        for (j in 1:3) {
                mu[j] ~ dnorm(0.0, 1.0/1.0e6)
                prec[j] ~ dgamma(5/2.0, 5*1.0/2.0)
                sig[j] = sqrt( 1.0 / prec[j] )
                }
         } "
set.seed(82)
str(PlantGrowth)
data_jags1 = list(y=PlantGrowth$weight, 
                 grp=as.numeric(PlantGrowth$group))

params1 = c("mu", "sig")

inits1 = function() {
        inits = list("mu"=rnorm(3,0.0,100.0), 
                     "prec"=rgamma(3,1.0,1.0))}

mod1 = jags.model(textConnection(mod_string1), 
                 data=data_jags1, 
                 inits=inits1, 
                 n.chains=3)
update(mod1, 1e3)

mod_sim1 = coda.samples(model=mod1,
                       variable.names=params1,
                       n.iter=5e3)
mod_csim1 = as.mcmc(do.call(rbind, mod_sim1)) # combined chains
dic.samples(mod1, n.iter = 1e5)



plot(mod_sim1)
gelman.diag(mod_sim1)
autocorr.diag(mod_sim1)
autocorr.plot(mod_sim1)
effectiveSize(mod_sim1)
(pm_params1 = colMeans(mod_csim1))
coefficients(lmod1)
yhat1 = pm_params1[1:3][data_jags1$grp]
resid1 = data_jags1$y - yhat1
plot(resid1)
plot(yhat1, resid1)
summary(mod_sim1)
HPDinterval( mod_csim1 )
HPDinterval( mod_csim1 , 0.9 )

head(mod_csim1)
mean(mod_csim1[,3] > mod_csim1[,1])
mean(mod_csim1[,3] > 1.1 * mod_csim1[,1])


mod_cm = lm( weight ~ -1 + group, data = PlantGrowth)
summary(mod_cm)
mod_cm0 = lm( weight ~ group, data = PlantGrowth)
summary(mod_cm0)
