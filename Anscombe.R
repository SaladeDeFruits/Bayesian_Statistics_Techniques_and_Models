library("rjags")

mod_string = " model {
for (i in 1:length(education)) {
education[i] ~ dnorm(mu[i], prec)
mu[i] = b0 + b[1]*income[i] + b[2]*young[i] + b[3]*urban[i]
}

b0 ~ dnorm(0.0, 1.0/1.0e6)
for (i in 1:3) {
b[i] ~ dnorm(0.0, 1.0/1.0e6)
}

prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
## Initial guess of variance based on overall
## variance of education variable. Uses low prior
## effective sample size. Technically, this is not
## a true 'prior', but it is not very informative.
sig2 = 1.0 / prec
sig = sqrt(sig2)
} "

data_jags = as.list(Anscombe)
mod_lm = lm(formula = Anscombe)
plot(mod_lm)
dic.samples(mod_lm, n.iter = 1e5)
# question3
#lmod1 = lm(education ~ income, data = Anscombe)
#summary(lmod1)
#lmod2 = lm(education ~ young, data = Anscombe)
#summary(lmod2)
#lmod3 = lm(education ~ urban, data = Anscombe)
#summary(lmod3)

### Convergence
plot(mod_lm)

gelman.diag(mod_lm)
#autocorrelation
autocorr.diag(mod1_sim)

# we test 15,000 data, and the effect data is only around 300
effectiveSize(mod1_sim)

lmod = lm(loginfant ~ logincome, data = Leinhardt)
summary(lmod)
dic.samples(lmod)
