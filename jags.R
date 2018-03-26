# Specify the model
library("rjags")
library("coda")
mod_string = "model{
        for(i in 1:n) {
                y[i] ~ dnorm (mu, 1.0/sig2)
        }
        mu ~ dt(0.0, 1.0/1.0, 1) # location, inverse scale, degrees of freedom t(0,1,1)
        sig2 = 1.0
}" # JAGS believes the model is in the format of strings.

#2. Set up the model
#set.seed(50)
#y = c(1.2, 1.4, -0.5, 0.3, 0.9, 2.3, 1.0, 0.1, 1.3, 1.9)
y = c(-0.2, -1.5, -5.3, 0.3, -0.8, -2.2)
n = length(y)

data_jags = list(y=y, n=n)
params = c("mu")

inits = function(){
        inits = list("mu"= 0.0) 
        # This function create a variable with the name of inits, 
        # and set the variable with list data structure, 
        # with a variable with the name mu and the initial value of 0.0
}

mod = jags.model(textConnection(mod_string), data=data_jags, inits=inits)

#3. Run the MCMC sampler
# function1 update
update(mod, 500)
mod_sim = coda.samples(model = mod, variable.names = params, n.iter=1000)

#4. Post processing
plot(mod_sim)
summary(mod_sim) # posterior distribution of mu