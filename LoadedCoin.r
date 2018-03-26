## You are given a coin with a 60% of probability being loaded
## with 70% Heads and 30% Tails
## Suppose you throw it 5 times and you get 2 heads
## Explore the posterior probabilities
## We write the likelihood function 0 = fair 1 = loaded
## When prompted enter the numbers of given above
p_theta <- readline('Enter the prior probability of the coin being loaded: ')
p_theta <- as.numeric(p_theta)
print('For the next input leave a space between the two numbers written in decimal form.')
probs   <- readline('Enter the probabilities H and T for the loaded case : ')
probs   <- strsplit(probs,' ')
probs   <- as.numeric(c(probs[[1]][1],probs[[1]][2]))
likelihood_coin <- function(x){
        return(choose(5,2)*(0.5)^5*(x==0)+choose(5,2)*probs[1]^2*probs[2]^3*(x==1))
}
posterior_prob  <- function(x){
        nominator   <- likelihood_coin(c(0,1))*c(1-p_theta,p_theta)
        denominator <- sum(nominator)
        nominator   <- nominator[x+1]
        return(nominator/denominator)
}
print('After 5 coin tosses and having observed 2 heads we get: ')
print('Posterior Probability Of The Coin Being Loaded: ')
print(posterior_prob(1))
print('Posterior Probability Of The Coin Being Fair: ')
print(posterior_prob(0))

## We will use the Manhattan-Hastings algorithm to come up with the same result
## Step 1 choose theta_0
theta_0 <- readline('Choose initial theta (enter fair or loaded): ')
if (theta_0 == 'fair'){
        theta_0 <- 0
} else if (theta_0 == 'loaded'){
        theta_0 <- 1
}
iters <- readline('Enter the number of iterations: ')
## Step 2 Choose as theta* the opposite from the previous state
## In our case the candidate distribution is always 1 meaning that 
## q(theta_{i-1}|theta*) = q(theta*|theta_{i-1})
## Step 3 compute the alpha = g(theta*)/g(theta_{i-1})
## g is the posterior_prob we found earlier
iters <- as.numeric(iters)
x     <- numeric(iters)
x[1]  <- theta_0
for (ii in 2:iters){
        theta <- (x[ii-1] + 1) %% 2
        alpha <- posterior_prob((theta + 1) %% 2)/posterior_prob(theta)
        if (alpha >= 1){ ## accept the new one
                x[ii]   <- theta
        }else {
                if (alpha >= runif(1)){ ##accept the new one
                        x[ii] <- theta
                }else { ## reject the new keep the old
                        x[ii] <- x[ii-1]
                }
        }
}
print(sprintf('Acceptance Rate (For Fair After Two Heads): %.2f%%',sum(x)/iters * 100)