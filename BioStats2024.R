##### Intro to Biostats 2024
##### Week 1 #########################################

### Some simulations to look at two linear distributions

##starting with y=mx+b - NO ERROR
m<-1.5
b<-0
x<-rnorm(1000)
y<-m*x+b
y2<-0*x+b
plot.new()
plot(density(y), ylim=c(0, 1.75), xlim=c(-7,7),  col="red")
lines(density(y2))
lm<-lm(y~x)
lm2<-lm(y2~x)
t.test(y, y2)
plot(x, y, col="red")
(lines(x, y2))

#### adding error of various sizes!
y3<-m*x+b+rnorm(1000,0, 0.1) # 0.1 is the error
y4<-m*x+b+rnorm(1000,0, 1)
y5<-m*x+b+rnorm(1000,0, 10)
lm3<-lm(y3~x)
lm4<-lm(y4~x)
lm5<-lm(y5~x)


par(mfrow=c(2,2))
eq = paste0("y = ", round(lm$coefficients[2],1), "*x", "+",round(lm$coefficients[1],1), ", R^2=", round(summary(lm)$adj.r.squared, digits=3))
plot(x, y, col="red", main=eq)
abline(lm, col="black")
eq3 = paste0("y = ", round(lm3$coefficients[2],1), "*x", "+",round(lm3$coefficients[1],1), ", R^2=", round(summary(lm3)$adj.r.squared, digits=3))
plot(x, y3, col="purple", main=eq3)
abline(lm3, col="black")
eq4 = paste0("y = ", round(lm4$coefficients[2],1), "*x", "+",round(lm4$coefficients[1],1), ", R^2=", round(summary(lm4)$adj.r.squared, digits=3))
plot(x, y4, col="blue", main=eq4)
abline(lm4, col="black")
eq5 = paste0("y = ", round(lm5$coefficients[2],1), "*x", "+",round(lm5$coefficients[1],1), ", R^2=", round(summary(lm5)$adj.r.squared, digits=3))
plot(x, y5, col="grey", main=eq5)
abline(lm5, col="black")
dev.off()


plot(density(y), ylim=c(0, 0.5), xlim=c(-4,4),  col="red")
lines(density(y3), col="purple")
lines(density(y4), col="blue")
lines(density(y5), col="black")

##### Week 2 #########################################
title: "Distributions for All"
author: "Eryn McFarlane"
date: '`r Sys.Date()`'
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
##Videos I've used for this:
https://www.youtube.com/watch?v=UrOXRvG9oYE
https://www.youtube.com/watch?v=BPlmjp2ymxw
https://www.youtube.com/watch?v=v1uUgTcInQk
https://www.youtube.com/watch?v=CEVELIz4WXM

##other references
https://www.zoology.ubc.ca/~bio301/Bio301/Lectures/Lecture25/Overheads.html


### Assignment:

# What distribution are your data likely from?
  
# Why do you think this?
  
# Using the distribution that you've chosen, change the different parameters. What happens when you change the parameters for the distribution? Describe how the shape changes.

# Can you simulate data that looks like what you expect your data looks like? What are the parameters for that?

# Get into a group with a peer. Listen to them describe their response variable. What do you think the appropriate distribution would be? Can you simulate data that resembles what your partner has done?

# Distributions!

# I want to talk about a bunch of distributions, and then I want to simulate them as we go so people can see what I'm talking about. 

# I'm going to organize this by continuous vs discrete

## Discrete Distributions

# 1) Bernoulli - the distribution of the number of successes on a single Bernoulli trial.
# For example survival at a single time point. 0 or 1, not a bunch of 0's or 1's (that would be binomial). If we toss a coin once, what's the probability of it being heads? Special case of binomial, n=1

# 2) Binomial distribution is multiple Bernoulli trials - if a coin is tossed 20 times, what is the probability that heads comes up xx/20? How many patients in this ER have covid? What about in this classroom? Discrete space/time. 

#{r Bernoulli}

#### Eryn - note to yourself that the fun thing to do here would be to change the different variables for each simulation and see how they look difference in the plots!

trials<-0:100 ## 1000 Bernoulli trials
hist(rbinom(1000, 1, prob=0.5))
plot(density(rbinom(1000, 1, prob=0.5)))


#```{r Binomial}
 
hist(rbinom(100,6, 0.5))
plot(density(rbinom(100,6, 0.5)))

# 3) geometric distribution is the distribution of the number of trials needed to get the first success. How many times do I have to flip a coin to get a heads? How many patients do I have to test before one has covid? E.g/ if a coin is repeatedly tossed, what is the probability the first time heads occurs is on the 8th toss? Can be thought of as the spacing between neighbours. How many more tests until I get another positive case?

#```{r Geometric}
rgeom(100, 0.05) # this gives back the number of failures before the first successs in each of 10 trials. In our chick example, this could be how many kids each of 10 sets of parents need to have before they successfully recruit one. 

hist(rgeom(100, 0.05))
plot(density(rgeom(100, 0.05)))

# 4) Poisson- The probability of a given number of events occuring in a fixed time interval. Closely approximates the binomial if n is very large and the probablity of success, p, is very small. Counts, like binomial, but not assuming independence, and in continuous space/time. Mean and the variance are the same (which is why when you know you have overdispersed data, you're encouraged to use the negative binomial.) The Poisson only makes sense for count data. 

#```{r Poisson}

hist(rpois(1000, 6.5))
plot(density(rpois(1000, 6.5)))

# 4) Negative binomial - distribution of the number of trials needed to get a certain number of successes. How many random people do we need on a plane to be fairly sure there's some medical doctors (whereas the geometic is how many to be sure there's one)? If a coin is repeatedly tossed, what is the probability the 3rd time a heads appears is on the 9th trial? Rather than the spacing between neighbours, can think of this as the spacing of any number of things in reference category. 
# Generalizes from the geometric. Geometric is to the the 1st success, negative binomial is to get the rth success. Bolker basically says that this is not the way that ecologists think of the negative binomial - rather, we like it because it's independent counts of successes, like poisson, but allows for overdispersion. 

#```{r Negative Binomial}

library(MASS)
MASS::rnegbin(100, mu=5, theta=10) # To carry our flycatcher example forward, negative binomial could be what is the number of singing males we see over a week (as opposed to alarm calling or otherwise not singing). n=# of sites we visit (or site-weeks), mu= mean, theta= measure of over dispersion. If mu = theta, you have poisson distribution (according to here: https://stats.stackexchange.com/questions/10419/what-is-theta-in-a-negative-binomial-regression-fitted-with-r)
hist(MASS::rnegbin(100, mu=5, theta=10))
plot(density(MASS::rnegbin(100, mu=5, theta=10)))

# Binomial vs Negative Binomial
# Binomial - number of trials is fixed (n), number of successes is a random variable.
# Negative Binomial - number of successes is fixed (r), number of trials is the random variable (x)

# 8) Beta distribution - probability of success (in contrast to number of successes, which is what the binomial models). Trivia Answer - what is Alex Buerkle's favourite distribution? Has two  parameters (a and b) to describe the distribution, which can basically be anything between 0 and 1 (but excluding both). For example, allele frequencies are well characterized by the beta distribution. Continuous distribution. This allows you to model anything that looks like a probability. 
# If a=0.5 and b=0.5, you get a u shape. If a and b are not the same, the distribution isn't symetrical. If a and b both =1, you get the uniform probability density. 

# ```{r Beta}
 ## My chick example isn't working for me as well right now. But, something like distribution of admixed hybrids (from two populations) is well approximated by beta. So let's say we have two interbreeding populations, and we want to know what the proportion of species A is in each individual. Beta's great for that. 
hist(rbeta(100, 5, 5))
plot(density(rbeta(100, 5, 5)))



# 9) Dirichlet distribution- generalization of the beta distribution. Give k probabilities (instead of just over two things like in beta) that sum to 1. Parameter of the dirichlet distribution is a vector, of k length. 

#```{r dirichlet}
library(dirmult)
library(ggplot2)
library(Compositional)

bivt.contour(rdirichlet(n=100, alpha=rep(1,3)) )
## example includes metabarcoding, rnaseq. Anything that must sum up to one. 
