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
  
#  ```{r setup, include=FALSE}
#knitr::opts_chunk$set(echo = TRUE)

##Videos I've used for this:
# https://www.youtube.com/watch?v=UrOXRvG9oYE
# https://www.youtube.com/watch?v=BPlmjp2ymxw
# https://www.youtube.com/watch?v=v1uUgTcInQk
# https://www.youtube.com/watch?v=CEVELIz4WXM

##other references
# https://www.zoology.ubc.ca/~bio301/Bio301/Lectures/Lecture25/Overheads.html


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

##### Week 3 #########################################
---
  title: "Lecture 3"
author: "Eryn McFarlane"
date: "`r Sys.Date()`"
output: html_document
---
  
  #{r setup, include=FALSE}
  #knitr::opts_chunk$set(echo = TRUE)
  
  
  
  #We're going to simulate some data that can be used in each of the following tests.
  
  #The response variable is normally distributed, and then we're going to back calculate some predictor variables.
  
  #NOTE: There are 9 questions below to answer. Do your best to answer them in full sentences. This is part of what we're practicing in this class.

#QUESTION: Describe the trait that you're simulating. This can be a real trait that you are working on, or hope to work on, or it can be completely made up. 
#Pay special attention to the N that you're expecting, the mean, and the sd around the mean. 

#{r simulate trait of interest}
rnorm(1000, 0, 10)->trait_of_interest ### change this to be a trait that you're actually interested in, with an appropriate distribution! 

plot(density(trait_of_interest))
min(trait_of_interest)
max(trait_of_interest)


#QUESTION: Describe the predictor variable. What does this mean biologically to your trait of interest. How did you decide on the numbers in yes on line 33?

#{r simulate predictor variable for a t test}

yes<-sample(trait_of_interest, 500, replace=FALSE, prob = ifelse(trait_of_interest>0, 0.95, 0.15)) #### play with this line! Is the test statistically significant. When is it not?
predictor_t.test<-(trait_of_interest %in% yes)

cbind.data.frame(trait_of_interest, predictor_t.test)->data

mean(data[which(data$predictor_t.test==TRUE),1])
mean(data[which(data$predictor_t.test==FALSE),1])
t.test(trait_of_interest~predictor_t.test, data=data) ### this does a two sample t-test. What would a one sample t test be testing? How would you do that?

### plots our two samples for distribution
plot(density(data[which(data$predictor_t.test==FALSE),1]), col="red", main="Two sample t test")
lines(density(data[which(data$predictor_t.test==TRUE),1]), ylim=c(0, 0.1), xlim=c(-20,20), main="Two Sample T test")

###plot one sample distribution
plot(density(data$trait_of_interest), col="red", main="One sample t test")
t.test(data$trait_of_interest) ### what is this test doing?



#QUESTION: Write one sentence where you report your t.text.

#Next we're going to move to Anova. So, the first thing we'll do is break our response variable (same one!) into 5 different categories, just as we did for the t-tests. 

#QUESTION: Describe the predictor variable. What does this mean biologically to your trait of interest. How did you decide on the numbers in lines 60, 61, 62, 63?

#{r Anova}

test1<-sample(trait_of_interest, 200, replace=FALSE, prob = ifelse(trait_of_interest>7, 0.95, 0.15))
test2<-sample(trait_of_interest[which(trait_of_interest %in% test1 == FALSE)], 200, replace=FALSE, prob = ifelse(trait_of_interest[which(trait_of_interest %in% test1 == FALSE)] >4, 0.95, 0.15))
test3<-sample(trait_of_interest[which(trait_of_interest %in% test2 == FALSE | trait_of_interest %in% test1 ==FALSE)], 200, replace=FALSE, prob = ifelse(trait_of_interest[which(trait_of_interest %in% test2 == FALSE | trait_of_interest %in% test1 == FALSE)] >0, 0.95, 0.15))
test4<-sample(trait_of_interest[which(trait_of_interest %in% test2 == FALSE | trait_of_interest %in% test1 ==FALSE | trait_of_interest %in% test3 == FALSE)], 200, replace=FALSE, prob = ifelse(trait_of_interest[which(trait_of_interest %in% test2 == FALSE | trait_of_interest %in% test1 ==FALSE | trait_of_interest %in% test3 == FALSE)] >-4, 0.95, 0.15))
test5<-trait_of_interest[which(trait_of_interest %in% test1 == FALSE| trait_of_interest %in% test2 == FALSE | trait_of_interest %in% test3 == FALSE | trait_of_interest %in% test4 == FALSE)]


plot(density(test1), ylim=c(0, 0.1), main="Anovas")
lines(density(test2), col="red")
lines(density(test3), col="blue")
lines(density(test4), col="purple")
lines(density(test5), col="yellow")


anova_predictor<-data.frame(ifelse(trait_of_interest %in% test1 == TRUE, "group1", ifelse(trait_of_interest %in% test2 == TRUE, "group2", ifelse(trait_of_interest %in% test3 == TRUE, "group3", ifelse(trait_of_interest %in% test4 == TRUE, "group4", "group5")))))

data2<-cbind.data.frame(data, anova_predictor)
names(data2)<-c("trait_of_interest", "predictor_t.test", "anova_predictor")


anova(aov(trait_of_interest~anova_predictor, data=data2)) ### what does this do?

anova(lm(trait_of_interest~anova_predictor, data=data2)) ### what does this do? 
summary(aov(trait_of_interest~anova_predictor, data=data2)) ### what does this do? What do you notice about the last three tests?

### what information is missing here that you wished you had to understand your study better?
analysis_of_variance<-aov(trait_of_interest~anova_predictor, data=data2) ##name the model to keep it for downstream
TukeyHSD(analysis_of_variance, conf.level = 0.95) ### what does this do, and where are the differences?


#QUESTION: Write one sentence where you report your ANOVA and Tukey tests. What did you find, and how do you report this?


#Again, our simulations aren't to be actually causal, I'm simulating predictor variables to fit a response I've already made. Normally, we would simulate the response variables from more thoughtful predictor variables.

#QUESTION:what is the difference between the assumed distributions for the prior predictor variables, and this one?

#QUESTION: Describe the predictor variable. What does this mean biologically to your trait of interest. How did you decide on the numbers in line 104?

#QUESTION: What is the difference between a regression and a correlation? When would you use each? How does the test stat from the correlation compare to the effect size from the regression?

#{r Linear Regression and correlation}

linear_regression_predictor<-0.5*trait_of_interest+rnorm(1000, 0, 4) ### change these numbers!! Remember that this is backwards from how we did this on day 1, so the slope should go the other way!
data3<-cbind(data2, linear_regression_predictor)

lm<-lm(trait_of_interest~linear_regression_predictor, data=data3)
summary(lm)### what is the output here? What are we interested in understanding from this model? How do we get m? How do we get the intercept?

eq = paste0("y = ", round(lm$coefficients[2],1), "*x", "+",round(lm$coefficients[1],1), ", R^2=", round(summary(lm)$adj.r.squared, digits=3))
plot(data3$linear_regression_predictor, data3$trait_of_interest, col="red", main=eq)
abline(lm, col="black")

### with the same data:
cor.test(data3$trait_of_interest, data3$linear_regression_predictor) ### compare the sample estimate to the 1) simulated effect sizes and 2) to the estimated effect size 
### how does the correlation estimate change when you change line 104?

#QUESTION: Report your regression and correlation in a sentence. Differentiate between them and what you report for each. 