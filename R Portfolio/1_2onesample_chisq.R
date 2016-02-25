#One Sample Chi-square Test of Variance for a Normal Distribution
#Prototyped by Justin Mann
#2/17/2016

#This test tests hypotheses about whether the variance of a population is statistically equivalent to a specified SigmaSquared_naught.

#Assumptions:
#1. Observed values, X1...Xn, are a random sample from a normal distribution.
#2. Variance of the population is unknown.

#Hypotheses:
#Null: SigmaSquared is equal to SigmaSquared-naught
#Alternative: SigmaSquared is not equal to SigmaSquared_naught

#assign "x" to data subset 
x <- iris$Sepal.Length[iris$Species=="setosa"]
x

#assign population variance to "sigmsq_naught" 
sigma_naught <- sqrt(0.1)
sigma_naught

#verify length and assign to "n"
n <- length(x)
n

#assign "xbar" to the mean of "x"
xbar <- mean(x)
xbar

#assign "s" to standard deviation 
s <- sqrt(var(x))
s

#assign "s_sq" to variance
s_sq <-var(x)
s_sq

#assign "degf" to n-1
degf <- n-1
degf

#*****Test Statistic*****
Xsq <- (degf*s_sq)/sigma_naught^2
Xsq

#Critical Value of the Test:
alpha <- 0.05

C1 <- qchisq(alpha/2,degf) #Two sided Lower
C1
C2 <- qchisq(1-alpha/2,degf) #Two sided Upper
C2
C3 <- qchisq(alpha,degf) #One sided lower
C3
C4 <- qchisq(1-alpha,degf)
C4

#Decision Rules:
#1. If chisq<C1 or chisq>C2, then reject the Null (two sided case)
#2. If chisq<C3, then reject Null (one sided lower tail)
#3. If chisq>C4, then reject Null (one sided upper lower)

#Probablility Values:
P <- 2*(1-pchisq(Xsq,degf))
P
PL <- pchisq(Xsq,degf)
PL
PU <- 1-(pchisq(Xsq,degf))
PU

#Confidence Intervals for Population Variance:
CI1 <- (degf*s_sq/C1) #two sided
CI1
CI2 <- (degf*s_sq/C2) #two sided
CI2
CI3 <- (degf*s_sq/C3) #one sided
CI3
CI4 <- (degf*s_sq/C4) #one sided
CI4

#Now text the R function
library(EnvStats)
varTest(x,sigma.squared = 0.1, alternative = "two.sided", conf.level = 0.95)
