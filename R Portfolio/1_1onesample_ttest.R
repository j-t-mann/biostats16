#One sample t-test prototype
#Justin Mann
#2/17/2016

#A one sample t-test tests hypotheses about the mean of a population with uknown variance.

#Assumptions:
#1. Observed values, x1-xn, are a random sample from a normally distributed population.
#2. Variance of the population is unknown
##Note: this test is robust for deviations from a normal distribution

#Hypotheses:
#Null: Mu equals mu_naught
#Alternative: Mu does not equal mu_naught

#I will run a t-test on a subset of the iris data (built into R), setosa sepal length.
#Read table
iris

#Assign data subset
x <- iris$Sepal.Length[iris$Species=="setosa"]
x

#Visually verify normal distribution of "x"
hist(x)

#Verify length and assign 
n <- length(x)
n

#Assign population mean 
Mu <- 5
Mu

#Assign sample mean 
xbar <- mean(x)
xbar

#Assign stardard deviation of x 
s <- sqrt(var(x))
s

#*****Test Statistic*****
t <- (xbar-Mu)/(s/sqrt(n))
t

#Critical Value of the Test:
alpha <- 0.05

degf <- n-1
degf

C1 <- qt(alpha/2,degf)
C1
C2 <- qt(1-alpha/2,degf)
C2

#Decision Rule:
#if t<C1 or if t>C2, then reject Null
#if abs(t)>abs(C), then reject Null

#Probability (P) Value (two sided case)

Pa <- 2 * pt(t,degf)
Pa
Pb <- 2*(1-pt(t,degf))
Pb

#Confidence Interval for the Mean

CI1 <- xbar+abs(C1)*(s/sqrt(n))
CI1
CI2 <- xbar-abs(C1)*(s/sqrt(n))
CI2

#Run test with R function "t.test"
t.test(x, alternative="two.sided", mu=0, conf.level=0.95)



#####One Tail Case(Lower Tail)######

#Assumptions: same as two-tailed

#Hypotheses:
#Null: Mu is greater than or equal to Mu_naught
#Alternative: Mu is less than Mu_naught

#*****Test Statistic*****
t <- (xbar-Mu)/(s/sqrt(n))
t

#Critical Value of the test:
alpha <- 0.05

degf <- n-1
degf

C <- qt(alpha, degf)
C

#Decision Rule: if t<C, then reject the Null.

#Probability Value:
P <- pt(t,degf)
P

#Confidence Interval for the Mean:
CI1 <- xbar+abs(C1)*(s/sqrt(n))
CI1

#Lower Tail Case built-in R function
t.test(x,alternative="less", mu=0,conf.level=0.95)



#####One Tail Case (Upper Tail)#####

#Assumptions: same as two-tailed

#Hypotheses:
#Null: Mu is less than or equal to Mu_naught
#Alternative: Mu is greater than Mu_naught

#*****Test Statistic*****
t <- (xbar-Mu)/(s/sqrt(n))
t

#Critical Value of the test:
alpha <- 0.05

degf <- n-1
degf

C <- qt(alpha, degf)
C

#Decision Rule: if t<C, then reject the Null.

#Probability Value:
P <- pt(t,degf)
P

#Confidence Interval for the Mean:
CI1 <- xbar-abs(C1)*(s/sqrt(n))
CI1

#Upper Tail Case built-in R function
t.test(x,alternaOve="greater", mu=5,conf.level=0.95)
