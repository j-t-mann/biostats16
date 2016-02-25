#Levene test prototype 
#Justin Mann
#2/24/2016

#for testing equal variances in two samples
#this test employs transformation of data values to difference values around each mean

#Assumptions:
# Observed values x1.1...x1.n are a random sample from a normal distribution.
# Observed values x2.1...x2.n are a random sample from a normal distribution.
# Both sample are independent.

## Note this test is less comprimised by deviation from normal distribution.
## Be sure to test for normal distribution before using this test!

# Hypotheses:
#1) Null: Sigma1^2 is equal to Sigma2^2
#2) Alternative: Sigma1^2 is NOT equal to Sigma2^2 (two sided case)
#                Sigma1^2 < Sigma2^2 (one sided case lower tail)   
#                Sigma1^2 > Sigma2^2 (one sided case upper tail)

#Paperwork

#read in data
iris

#assign variables
x1 <- iris$Sepal.Length[iris$Species=="setosa"]
x1

x2 <- iris$Sepal.Length[iris$Species=="versicolor"]
x2

#assign number of observations
n1 <- length(x1)
n1

n2 <- length(x2)
n2

#assign means
x1bar <- mean(x1)
x1bar
x2bar <- mean(x2)
x2bar

#Transformation to Absolute Difference from Mean: absolute value of each observation subtracted from the respective mean
x1t <- abs(x1-x1bar)
x1t

x2t <- abs(x2-x2bar)
x2t

#means of transformed differences from mean
x1tbar <- mean(x1t)
x1tbar

x2tbar <- mean(x2t)
x2tbar

#variances of transformed differences from mean
st1 <- var(x1t)
st1

st2 <- var(x2t)
st2

#pooled variance of transformed observations
stp <- sqrt(((n1-1)*st1+(n2-1)*st2)/n1+n2-2)
stp

#####Test Statistic#####
t <- (x1tbar-x2tbar)/sqrt(stp/n1+stp/n2)
t

#Sampling Distribution: if assumptions hold and Null Hypothesis is true, then t~t(n1+n2-2)

#Critical Values of the Test:
alpha <- 0.05 #probability of type 1 error

#two sided case
c1 <- qt(alpha/2, n1+n2-2) #lower cv  
c1

c2 <- qt(1-alpha/2, n1+n2-2) #upper cv
c2

abs_c <- abs(c1) #cv used for two sided test
abs_c

#one sided case
c3 <- qt(alpha, n1+n2-2) #lower cv
c3

c4 <- qt(1-alpha, n1+n2-2) #upper cv
c4

#Decision Rules:
#if abs(t) > abs_c, then reject Null, otherwise accept (two sided case)
#if t < c3, then reject Null, " (one sided lower tail)
#if t > c4, then reject Null, " (one sided upper tail)

#Probability Values:
#two sided case
p1 <- 2*pt(t, n1+n2-2) #if t is < or equal to 0
p1

p2 <- 2*(1-pt(t, n1+n2-2)) #if t is > 0
p2

#one sided case
p3 <- pt(t, n1+n2-2) #lower tail 
p3

p4 <- 1-pt(t, n1+n2-2) #upper tail
p4

#Test the built in R function for Levene test
library(car)
leveneTest(x1,x2,center = mean)
