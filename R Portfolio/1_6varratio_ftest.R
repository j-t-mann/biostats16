#Variance Ratio F-test prototype 
#Justin Mann
#2/24/2016

#for testing equal variances in two samples

#Assumptions:
# Observed values x1.1...x1.n are a random sample from a normal distribution.
# Observed values x2.1...x2.n are a random sample from a normal distribution.
# Both sample are independent.

## Note this test is seriously comprimised by deviation from normal distribution.
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

#assign variances 
s1_sq <- var(x1)
s1_sq
s2_sq <- var(x2)
s2_sq

######Test Statistic######

#two sided case:
f <- s2_sq/s1_sq #note: put larger variance in the numerator
f

#one sided case lower tail:
f_a <- s1_sq/s2_sq
f_a

#one sided case upper tail:
f_b <- s2_sq/s1_sq
f_b

#Sampling Distribution: if assumptions hold and Null Hypothesis is true, the F~F(n1-1)/(n2-1)

#Critical Values of the Test:
alpha <- 0.05 #probablility of type 1 error

cv <- qf(1-alpha/2, n1-1, n2-1) #two sided cv
cv

cv_a <- qf(alpha, n1-1, n2-1) #one sided lower cv
cv_a

cv_b <- qf(1-alpha, n1-1, n2-1) #one sided upper cv
cv_b

#Decision Rules:
#If f > cv, then reject Null, otherwise accept Null (two sided case)
#If f_a < cv_a, then reject Null, " (one sided lower tail)
#If f_b > cv_b, then reject Null, " (one sided upper tail)

#Probability Values:
#two sided case
p1 <- 2*pf(f, n2-1, n1-1) #if f < or equal to 1
p1

p2 <- 2*(1-pf(f, n1-1, n2-1)) #if f > 1
p2

#one sided case

p3 <- pf(f, n1-1, n2-1) #lower tail
p3

p4 <- 1-pf(f, n1-1, n2-1) #upper tail
p4

#Confidence Intervals for Variance Ratio:
#two sided case: (ci_a ci_b)
ci_a <- (s2_sq/s1_sq)*(1/qf(1-alpha/2, n1-1, n2-1))
ci_a

ci_b <- (s2_sq/s1_sq)*(qf(1-alpha/2, n2-1, n1-1))
ci_b

#one sided case
ci_l <- (s2_sq/s1_sq)*(1/qf(1-alpha, n1-1, n2-1)) #lower tail (0-ci_l)
ci_l

ci_u <- (s2_sq/s1_sq)*(qf(1-alpha, n2-1, n1-1)) #upper tail (ci_u-infinity)
ci_u

#Now test the built in R function:
var.test(x2,x1,alternative = "two.sided",conf.level = 0.95)

#one sided case lower tail 
var.test(x2,x1,alternative = "less",conf.level = 0.95)

#one sided case upper tail
var.test(x2,x1,alternative = "greater",conf.level = 0.95)
