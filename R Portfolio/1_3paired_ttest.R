#Paired t-test prototype
#Justin Mann
#2/16/2016

#A paired t-test is a parametric test which compares the means of two matched samples.

#Read in data: I'm using "iris," a data set built into r.
iris

#The following test will compare Sepal.Length between "versicolor" and "virginica"
#Assign subsets of two equal-length vectors

versicolor <- iris$Sepal.Length[iris$Species == "versicolor"]
virginica <- iris$Sepal.Length[iris$Species == "virginica"]

#Assumptions:
#1. Observed values X1,1, X1,2, X1,3, ... X1,n are a random sample exactly matched with
#Observed values X2,1, X2,2, X2,3, ... X2,n across individuals 1,2,3, ... ,n.
#2. Let di= X2,i -X1,i for each individual i are a random sample from ~N(mu,Sigma).
#3. Variance of the population is unknown.
##Note: paired t-test is reasonably robust for deviations from normal distribution.

#Verify that data vectors are the same length
length(versicolor)
#[1] 50
length(virginica)
#[1] 50

#Differences between pairs are normally distributed
hist(versicolor - virginica, col="grey")

#Below are the manual calculations for paired t-test

#Assign vectors versicolor and virginica to variables "x1" and "x2" respectively. 
x1 <- versicolor
x2 <- virginica

#Assign "n" to sample size
n <- length(versicolor)

#Assign means and standard deviations
x1bar <- mean(versicolor)
x2bar <- mean(virginica)
s1 <- sd(versicolor)
s2 <- sd(virginica)

#Assign "dbar" to the mean difference between paired data
d <- x1-x2
dbar <- mean(d)

#Assign standard deviation of "d" 
s_d <- sqrt(var(d))

#Test statistic calculation for "t"
t <- dbar/(s_d/sqrt(n))
t

#Probability (P) value (two sided case)
degf <- n-1
P <- 2 * pt(t, degf)
P

#Confidence intervals
alpha <- 0.05

#T distribution
c1 <- qt(alpha/2,degf)
c1 <- -c1

#Confidence Intervals
ci_l <- dbar-c1*(s_d/sqrt(n))
ci_u <- dbar+c1*(s_d/sqrt(n))

CI <- c(ci_l, ci_u)
CI

#Run the test using the built in function
t.test(versicolor,virginica,paired=TRUE, conf.level=0.95)
