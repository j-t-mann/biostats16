#Started by Matthew Lundquist
#Completed by Justin Mann
#1/24/2016
#Based on Biostatistics 020 and 030 by William Stein

#This is the first homework assignment for Biol 483N/597

#Due by 11:59 PM 2/1/2016 (Tuesday section) or 2/3/2016 (Thursday section)

#**Note: This is a skeleton script, much more detail is needed to get full 
#credit for this assignment

#**Note: Dr. Stein prefers to use "=" to assign variables, I prefer "<-". 
#While these are both correct, it is easy to confuse "=" with "==" which denotes
#equality. It is up to you to decide which you want to use.
#Be consistent!

#Part one: Inputting data

#set working directory to where iris.txt file is located
setwd("C:/Users/Karin/Desktop/BU/BIOSTATS")

# this line designates "iris.txt" to the variable "iris"
iris  <- read.table("iris.txt")

# calls for the variable
iris

#resulting output from calling for variable "iris"
#Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
#1            5.1         3.5          1.4         0.2     setosa
#2            4.9         3.0          1.4         0.2     setosa
#3            4.7         3.2          1.3         0.2     setosa
#4            4.6         3.1          1.5         0.2     setosa
#5            5.0         3.6          1.4         0.2     setosa
#6            5.4         3.9          1.7         0.4     setosa


#Part two: Descriptive statistics

#mathmatic functions [r works as a basic calculator]
2 + 2
#[1] 4
35/5
#[1] 7
6*5
#[1] 30

#r contains built in functions **avoid renaming built in functions to variables

pi
# [1] 3.141593

#assignment (setting variables) with "<-" as in x <- x
var <-  78

#evaluation: typing the variable "var" outputs "78"
var
#[1] 78

#assignement of var2
var2 <- 6*(7/9)

#evaluation of var2
var2
#[1] 4.666667

#assignment 
var3 <- (5+3)/(sqrt(pi))

#evaluation 
var3
#[1] 4.513517

#assignment of variable "SL" to column 1 of table "iris"
#[] denotes a subset: [row,column]
SL <- iris[,1]
SL
#[1] 5.1 4.9 4.7 4.6 5.0 5.4 4.6 5.0 4.4 4.9 5.4

#the following lines are a different method of accomplishing the same thing
#assigning "SL,SW,PL,PW, and species"to their respective columns in the data table
#the symbol "$" also denotes subset by heading
SL <- iris$Sepal.Length
SL
#[1] 5.1 4.9 4.7 4.6 5.0 5.4 4.6 5.0 4.4 4.9 5.4 4.8 4.8 4.3 5.8 5.7 5.4 5.1 5.7 5.1 5.4 5.1 4.6 5.1 4.8 5.0 5.0
SW <- iris$Sepal.Width
SW
#[1] 3.5 3.0 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 3.7 3.4 3.0 3.0 4.0 4.4 3.9 3.5 3.8 3.8 3.4 3.7 3.6 3.3 3.4 3.0 3.4
PL <- iris$Petal.Length
PL
#[1] 1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 1.5 1.6 1.4 1.1 1.2 1.5 1.3 1.4 1.7 1.5 1.7 1.5 1.0 1.7 1.9 1.6 1.6
PW <- iris$Petal.Width
PW
#[1] 0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 0.2 0.2 0.1 0.1 0.2 0.4 0.4 0.3 0.3 0.3 0.2 0.4 0.2 0.5 0.2 0.2 0.4

#therefore, the following two lines accomplish the same task also
species <- iris[,5]
species <- iris$Species
species
#[1] setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa    
#[10] setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa 

#cbind assigns the variables in parentheses into ordered vectors
out <- cbind(SL,SW,PL,PW,species)
out
#        SL  SW  PL  PW species
#[1,] 5.1 3.5 1.4 0.2       1
#[2,] 4.9 3.0 1.4 0.2       1
#[3,] 4.7 3.2 1.3 0.2       1
#[4,] 4.6 3.1 1.5 0.2       1
#[5,] 5.0 3.6 1.4 0.2       1
#[6,] 5.4 3.9 1.7 0.4       1

#length is a function that evaluates the number of rows in column()
length(SL)
#[1] 150

#this line accomplishes the same length function with the nominal heading of the column
length(iris$Sepal.Width)
#[1] 150

#the following two lines are functions evaluating the length of PW
length(iris[,3])
length(PW)
#[1] 150

#assignment of variable n to the length of vector SL
n <- length(SL)
#evaluation
n
#[1] 150

#evaluates value of column (SL), row 2
SL[2]
#[1] 4.9

#evaluates value of column (SL), row 3
SL[3]
#[1] 4.7

#evaluates value of table (iris) [row 3, column 1]
iris[3,1]
#[1] 4.7

#assignment of variable "xbar" to the formula for the mean of column "SL"
xbar <- (1/n)*sum(SL)
#evaluation of the mean of column "SL"
xbar
#[1] 5.843333

#this line uses the an r function to calculate the mean of column "SL"
mean(SL)
#[1] 5.843333

#r function to calculate the median of "SL"
median(SL)
#[1] 5.8

#assignment of variable "geom_meanSL" to the formula for the geometric mean formula
geom_meanSL <- prod(SL)^(1/length(SL))
#evaluation
geom_meanSL
#[1] 5.78572

#assignment of variable "harm_meanSL" to the formula for the harmonic mean formula
harm_meanSL <- n/sum(1/SL)
#evaluation
harm_meanSL
#[1] 5.728905

#evaluation of sample variance of "SL" using r function var()
var(SL)
#[1] 0.6856935

#assignment of "sample_varSL to formula for population variance
sample_varSL <- ((n-1)/n)/var(SL)
#evaluation
sample_varSL
#[1] 1.448655

#linear transformation
#the following values convert degrees Celsius to Fahrenheit
#assignment
b1 <- 32
#assignment
c1 <- 1.8

#assignment of "SLt" coded by linear tranformation y = 1.8x + 32 
SLt  <- c1*SL+b1

#linear transformation of mean 
mean(SLt)
#[1] 42.518

#linear transformation of variance 
var(SLt)
#[1] 2.221647

#Part three: Summary statistics and graphics

#the following line produces a ready-made output of summary statistics for "iris"
summary(iris)
#Sepal.Length    Sepal.Width     Petal.Length    Petal.Width          Species  
#Min.   :4.300   Min.   :2.000   Min.   :1.000   Min.   :0.100   setosa    :50  
#1st Qu.:5.100   1st Qu.:2.800   1st Qu.:1.600   1st Qu.:0.300   versicolor:50  
#Median :5.800   Median :3.000   Median :4.350   Median :1.300   virginica :50  
#Mean   :5.843   Mean   :3.057   Mean   :3.758   Mean   :1.199                  
#3rd Qu.:6.400   3rd Qu.:3.300   3rd Qu.:5.100   3rd Qu.:1.800                  
#Max.   :7.900   Max.   :4.400   Max.   :6.900   Max.   :2.500

#assignment of "i" to an output of iris table columns 1-4, excluding species
i <- iris[,1:4]
#evaluation
i
#Sepal.Length Sepal.Width Petal.Length Petal.Width
#1            5.1         3.5          1.4         0.2
#2            4.9         3.0          1.4         0.2
#3            4.7         3.2          1.3         0.2
#4            4.6         3.1          1.5         0.2
#5            5.0         3.6          1.4         0.2

#assignment, "diag()" extracts the diagonal of a matrix. 
#"var" calculates the variances in matrix "i"
variances <- diag(var(i))
#evaluation
variances
#Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
#0.6856935    0.1899794    3.1162779    0.5810063

#assignment of "sd_i" to the standard deviation formula as the square route of variance
sd_i <- sqrt(variances)
#evaluation
sd_i
#Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
#0.8280661    0.4358663    1.7652982    0.7622377

#assignment of "coefvar" to the formula for coefficient of variation, i.e. Sample Standard Deviation divided by mean for each variable
coefvar  <- sd_i/colMeans(i)
#evaluation
coefvar
#Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
#0.1417113    0.1425642    0.4697441    0.6355511

#Scatter plots

#in RStudio, this function brings up a menu of scatter plots representing various comparisons of the iris data
plot(iris)

#plots sepal length (x) against sepal width (y)
plot(iris$Sepal.Length, iris$Sepal.Width, col="red", pch=19)
#output saved as SL_SWplot.pdf

#same plot with default black since "col" is not defined
plot(iris$Sepal.Length, iris$Sepal.Width, pch=19)
#output saved as SL_SWplot2.pdf

#with point function, you can change the colors of specific data points 
#the following two lines output "setosa" species points in red and "veriscolor" species points in blue
#remaining species remain black without a defined color
points(iris$Sepal.Length[iris$Species=="setosa"], iris$Sepal.Width[iris$Species=="setosa"], col="red", pch=19) 
points(iris$Sepal.Length[iris$Species=="versicolor"], iris$Sepal.Width[iris$Species=="versicolor"], col="blue", pch=19)
#output saved as SL_SWplot3.pdf

#Box plots
#here, the plot() function plots species (x) against SL (y) in box plot format
plot(iris$Species,iris$Sepal.Length)
#output saved as "Species_SLbox.pdf

#library entitled "Trellis Graphics for R"
library(lattice) 
#graphs all data from iris table in a boxplot 
boxplot(iris)
#output saved as irisbox.pdf

#Histograms
op <- par(mfrow=c(2, 2)) 

#the following lines produce for the data SL, SW, PL, and PW, respectively
hist(iris$Sepal.Length,col='red') 
#output saved as hist_SL.pdf
hist(iris$Sepal.Width,col='blue') 
#output saved as hist_SW.pdf
hist(iris$Petal.Length,col='green')
#output saved as hist_PL.pdf
hist(iris$Petal.Width,col='purple') 
#output saved as hist_PW.pdf
par(op)

#the following lines produce histograms comparing the differences between each species SL, SW, PL, and PW, respectively
histogram(~ Sepal.Length | Species,data=iris)
# output saved as hist_SLspecies
histogram(~ Sepal.Width | Species,data=iris)
# output saved as hist_SWspecies
histogram(~ Petal.Length | Species,data=iris)
# output saved as hist_PLspecies
histogram(~ Petal.Width | Species,data=iris)
# output saved as hist_PWspecies
