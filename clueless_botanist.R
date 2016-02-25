#Clueless Botanist
#subsets of species data 

setosa_sl <- iris$Sepal.Length[iris$Species=="setosa"]
setosa_sl

setosa_sw <- iris$Sepal.Width[iris$Species=="setosa"]
setosa_sw

setosa_pl <- iris$Petal.Length[iris$Species=="setosa"]
setosa_pl

setosa_pw <- iris$Petal.Width[iris$Species=="setosa"]
setosa_pw

versicolor_sl <- iris$Sepal.Length[iris$Species=="versicolor"]
versicolor_sl

versicolor_sw <- iris$Sepal.Width[iris$Species=="versicolor"]
versicolor_sw

versicolor_pl <- iris$Petal.Length[iris$Species=="versicolor"]
versicolor_pl

versicolor_pw <- iris$Petal.Width[iris$Species=="versicolor"]
versicolor_pw

virginica_sl <- iris$Sepal.Length[iris$Species=="virginica"]
virginica_sl

virginica_sw <- iris$Sepal.Width[iris$Species=="virginica"]
virginica_sw

virginica_pl <- iris$Petal.Length[iris$Species=="virginica"]
virginica_pl

virginica_pw <- iris$Petal.Width[iris$Species=="virginica"]
virginica_pw


?leveneTest()

su <- subset(iris, iris$Species != "virginica")
leveneTest(su$Sepal.Length, sv$Species, center=mean)

sv <- subset(iris, iris$Species != "setosa")
leveneTest(sv$Sepal.Length, sv$Species, center=mean)

sw <- subset(iris, iris$Species != "versicolor")
leveneTest(sw$Sepal.Length, sv$Species, center=mean)


su <- subset(iris, iris$Species != "virginica")
leveneTest(su$Sepal.Width, sv$Species, center=mean)

su <- subset(iris, iris$Species != "virginica")
leveneTest(su$Petal.Length, sv$Species, center=mean)

su <- subset(iris, iris$Species != "virginica")
leveneTest(su$Petal.Width, sv$Species, center=mean)



