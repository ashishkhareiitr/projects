library(MASS)
library(ggplot2)
library(scales)
library(gridExtra)

iris <- read.csv(file="C:\\SAProject\\IRIS\\train.csv",header=TRUE,stringsAsFactors = FALSE)

names(iris)
iris
table(iris$Species)

#install.packages("ggplot2")
library(ggplot2)

# it shows versicolor and virginica are most similar so will be creating metaclass for the same.
qplot(Petal.Width, Sepal.Width, data=iris, colour=Species, size=I(4))

#merge to class4
iris$Species[iris$Species == 'virginica'] <- 'Class4'
iris$Species[iris$Species == 'versicolor'] <- 'Class4'


head(iris)
#iris

#first fisher project by discriminate class 3 (sentosa) vs to class 4
lda <- lda(iris$Species ~ ., data = iris)
(lda)

#Coefficients of linear discriminants:
#  LD1
#X             0.001756008
#Sepal.Length  0.575704180
#Sepal.Width   1.989111545
#Petal.Length -1.538761499
#Petal.Width  -0.809278459

# filter out versicolor and virginica

iris2 <- read.csv(file="C:\\SAProject\\IRIS\\train.csv",header=TRUE,stringsAsFactors = FALSE)

#second fisher project by discriminate class 1 to class 2 (versicolor vs virginca)
head(iris2)
iris2<-filter(iris2,iris2$Species=='virginica' | iris2$Species == 'versicolor')
iris2
head(iris2)
lda2 <- lda(iris2$Species ~ ., data = iris2)
lda2

#################################################################

iris3 <- read.csv(file="C:\\SAProject\\IRIS\\test.csv",header=TRUE,stringsAsFactors = FALSE)
plda3 <- predict(object = lda, newdata = iris3)
plda3
plda4 <- predict(object = lda2, newdata = iris3)
plda4

ldadataset = data.frame(Species = iris3[,"Species"],lda = plda3$x, lda2 = plda4$x)
str(ldadataset)


prop.lda = lda$svd^2/sum(lda$svd^2)
prop.lda
prop.lda2 = lda2$svd^2/sum(lda2$svd^2)
prop.lda2


ggplot(ldadataset) + geom_point(aes(LD1, LD1.1, colour = Species, shape = Species), size = 2.5) + 
  labs(x = paste("LD1 (", percent(prop.lda[1]), ")", sep=""),
       y = paste("LD2 (", percent(prop.lda2[1]), ")", sep=""))



