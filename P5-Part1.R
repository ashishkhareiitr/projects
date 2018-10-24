library(MASS)
library(stringi)
library(stringr)
library(Matrix)
library(dplyr)
library(naivebayes)
library(data.table)
library(ggplot2)
library(ggfortify)
library(corrplot)
library(HotDeckImputation)
library(class)
#install.packages("gmodels")
library(gmodels)

filepath <-"C:\\SAProject\\MNIST\\train"
setwd(filepath)

csv.read <- function(x) {
  out <- read.csv(x)[ ,2:785]
  label <- substr(x,6,6)
  cbind(Site=label, out)
}


filenames <- list.files(filepath, full.names = TRUE, pattern = "csv", recursive = TRUE)
digits<-substr(filenames, nchar(filenames)-10+1, nchar(filenames))
traindata <- lapply(digits, csv.read) %>% bind_rows()

head(traindata)
summary(traindata)
nrow(traindata)
ncol(traindata)
ncol(features)
## get the features
features<-traindata[,-1]

pca<-prcomp(na.omit(features))
D0<-predict(pca,traindata[,2:785])
D0<-as.data.frame(D0)
D0<-D0[,1:9]

# add class to the D1
D0$class <- traindata$Site

head(D0)
unique(D0$class)

### test data

########### D1 data sets############

filepath <-"C:\\SAProject\\MNIST\\test"
setwd(filepath)
csv.read <- function(x) {
  out <- read.csv(x)[ ,2:785]
  label <- substr(x,5,5)
  cbind(Site=label, out)
}


filenames <- list.files(filepath, full.names = TRUE, pattern = "csv", recursive = TRUE)
digits<-substr(filenames, nchar(filenames)-9+1, nchar(filenames))
testdata <- lapply(digits, csv.read) %>%bind_rows()


D1<-predict(pca,testdata[,2:785])
D1<-as.data.frame(D1)
D1<-D1[,1:9]

# add class to the D1
D1$class <- testdata$Site

D0 <- na.omit(D0)
D1<-na.omit(D1)

for (i in seq(1, 17, by = 2))
{
      
      test.pred <- knn(train = D0, test = D1,cl = D0$class, k=i)
      
      CCT <- table(D1$class,test.pred)
      #,prop.chisq = FALSE)
      
      CCT.ACCURACY <- sum(diag(CCT)) / sum(CCT)
      print(i)
      print(CCT.ACCURACY)
}



############# parzen window


library(kdensity)
library(MASS)
plot(kdensity(mtcars$mpg, start = "normal"))

library("PerformanceAnalytics")

my_data <- mtcars[, c(1,3,4,5,6,7)]
chart.Correlation(testdata[,-c(1)], histogram = TRUE, pch = 19)

##############################################################################
#The kernel estimator ^f is a sum of 'bumps' placed at the observations.
#The kernel function determines the shape of the bumps while the window
#width h determines their width. 

x <- c(0, 1, 1.1, 1.5, 1.9, 2.8, 2.9, 3.5)
n <- length(x)
#For a grid
xgrid <- seq(from = min(x) - 1, to = max(x) + 1, by = 0.01)

#on the real line, we can compute the contribution of each measurement in x,
#with h = 0.4, by the Gaussian kernel (defined in Figure 7.1, line 3) as follows;
h <- 0.4
gauss <- function(x) 1/sqrt(2*pi) * exp(-(x^2)/2)

bumps <- sapply(x, function(a) gauss((xgrid - a)/h)/(n * h))
#A plot of the individual bumps and their sum, the kernel density estimate ^f,
#is shown in Figure 7.2.






