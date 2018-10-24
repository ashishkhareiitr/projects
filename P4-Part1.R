################### This P4 - PCA Projections##########################

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
#library(rcorr)

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
stddev <- pca[1:9]$sdev
prvar <- stddev^2
propvarex <- prvar/sum(prvar)

 

## % explained by 9 projection
plot(cumsum(propvarex[1:9]), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")


# lets get D0 using train data ,project whole training data using these 9 PCA and plot.

D0<-predict(pca,traindata[,2:785])
D0<-as.data.frame(D0)
D0<-D0[,1:9]

# add class to the D1
D0$class <- traindata$Site
names(D0) 
unique(D0$class)

ggplot(D0) + geom_point(aes(D0$PC1, D0$PC2, colour = D0$class, shape = D0$class), size = 2.5)

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
names(D1) 
ggplot(D1) + geom_point(aes(D1$PC1, D1$PC2, colour = D1$class, shape = D1$class), size = 2.5)


## model with d0 data
nbPCA<-naive_bayes(class~., data=D0)
print(nbPCA)


## prediction using test data
nbPCAD1 <- predict(nbPCA, D1,type='prob')

## Confustion Matrix - test Data
resultsD1 <- predict(nbPCA, D1)
output <- table(resultsD1, D1$class)
Accuracy <- sum(diag(output))/sum(output)
Accuracy

# using full covariance matrix 

sum(is.na(D0))
nbQDA <- qda(class~., data=D0,na.action=na.omit)
prob.qda <- predict(nbQDA, D1,type='prob')
output.qda <- predict(nbQDA, D1)

tab.output.qda<- table(results_qda_pca$class, D1$class)
Accuracy.qda <- sum(diag(tab.output.qda))/sum(tab.output.qda)
Accuracy.qda

warnings()





