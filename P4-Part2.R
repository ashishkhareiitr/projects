######################################################################################################
#################### this is LDA projections##########################################################
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

################data prep###############
tempData = traindata[,2:785]
df1 <- impute.NN_HD(DATA=tempData,distance="eukl",comp="rseq")
df1$Site<- traindata$Site
df2<-df1[,-c(1,2,3,4,5,6,7,8,9,10,11,12,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,53,54,55,
             56,57,58,60,83,84,85,86,112,113,140,141,142,169,477,561,589,616,617,644,645,646,672,673,674,699,700,701,702,727,728,
             729,730,731,753,755,756,757,758,759,760,781,782,783,784)]

nblda = lda( Site ~ ., data = df2)

### Get 9 dimensional  train data
train.D2 = as.matrix(df2[,1:ncol(df2)-1]) %*% nblda$scaling
train.D2 = as.data.frame(train.D2)
train.D2$digit = traindata$Site

unique(train.D2$digit)
summary(nblda)
nblda
## get 9 dimensional test data for lda
test.filepath <-"C:\\SAProject\\MNIST\\test"
setwd(test.filepath)
getwd()

csv.read.test <- function(x) {
  out <- read.csv(x)[ ,2:785]
  label <- substr(x,5,5)
  cbind(Site=label, out)
}

test.filenames <- list.files(test.filepath, full.names = TRUE, pattern = "csv", recursive = TRUE)
digits<-substr(test.filenames, nchar(test.filenames)-9+1, nchar(test.filenames))
testdata <- lapply(digits, csv.read.test) %>% bind_rows()

tempData = testdata[,2:785]
df1 <- impute.NN_HD(DATA=tempData,distance="eukl",comp="rseq")
df1$Site<- testdata$Site
df3<-df1[,-c(1,2,3,4,5,6,7,8,9,10,11,12,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,53,54,55,
             56,57,58,60,83,84,85,86,112,113,140,141,142,169,477,561,589,616,617,644,645,646,672,673,674,699,700,701,702,727,728,
             729,730,731,753,755,756,757,758,759,760,781,782,783,784)]

test.D2 = as.matrix(df3[,1:ncol(df3)-1]) %*% nblda$scaling
test.D2 = as.data.frame(df3)
test.D2$digit <- testdata$Site

# lets now run the naive bayes which has diagonal nonzero.
# first we will run naive bayes on train.D2

nbLDA.train<-naive_bayes(digit~., data=train.D2)
nrow(train.D2)
nrow(test.D2)
#pred.test <- predict(nbLDA.train, df3,type='prob')
#pred.test

# confustion Matrix - test Data
unique(test.D2[,c(24189,24190)])
colnames(test.D2)
nrow(test.D2)

output.ldax <- predict(nbLDA.train, test.D2[,-c(24189,24190)])
length(output.ldax)


tab2 <- table(output.ldax, test.D2$digit)
lda.Accuracy <- sum(diag(tab2))/sum(tab2)
lda.Accuracy


# full covariance matrix bayes


xqda <- qda(digit ~ ., data = train.D2)
output.qdax <- predict(xqda, test.D2)

(tab1 <- table(output.qdax$class, test.D2$Site))

qda.Accuracy <- sum(diag(tab1))/sum(tab1)
qda.Accuracy