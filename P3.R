# set working directory
setwd("C://SAProject")


library(MASS)
library(ggplot2)
library(scales)
library(gridExtra)
library(e1071)
library(caret)
library(modelr)

########################### NAIVE BAYES#########################################################################3
mushroomTrain <- read.csv(file="C:\\SAProject\\Mushroom\\train.csv",header=TRUE,stringsAsFactors = TRUE)
mushroomTest <- read.csv(file="C:\\SAProject\\Mushroom\\test.csv",header=TRUE,stringsAsFactors = TRUE)

lambda = 0
result=matrix(nrow =50 , ncol = 3)

for (lambda in 0:49)
{

    modeltrain<- naiveBayes(mushroomTrain$V1 ~ . , data = (mushroomTrain[,-c(1)]) ,laplace = lambda )
    #modeltest<- naiveBayes(mushroomTest$V1 ~ . , data = (mushroomTest[,-c(1)]) ,laplace = lambda )
    
    head(mushroomTrain)
    trainPred <-predict(modeltrain, newdata = mushroomTrain[,-c(1)],type="class")
    testPred <-predict(modeltrain, newdata = mushroomTest[,-c(1)],type="class")
    
    summary(trainPred)
    table(trainPred)
    
    #Training accuracy
    
    trainTable = table(mushroomTrain$V1, trainPred)
    trainAcc=(trainTable[1,1]+trainTable[2,2])/sum(trainTable)
    trainAcc
    nrow(mushroomTrain)
    #Testing accuracy
    testTable = table(mushroomTest$V1, testPred)
    testAcc=(testTable[1,1]+testTable[2,2])/sum(testTable)
    testAcc
    nrow(mushroomTest)

    
    result[lambda+1,1] = lambda 
    result[lambda+1,2] = trainAcc
    result[lambda+1,3] = testAcc
}

colnames(result)<-c("Lambda","trainAcc","testAcc")
result

write.csv(result, file = "C:\\SAProject\\NaiveBayes.csv")
#####################################################################################################################

#####################################################DECISION TREE####################################################

library(party)
library(tree)

mushroomTrain <- read.csv(file="C:\\SAProject\\Mushroom\\train.csv",header=TRUE,stringsAsFactors = TRUE)
mushroomTest <- read.csv(file="C:\\SAProject\\Mushroom\\test.csv",header=TRUE,stringsAsFactors = TRUE)

sizeThreshold = 4
DT=matrix(nrow =16 , ncol = 3)
counter = 1
for (sizeThreshold in seq(4, 64, by = 4))
{

    train.output.tree <- ctree(mushroomTrain$V1 ~ .,data = mushroomTrain[,-c(1)],controls=ctree_control(maxdepth=sizeThreshold))
    #test.output.tree <- ctree(mushroomTest$V1 ~ .,data = mushroomTest[,-c(1)],controls=ctree_control(maxdepth=sizeThreshold))
    
    plot(train.output.tree)
    train.p <- predict(train.output.tree, mushroomTrain)
    trainTable = table(mushroomTrain$V1,train.p)
    trainAcc=(trainTable[1,1]+trainTable[2,2])/sum(trainTable)
    trainAcc
    
    #plot(test.output.tree)
    test.p <- predict(train.output.tree, mushroomTest)
    testTable = table(mushroomTest$V1,test.p)
    testAcc=(testTable[1,1]+testTable[2,2])/sum(testTable)
    testAcc
    
    
    DT[counter,1] = sizeThreshold 
    DT[counter,2] = trainAcc
    DT[counter,3] = testAcc

    counter = counter + 1
}

colnames(DT)<-c("sizeThreshold","trainAcc","testAcc")
DT
write.csv(DT, file = "C:\\SAProject\\DT.csv")
