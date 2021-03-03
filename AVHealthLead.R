# set working directory
setwd('.')

# install packages
install.packages(c("data.table","dplyr","h2o","pROC"))

# files in current directory and sample submission will be saved in same directory

trainfile <- "train_Df64byy.csv"
testfile <- "test_YCcRUnU.csv"
samplesubmission <- "sample_submission_QrCyCoT.csv"

# load libraries

library(data.table)

library(dplyr)

library(h2o)

library(pROC)

# read file and keep columns to their datatypes

h2o.init('localhost')

train_data <- fread(file = trainfile,
                    colClasses=list(character=c(2:5,8:9,10:12),
                                    numeric=c(1,6,7,13,14)),
                    sep=",")

test_data <- fread(file = testfile,
                    colClasses=list(character=c(2:5,8:9,10:12),
                                    numeric=c(1,6,7,13)),
                    sep=",")


names(train_data)[9] <- "Health_Indicator"
names(test_data)[9] <- "Health_Indicator"

train_data <- as.data.frame(train_data)

train_data <- train_data[!(train_data$Holding_Policy_Duration==""),]


train_data_survival <- train_data[!(train_data$Holding_Policy_Duration=="14+"),]

train_data_14 <- train_data[(train_data$Holding_Policy_Duration=="14+"),]

train_data_survival$Holding_Policy_Duration <- as.numeric(train_data_survival$Holding_Policy_Duration)

storelist <- list()
cstorelist <- list()

for (i in 1:14){
  

  wt <- train_data_survival[(train_data_survival$Holding_Policy_Duration==i),]
  
  wt <- dim(wt)[1]
  
  storelist[[i]] <- (dim(train_data_14)[1])/wt
  
  wt.col <- c(rep(storelist[[i]],wt))
  
  cstorelist[[i]] <- wt.col

}

df.list <- c(cstorelist[[1]],cstorelist[[2]],cstorelist[[3]],cstorelist[[4]],cstorelist[[5]],
             cstorelist[[6]],cstorelist[[7]],cstorelist[[8]],cstorelist[[9]],cstorelist[[10]],
             cstorelist[[11]],cstorelist[[12]],cstorelist[[13]],cstorelist[[14]],rep(1,(dim(train_data_14)[1])))




mmtrain <- train_data_survival[order(train_data_survival$Holding_Policy_Duration, decreasing = FALSE),]

mmtrain <- rbind(mmtrain,train_data_14)


final.train <- mmtrain %>% mutate_if(is.character,as.factor)

final.train$wtcol <- df.list

final.train$wtcol <- as.numeric(final.train$wtcol)

final.train$Response <- as.factor(final.train$Response)

final.h2o <- as.h2o(final.train)

final.h2o["Holding_Policy_Duration"] <- h2o.asfactor(final.h2o["Holding_Policy_Duration"])



modelgbm <- h2o.gbm(y = "Response", x = names(final.train[,-c(1)]), training_frame = final.h2o,
                    weights_column = "wtcol",distribution = "bernoulli",
                    ntrees = 5, max_depth = 10)



test_14 <- test_data[(test_data$Holding_Policy_Duration=="14+"),]

test_not14 <- test_data[!(test_data$Holding_Policy_Duration=="14+"),]

test_not14$Holding_Policy_Duration <- as.numeric(test_not14$Holding_Policy_Duration)

test_14$Holding_Policy_Duration <- as.character(test_14$Holding_Policy_Duration)


test <- rbind(test_not14,test_14)

test <- test %>% mutate_if(is.character,as.factor)


test$Response <- "0"

test$Response <- as.factor(test$Response)

test.h2o <- as.h2o(test)


testgbm <- h2o.predict(modelgbm,test.h2o)

testgbm <- as.data.frame(testgbm)

testttt <-
  cbind(test$ID,testgbm$p1)

finalsubmission <- as.data.frame(testttt)

names(finalsubmission) <- c("ID","Response")

fwrite(finalsubmission,samplesubmission)

