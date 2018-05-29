library(caTools)
library(caret)
library(rpart)
library(rpart.plot)
library(tidyverse)

# search()
# unloadNamespace("InformationValue")

########################### Working with Spam Data ###########################################

# spam data
spam.data = read.csv("D:/Training Data Science/Session 2/spam/spambase.csv",header=TRUE)
spam.data = spam.data %>% mutate(spam=ifelse(spam==1,"Spam","notSpam"))

# splitting data 80 training and 20 testing
spam.sample = sample.split(spam.data$spam,SplitRatio = 0.8)
spam.train = spam.data[spam.sample,]
spam.test = spam.data[!spam.sample,]

########################### Cross Validation #################################################

# model using decision tree
spam.dtree = rpart(spam~.,data=spam.train,cp=0.02)
spam.dtree
summary(spam.dtree)
# predict
spam.test.predict = predict(spam.dtree,newdata=spam.test,type="class")

confusionMatrix(data=spam.test.predict,reference=spam.test$spam)
prp(spam.dtree)
printcp(spam.dtree)
########################## KFold Cross Validation ############################################

# buku Max Kuhn halaman 389
# ctrl = trainControl(method = "repeatedcv", number =5,repeats = 5) 
ctrl = trainControl(method = "cv", number =5) 
spam.tree.kcv = train(spam~.,
                      data = spam.data,
                      method = "rpart",
                      trControl = ctrl)

spam.tree.kcv
summary(spam.tree.kcv)
# the result actually already using grid search with default value of 3 per parameter.
# if it has 3 paramepeter, it will have 3x3 = 9 experiment
########################## Grid Search #######################################################

ctrl = trainControl(method = "cv", number = 5)
grid = expand.grid(cp=seq(0,0.5,0.01))
spam.tree.kcv.grid = train(spam~.,
                      data = spam.data,
                      method = "rpart",
                      trControl = ctrl,
                      tuneGrid = grid)
spam.tree.kcv.grid

# try different grid cp = seq(0,0.5,0.01)

# Note
# - method: intelligent guess, if factor then classification
# - minsplit default is 20 - number of sample in node before split
# - minbucket is the value of number of sample in one onde = minsplit/3
# - maxdepth is the maximum depth default 30 in 32bit
# - rpart only capable to control cp
# - default grid is 3
