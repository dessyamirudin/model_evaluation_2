library(tidyverse)
library(caTools)
library(pROC)
library(caret)
library(InformationValue)
# library(ggplot2)


###################### Working with Spam Data ###########################################
# spam data
spam = read.csv("D:/Training Data Science/Session 2/spam/spambase.csv",header=TRUE)
spam$spam = factor(as.character(spam$spam))

# splitting data 70 training and 30 testing
spam.sample = sample.split(spam$spam,SplitRatio = 0.7)
spam.train = spam[spam.sample,]
spam.test = spam[!spam.sample,]

# model logistics regression
spam.logit = glm(spam~.,data=spam.train,family = binomial())

# make prediction
spam.test.predict = predict(spam.logit,newdata = spam.test,type="response")

###################### Working with Bank Data ############################################
# bank marketing data
bank = read.csv("D:/Training Data Science/Session 2/bank/bank-full.csv",header=TRUE, sep=";")
bank$y = factor(bank$y)

# splitting data 70 training and 30 testing
bank.sample = sample.split(bank$y,SplitRatio = 0.7)
bank.train = bank[bank.sample,]
bank.test = bank[!bank.sample,]

# model logistics regression
bank.logit = glm(y~.,data=bank.train,family = binomial())

# make prediction
bank.test.predict = predict(bank.logit,newdata = bank.test,type="response")

###################### Working with Delinquency Data #####################################
# delinquency data
dlqn = read.csv("D:/Training Data Science/Session 2/delinquent/credit_dlqn.csv",header=TRUE)
dlqn$SeriousDlqin2yrs=factor(dlqn$SeriousDlqin2yrs)

# splitting data 70 training and 30 testing
dlqn.sample = sample.split(dlqn$SeriousDlqin2yrs,SplitRatio = 0.7)
dlqn.train = dlqn[dlqn.sample,]
dlqn.test = dlqn[!dlqn.sample,]

# model logistics regression
dlqn.logit = glm(SeriousDlqin2yrs~.,data=select(dlqn.train,-ID),family = binomial())

# make prediction
dlqn.test.predict = predict(dlqn.logit,newdata = dlqn.test,type="response")


###################### ROC Curve #########################################################
# roc curve spam
rocCurve.spam = roc(response = spam.test$spam,
                    predictor = spam.test.predict,
                    levels = rev(levels(spam.test$spam)))
auc(rocCurve.spam)
ci(rocCurve.spam)
plot(rocCurve.spam,legacy.axes=TRUE)
title("ROC Curve Spam",line=+3)

# roc curve bank
rocCurve.bank = roc(response =bank.test$y,
               predictor =bank.test.predict,
               levels = levels(bank.test$y))
auc(rocCurve.bank)
ci(rocCurve.bank)
plot(rocCurve.bank,legacy.axes=TRUE)
title("ROC Curve Marketing Bank",line=+3)

# roc curve dlqn (class assigment)
# What is the ROC value?
# Can you check the proportion of each label from spam, bank and dlqn?

###################### Gini Coefficient ##################################################
Gini.Bank = 2*auc(rocCurve.bank)-1

###################### Gain Lift Chart ###################################################

# spam data manual plot
spam.predict.actual = data.frame(spam.test$spam,spam.test.predict) %>% arrange(-spam.test.predict)

spam.decile = spam.predict.actual %>% mutate(decile.10 = ntile(spam.test.predict,10))

spam.decile.summ = spam.decile %>% group_by(decile.10,spam.test.spam)%>% 
  summarise(Pop=n()) %>% mutate(spam.test.spam=ifelse(spam.test.spam==1,"True","False"))%>% 
  spread(spam.test.spam,Pop)
spam.decile.summ[is.na(spam.decile.summ)]=0
spam.decile.summ = spam.decile.summ %>% mutate(Population=False+True)

# Now go to EXCEL!
write.csv(spam.decile.summ,"spam_decile.csv",row.names = FALSE)

# spam data automatic plot
labs.spam = c(spam.test.predict = "Spam Data Logistic Regression")
liftCurve.spam = lift(factor(spam.test$spam,levels=rev(levels(spam.test$spam)))~spam.test.predict,labels=labs.spam)
# liftCurve.spam = lift(spam.test$spam~spam.test.predict,labels=labs.spam)
liftCurve.spam
xyplot(liftCurve.spam,auto.key = list(columns = 2,lines = TRUE,points = FALSE))

# bank data automatic plot
bank.test.predict.rev = 1-bank.test.predict
labs.bank = c(bank.test.predict.rev = "Bank Data Logistic Regression")
liftCurve.bank = lift(y~bank.test.predict.rev,data=bank.test,labels=labs.bank)
liftCurve.bank
xyplot(liftCurve.bank,auto.key = list(columns = 1,lines = TRUE,points = FALSE))

###################### Kolmogorov Smirnov Chart ##########################################

# KS manual plot will be similar to above

# spam data automatic plot
ks_stat(spam.test$spam,spam.test.predict, returnKSTable = T)
ks_stat(spam.test$spam,spam.test.predict)

source("KS_Plot_Function.R")
ks_plot(spam.test$spam,spam.test.predict)


###################### Concordant Discordant Ratio #######################################

# automatic calculation
Concordance(spam.test$spam,spam.test.predict)


