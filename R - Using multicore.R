library(data.table)
library(caret)
library(gbm)
library(randomForest)
library(rpart)
library(doParallel)
library(doMC)

# Specify number of cores

registerDoParallel(4)
registerDoMC(4)

setwd("~/r/titanic")
titanic <- fread("train-kaggle.csv")

row.names(titanic) <- titanic$name
titanic[,name:=NULL]
titanic[,survived:=as.factor(survived)]
titanic[,pclass:=as.factor(pclass)]
titanic[,lastname:=as.factor(lastname)]
titanic[,title:=as.factor(title)]
titanic[,sex:=as.factor(sex)]
titanic[,embarked:=as.factor(embarked)]
titanic[,pid:=NULL]
# Temporary deletion of lastname
lastname <- titanic$lastname
titanic[,lastname:=NULL]

# Step 1: Calculate Age
# First find all rows where
set.seed(1001)
withAge <- titanic[!is.na(age),]
withAgeSurv <- withAge$survived
withAge[,survived:=NULL]

ageInd <- runif(nrow(withAge)) <= 0.75
ageTrain <- withAge[ageInd]
ageTest <- withAge[!ageInd]
lm.age <- lm(age~., ageTrain)


actual <- ageTest$age
ageTest[,age:=NULL]

ageTest$title[35] <- "Mr."
pred.age <- predict(lm.age, ageTest)

withAge[,survived:=withAgeSurv]

withoutAge <- titanic[is.na(age)]
withoutAge[,age:=NULL]
withoutAge$title[113] <- "Mr."

woutAgeSurv <- withoutAge$survived
withoutAge[,survived:=NULL]
withoutAge[,age:=predict(lm.age, withoutAge)]

withoutAge[,survived:=woutAgeSurv]
titanic.fin <- rbind(withAge,withoutAge)


titanicInd <- runif(nrow(titanic.fin)) <= 0.80
titanicTrain <- titanic.fin[titanicInd]
titanicTest <- titanic.fin[!titanicInd]

titanicTest_survived <- titanicTest[,"survived",with=F]

fitControl <- trainControl(
    method = "repeatedcv", 
    number = 10,
    repeats = 3)

rf.train <- train(survived ~., titanicTrain, method="rf", trControl=fitControl)
gbm.train <- train(survived ~., titanicTrain, method="rf", trControl=fitControl)
rpart.train <- train(survived ~., titanicTrain, method="rpart", trControl=fitControl)

predrf.titanic <- predict(rf.train, titanicTest)
predgbm.titanic <- predict(gbm.train, titanicTest)
predrpart.titanic <- predict(rpart.train, titanicTest)


# Begin Testing

fintest <- fread("test-kaggle.csv")
row.names(fintest) <- fintest$pid

fintestPID <- fintest$pid
fintest[,pid:=NULL]
fintest[,pclass:=as.factor(pclass)]
fintest[,title:=as.factor(title)]
fintest[,sex:=as.factor(sex)]
fintest[,embarked:=as.factor(embarked)]
fintest$title[fintest$title=="Dona."] <- "Mrs."

fintestAge <- fintest$age
fintest[,age:=NULL]
fintest[,predAge:=predict(lm.age, fintest)]

fintest[,age:=fintestAge]
fintest[,predAge:=fintemp$predAge]

fintest$age[is.na(fintest$age)] <- fintest$predAge[is.na(fintest$age)]
fintest[,predAge:=NULL]
fintest$fare[153] <- mean(fintest$fare, na.rm=T)

##### ENSEMBLE METHOD ##########################
# Prepare ensemble

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

predgbm.test <- predict(gbm.train, fintest)
predrf.test <- predict(rf.train, fintest)
predrpart.test <- predict(rpart.train, fintest)

combined <- data.table(rf=predrf.test, gbm=predgbm.test, rpart=predrpart.test)
pred.test <- as.numeric(apply(combined, 1, FUN=mode))


fintest[,PassengerId:=fintestPID]
fintest[,Survived:=pred.test]
result <- data.table(PassengerId=fintest$PassengerId, Survived=fintest$Survived)
write.csv(result, "result.csv", row.names=FALSE)

# fintest
# str(fintest)
# str(titanic)
# gbmGrid <- expand.grid(.interaction.depth = (1:5) * 2, .n.trees = (1:5)*50, 
#                        .shrinkage = .1)
# 
# fitControl <- trainControl(
#     method = "repeatedcv", 
#     number = 3,
#     repeats = 3) 
# 
# gbmFit1 <- train(age ~ ., data = ageTrain,  
#     method = "gbm",  
#     trControl = fitControl,
#     # tuneGrid = gbmGrid,
#     verbose = FALSE)
