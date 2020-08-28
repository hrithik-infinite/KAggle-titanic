library(caret)
library(doSNOW)
library(caTools)
library(randomForest)
train <- read.csv('train.csv', stringsAsFactors = F , header = T)
test <- read.csv('test.csv', stringsAsFactors = F , header = T)

train$Embarked[train$Embarked == ''] <- "S"
train$trainset <- T
test$trainset <-F
test$Survived <- NA
full <- rbind(train ,test)
full$missingage <- ifelse(is.na(full$Age),"Y" , "N")
full$familysize <- 1+full$SibSp +full$Parch
full$Survived <- as.factor(full$Survived)
full$Pclass <- as.factor(full$Pclass)
full$Sex<- as.factor(full$Sex)
full$Embarked <- as.factor(full$Embarked)
full$missingage <- as.factor(full$missingage)
full[is.na(full$Fare),"Fare"] <- median(full$Fare ,na.rm=T)

features <- c("Survived","Pclass","Sex","Age" ,"SibSp","Parch","Fare","Embarked","missingage","familysize")
featuresfull <- c("Survived","Pclass","Sex","Age" ,"SibSp","Parch","Fare","Embarked"
                  ,"missingage","familysize","trainset")


full1 <- full[,features]
full_set <- full[,featuresfull]

dumm1 <- dummyVars(~. , data = full1[,-1])
full.dummy <- predict(dumm1 ,full1[,-1])

pre.process <- preProcess(full.dummy , method ="bagImpute")
imput <- predict(pre.process,full.dummy)
full_set$Age <- imput[,6]

tit.train <- full_set[full_set$trainset == T,]
tit.test <- full_set[full_set$trainset == F,]
tit.train <- subset(tit.train , select = -trainset)
tit.test <- subset(tit.test , select = -trainset)

model <- randomForest(Survived~.,data=tit.train , ntree =2000 ,mtry =3 , nodesize =0.1*nrow(tit.test))
Survived <- predict(model , newdata = tit.test)

PassengerID <- test$PassengerId
opdf <- as.data.frame(PassengerID)
opdf$Survived <- Survived
write.csv(opdf , file = "Kaggletitanic1.csv" , row.names = F)
