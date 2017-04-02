titanic_train<-read.csv("train.csv")
titanic_test<-read.csv("test.csv")
str(titanic_train)
table(titanic_train$Sex,titanic_train$Survived)
prop.table(table(titanic_train$Sex,titanic_train$Survived),1)
titanic_test$Survived<-0
titanic_test$Survived[titanic_test$Sex=="female"]<-1
submit<-data.frame(PassengerId=titanic_test$PassengerId,Survived=titanic_test$Survived)
write.csv(submit,"titanic_solution.csv",row.names = FALSE)

summary(titanic_train$Age)
titanic_train$child<-0
titanic_train$child[titanic_test$Age<18]<-1
table(titanic_train$Sex,titanic_train$child)
aggregate(Survived~child+Sex,data=titanic_train,FUN=sum)#gives the number of survivors
aggregate(Survived~child+Sex,data=titanic_train,FUN=length)#gives the number of elements in the survivor vector(survivors+non survivors)
aggregate(Survived~child+Sex,data=titanic_train,function(x){sum(x)/length(x)})

titanic_train$fare2<-"30+"
titanic_train$fare2[titanic_train$Fare<30&titanic_train$Fare>=20]<-"20-30"
titanic_train$fare2[titanic_train$Fare<20&titanic_train$Fare>=10]<-"10-20"
titanic_train$fare2[titanic_train$Fare<10]<-"<10"
aggregate(Survived~Sex+fare2+Pclass,data=titanic_train,function(x){sum(x)/length(x)})

titanic_test$Survived<-0
titanic_test$Survived[titanic_test$Sex=="female"]<-1
titanic_test$Survived[titanic_test$Sex=="female"&titanic_test$Pclass==3&titanic_test$Fare>20]<-0
submit<-data.frame(PassengerId=titanic_test$PassengerId,Survived=titanic_test$Survived)
write.csv(submit,"titanic_solution.csv",row.names = FALSE)
str(titanic_train$fare2)


require(rpart)
require(rattle)
require(rpart.plot)
require(RColorBrewer)
fit<-rpart(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data=titanic_train,method = "class",control = rpart.control(minsplit = 2))
rattle()
rpart.plot(fit)
Prediction<-predict(fit,titanic_test,type="class")
submit<-data.frame(PassengerId=titanic_test$PassengerId,Survived=titanic_test$Survived)
write.csv(submit,"titanic_solution.csv",row.names = FALSE)
summary(fit)
