library(readxl)
set.seed(1234)
data = read_excel("C:\\Users\\annap\\Desktop\\???\\3 ????\\?????? ??????\\???????\\??????\\car2.xlsx")
names(data)
dim(data)
head(data)
data1 = head(data, 1600)

#???????????? ???? ???????
number_train<-sample(1:nrow(data),2/3*nrow(data))
train_learn<-data1[number_train,] #????????? ???????
train_test<-data1[-number_train,]#???????? ???????

train=train_learn[complete.cases(train_learn), ]
test=train_test[complete.cases(train_test), ]

#?????????? ??????
library(rpart)
library(rpart.plot)
rtree<-rpart(data$C~.,data[,-13],method="class")
rpart.plot(rtree)
summary(rtree)

printcp(rtree)
plotcp(rtree)

#???????? ?????????????
res_pred<-predict(rtree,train_test[,-13],type="class")
table(res_pred, test$C)

model_tree<-rpart(train_test$C~.,train_test[,-13],method="class",
                  control=rpart.control(minsplit=2,minbucket = 1))
rpart.plot(model_tree)

#LDA
library(MASS)
lda.model<-lda(train$C~.,train[,-13])
lda_res<-predict(lda.model,test[,-13])
table(test$C,lda_res$class)

#????? ?????????? ??????
library(class)

cl=train[complete.cases(train), ][,13]
model_knn <- knn(train[,-13], test[,-13], t(cl), k = 3, prob=TRUE)
table(t(train_test$C), model_knn)

#??????? ??????????? ????? 
library(klaR)
naive_model <- NaiveBayes(as.factor(train$C)~.,data=train[,-13])
naive_res<-predict(naive_model,test[,-13])$class
table(test$C,naive_res)

library(e1071)
#????????????? ??????? svm
svm.model<-svm(train$C~.,train[,-13])
ytest <- subset(test, select = C)
xtest <- subset(test, select = -C)
svm_res<-predict(svm.model,xtest)
svm_res=round(svm_res, digits = 0)
table(svm_res, test$C)

#????? ????????
#????????? ???
#?????? 1
# ????? ???????? ? ????
ntree1 <- 100 
library(randomForest)
forest_model1<- randomForest(train[,-13],t(train[,13]), 
                             ntree=ntree1, mtry=12,
                             replace=FALSE, nodesize = 1,
                             importance=TRUE, localImp=FALSE,
                             proximity=FALSE, norm.votes=TRUE, 
                             do.trace=ntree1/10,
                             keep.forest=TRUE, 
                             corr.bias=FALSE, 
                             keep.inbag=FALSE)  


res_test<-predict(forest_model1,test,type="class")
random_res<-predict(forest_model1,test[,-13])
table(test$C, round(random_res,0))
varImpPlot(forest_model1)

#?????? 2
# ????? ???????? ? ????
ntree1 <- 100 
library(randomForest)
forest_model2<- randomForest(train[,-13],t(train[,13]), 
                             ntree=ntree1, 
                             mtry= floor(sqrt(ncol(train))),
                             replace=FALSE, nodesize = 1,
                             importance=TRUE, localImp=FALSE,
                             proximity=FALSE, norm.votes=TRUE, 
                             do.trace=ntree1/10,
                             keep.forest=TRUE, 
                             corr.bias=FALSE, 
                             keep.inbag=FALSE)  

random_res<-predict(forest_model2,test[,-13])
table(test$C, round(random_res,0)) 
varImpPlot(forest_model2)






