data(iris)
library(ggplot2)
library(caret)
set.seed(333)
names(iris)
table(iris$Species)
inTrain <- createDataPartition(y = iris$Species, p = 0.7, list=F)
training<-iris[inTrain,]
testing<-iris[-inTrain,]
dim(training)
dim(testing)

#compare lda and naive bayes model
modlda<-train(Species~.,data=training,method="lda")
modnb<-train(Species~.,data=training,method="nb")
plda<-predict(modlda,testing)
pnb<-predict(modnb,testing)
table(plda,pnb)

equalPredictions=(plda==pnb)
qplot(Petal.Width,Sepal.Width,colour=equalPredictions,data=testing)
