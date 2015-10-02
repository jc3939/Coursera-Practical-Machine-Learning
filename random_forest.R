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

library(caret)
modFit<-train(Species~.,data=training, method="rf",prox=TRUE)
modFit

getTree(modFit$finalModel, k=2)

#class centers
irisP<-classCenter(training[,c(3,4)],training$Species,modFit$finalModel$prox)
irisP<-as.data.frame(irisP)
irisP$Species<-rownames(irisP)
p<-qplot(Petal.Width, Petal.Length, col=Species,data=training)
p+geom_point(aes(x=Petal.Width,y=Petal.Length,col=Species),size=5,shape=4,data=irisP)

#predict new values
pred<-predict(modFit,testing)
testing$predRight<-pred==testing$Species
table(pred,testing$Species)

#plot to see which I missed
qplot(Petal.Width,Petal.Length,colour=predRight,data=testing,main="newdata Predictions")
