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

qplot(Petal.Width, Sepal.Width,colour=Species,data=training)
modFit<-train(Species ~., method="rpart",data=training)
print(modFit$finalModel)

plot(modFit$finalModel, uniform=TRUE, main="Classification Tree")
text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=.8)

#to make the plot looks fancier
library(rattle)
fancyRpartPlot(modFit$finalModel)

#test
predict(modFit, newdata=testing)
