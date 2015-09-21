library(caret)
data(faithful)
set.seed(333)
inTrain <- createDataPartition(y=faithful$waiting, p=0.5, list=FALSE)
trainFaith <- faithful[inTrain,]
testFaith <- faithful[-inTrain,]

#fit a linear model
lm1 <- lm(eruptions ~ waiting, data=trainFaith)
summary(lm1)

#calculate RMSE on training
sqrt(sum((lm1$fitted-trainFaith$eruptions)^2))

#calculate RMSE on test
sqrt(sum((predict(lm1, newdata=testFaith)-testFaith$eruptions)^2))

#predict intervals
pred1 <- predict(lm1, newdata=testFaith, interval="prediction")
ord <- order(testFaith$waiting)
plot(testFaith$waiting, testFaith$eruptions, pch=19, col="blue")
matlines(testFaith$waiting[ord], pred1[ord,], type="l", col=c(1,2,2),lty=c(1,1,1),lwd=3)

#caret package regression
modFit <- train(eruptions~waiting,data=trainFaith,method="lm")
summary(modFit)

#multivariate regression
library(ISLR)
library(ggplot2)
library(caret)
data(Wage)
Wage <- subset(Wage,select=-c(logwage))
summary(Wage)
inTrain <- createDataPartition(y = Wage$wage, p = 0.7, list = FALSE)
training <- Wage[inTrain, ]
testing <- Wage[-inTrain, ]

featurePlot(x=training[,c("age","education","jobclass")],y=training$wage,plot="pairs")

#Plot age versus wage colour by jobclass
qplot(age, wage, colour=jobclass, data=training)

modFit<-train(wage~age+jobclass+education,method="lm",data=training)
finMod<-modFit$finalModel
print(finMod)
summary(finMod)
#diagnostics
plot(finMod,1,pch=19,cex=0.5,col="#00000010")
