library(ElemStatLearn)
data(prostate)
str(prostate)

#wagedata as example for emsembling method
training
testing
validation

#build two different models on training set
mod1 <- train(wage ~. method="glm", data=training)
mod2 <- train(wage ~. method="rf", data=training, trControl=trainControl(method="cv"),number=3)

pred1<-predict(mod1, testing)
pred2<-predict(mod2, testing)
qplot(pred1, pred2, colour =wage, data=testing)

predDF <- data.frame(pred1, pred2, wage=testing$wage)
combModFit<- train(wage~., method="gam", data=predDF)
combPred<-predict(combModFit, predDF)
#we can see error based on combModFit is less than mod1 and mod2

#forcasting using quantmod
library(quantmod)
from.dat<-as.Date("01/01/08", format="%m/%d/%y")
to.dat<-as.Date("12/31/13", format="%m/%d/%y")
getSymbols("GOOG",src="google",from=from.dat, to=to.dat)
head(GOOG)
mGoog<-to.monthly(GOOG)
googOpen<-Op(mGOOG)
ts1<-ts(googOpen,frequency=12)
plot(ts1,xlab="Years+1",ylab="GOOG")

plot(decompose(ts1),xlab="Years+1")

#training
ts1Train<-window(ts1, start=1, end=5)
ts1Test<-window(ts1, start=5, end=(7-0.01))

plot(ts1Train)

lines(ma(ts1Train, order=3),col="red")
ets1<-ets(ts1Train,model="MMM")
fcast<-forecast(ets1)
plot(fcast)
lines(ts1Test,col="red")

#get accuracy
accuracy(fcast,ts1Test)

#cluster
modFit<-train(cluster~., data=subset(training,select=-c(Species)),method="rpart")
table(predict(modFit,training),training$Species)