library(caret)
library(kernlab)
data(spam)
#data slicing
inTrain <- createDataPartition(y = spam$type, p = 0.75, list = FALSE)
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]
hist(training$capitalAve, main = "", xlab = "ave. capital run length")
#from the graph, we can tell data is skewed.

mean(training$capitalAve)#5.204
sd(training$capitalAve)#33.955

preObj <- preProcess(training[, -58], method = c("center", "scale"))

trainCapAves <- predict(preObj, training[,-58])$capitalAve

testCapAveS <- predict(preObj, testing[,-58])$capitalAve
mean(testCapAveS)

set.seed(32343)
modelFit <- train(type ~., data = training, preProcess=c("center", "scale"), method="glm")
modelFit

#use boxcox
preObj <- preProcess(training[, -58], method = c("BoxCox"))
trainCapAveS <- predict(preObj, training[,-58])$capitalAve

par(mfrow=c(1,2));
hist(trainCapAveS)
qqnorm(trainCapAveS)#From the q-q plot we can see there are lots of 0 values, and BoxCox cannot handle it.

#Impute and standardization
set.seed(13343)
    #making NA
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1], size = 1, prob=0.05)==1
training$capAve[selectNA] <- NA
    #Impute and standardization
preObj <- preProcess(training[,-58], method="knnImpute")
capAve <- predict(preObj, training[,-58])$capAve

#dummy variables
library(ISLR)
library(ggplot2)
library(caret)
data(Wage)
summary(Wage)
inTrain <- createDataPartition(y = Wage$wage, p = 0.75, list = FALSE)
training <- Wage[inTrain, ]
testing <- Wage[-inTrain, ]

dummies <- dummyVars(wage~jobclass, data=training)

head(predict(dummies, newdata=training))
#jobclass.1. Industrial jobclass.2. Information
# 86582                       0                       1
# 11443                       0                       1
# 160191                      1                       0
# 11141                       0                       1
# 229379                      1                       0
# 86064                       1                       0

#remove zero covariates

nsv <- nearZeroVar(training, saveMetrics=TRUE)
nsv

# freqRatio percentUnique zeroVar   nzv
# year        1.018970     0.3109729   FALSE FALSE
# age         1.128205     2.6654820   FALSE FALSE
# sex         0.000000     0.0444247    TRUE  TRUE
# maritl      3.344681     0.2221235   FALSE FALSE
# race        8.571429     0.1776988   FALSE FALSE
# education   1.520408     0.2221235   FALSE FALSE
# region      0.000000     0.0444247    TRUE  TRUE
# jobclass    1.024281     0.0888494   FALSE FALSE
# health      2.595847     0.0888494   FALSE FALSE
# health_ins  2.334815     0.0888494   FALSE FALSE
# logwage     1.061728    18.9693470   FALSE FALSE
# wage        1.061728    18.9693470   FALSE FALSE


#splines packages
library(splines)
bsBasis <- bs(training$age, df = 3)
head(bsBasis)
# 1          2           3
# [1,] 0.2368501 0.02537679 0.000906314
# [2,] 0.3625256 0.38669397 0.137491189
# [3,] 0.4422183 0.19539878 0.028779665
# [4,] 0.4430868 0.24369776 0.044677923
# [5,] 0.4308138 0.29109043 0.065560908
# [6,] 0.4261690 0.14823269 0.017186399


#Correlated Vars
inTrain <- createDataPartition(y = spam$type, p = 0.75, list = FALSE)
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]

M <- abs(cor(training[,-58]))
diag(M)<-0
which(M>0.8, arr.ind=T)
# row col
# num415  34  32
# direct  40  32
# num857  32  34
# direct  40  34
# num857  32  40
# num415  34  40

typeColor<-((spam$type=="spam")*1+1)
prComp<-prcomp(log10(spam[,-58]+1))
plot(prComp$x[,1], prComp$x[,2], col=typeColor, xlab="PC1",ylab="PC2")

#using preProcess
preProc <- preProcess(log10(spam[,-58]+1), method="pca", pcaComp=2)
spamPC<-predict(preProc, log10(spam[,-58]+1))
plot(spamPC[,1], spamPC[,2], col=typeColor)

preProc <- preProcess(log10(training[,-58]+1), method="pca", pcaComp=2)
trainPC <- predict(preProc, log10(training[,-58]+1))
modelFit<-train(training$type~., method="glm", data=trainPC)

#alternative method
modelFit<-train(training$type~., method="glm", data=training, preProcess="pca")
confusionMatrix(testing$type, predict(modelFit, testing))



















