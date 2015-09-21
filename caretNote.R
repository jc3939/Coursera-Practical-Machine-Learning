library(caret)
library(kernlab)
data(spam)
#data slicing
inTrain <- createDataPartition(y = spam$type, p = 0.75, list = FALSE)
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]
dim(training)#3451, 58

#cross validation
set.seed(32323)
folds <- createFolds(y = spam$type, k = 10, list = TRUE, 
                     returnTrain = FALSE)
sapply(folds, length)
#Fold01 Fold02 Fold03 Fold04 Fold05 Fold06 Fold07 Fold08 Fold09 Fold10 
#4141   4140   4141   4142   4140   4142   4141   4141   4140   4141 

#Sample with replacement(boostrap)
set.seed(32323)
folds <- createResample(y = spam$type, times = 10, list=TRUE)
sapply(folds, length)
#Resample01 Resample02 Resample03 Resample04 Resample05 Resample06 Resample07 
#4601       4601       4601       4601       4601       4601       4601 
#Resample08 Resample09 Resample10 
#4601       4601       4601 

folds[[1]][1:10]
#[1]  1  2  3  3  3  5  5  7  8 12

#Time slices
set.seed(32323)
tme <- 1:1000
folds <- createTimeSlices(y=tme, initialWindow=20, horizon=10)
names(folds)
#[1] "train" "test"

folds$train[[1]]
#[1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20

folds$test[[1]]
#[1] 21 22 23 24 25 26 27 28 29 30

inTrain <- createDataPartition(y = spam$type, p = 0.75, list = FALSE)
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]

modelFit <- train(type ~., data = training, method = "glm")

args(train.default)
#function (x, y, method = "rf", preProcess = NULL, ..., weights = NULL, 
#          metric = ifelse(is.factor(y), "Accuracy", "RMSE"), 
#          maximize = ifelse(metric == "RMSE", FALSE, TRUE), trControl = trainControl(), 
#          tuneGrid = NULL, 
#          tuneLength = 3) 
#NULL


# args(trainControl)
# function (method = "boot", number = ifelse(grepl("cv", method), 
#                                            10, 25), repeats = ifelse(grepl("cv", method), 1, number), 
#           p = 0.75, initialWindow = NULL, horizon = 1, fixedWindow = TRUE, 
#           verboseIter = FALSE, returnData = TRUE, returnResamp = "final", 
#           savePredictions = FALSE, classProbs = FALSE, summaryFunction = defaultSummary, 
#           selectionFunction = "best", preProcOptions = list(thresh = 0.95, 
#                                                             ICAcomp = 3, k = 5), index = NULL, indexOut = NULL, timingSamps = 0, 
#           predictionBounds = rep(FALSE, 2), seeds = NA, adaptive = list(min = 5, 
#                                                                         alpha = 0.05, method = "gls", complete = TRUE), allowParallel = TRUE) 
# NULL

library(ISLR)
library(ggplot2)
library(caret)
data(Wage)
summary(Wage)
inTrain <- createDataPartition(y = Wage$wage, p = 0.75, list = FALSE)
training <- Wage[inTrain, ]
testing <- Wage[-inTrain, ]
dim(training)#2251, 12
dim(testing)#749, 12

#caret
featurePlot(x = training[, c("age", "education", "jobclass")], 
            y = training$wage, plot="pairs")
#ggplot2
qplot(age, wage, data=training)

qplot(age, wage, colour = jobclass, data=training)

#Add regression smoothers
qq <- qplot(age, wage, colour = education, data=training)
qq + geom_smooth(method = "lm", formula=y~x)

#cut2, making factors
library(Hmisc)
cutWage <- cut2(training$wage, g = 3)
table(cutWage)

#Boxplot with cut2
p1 <- qplot(cutWage, age, data = training, fill = cutWage, geom = c("boxplot"))
p1

library(gridExtra)
p2 <- qplot(cutWage, age, data = training, fill = cutWage, geom = c("boxplot", "jitter"))
p2
grid.arrange(p1, p2, ncol=2)

#Tables
t1 <- table(cutWage, training$jobclass)
t1
# cutWage         1. Industrial 2. Information
# [ 20.1, 91.7)           473            281
# [ 91.7,118.9)           398            382
# [118.9,318.3]           280            437

prop.table(t1, 1)
# cutWage         1. Industrial 2. Information
# [ 20.1, 91.7)     0.6273210      0.3726790
# [ 91.7,118.9)     0.5102564      0.4897436
# [118.9,318.3]     0.3905160      0.6094840

#Density plots
qplot(wage, colour=education, data=training, geom = "density")




