trainData <- read.csv("Practice Test Effectiveness - Train.csv",strip.white =TRUE, stringsAsFactors = FALSE, na.strings = c("NA", 0))
testData <- read.csv("Practice Test Effectiveness - Test.csv",strip.white =TRUE, stringsAsFactors = FALSE, na.strings = c("NA", 0)) 

trainData$PracticeExam = as.integer(trainData$PracticeExam)
testData$PracticeExam = as.integer(testData$PracticeExam)

trainData  = na.omit(trainData)
testData  = na.omit(testData)

trainData <- subset(trainData, trainData$PracticeExam >30)
testData <- subset(testData, testData$PracticeExam >30)

trainData <- trainData[complete.cases(trainData), ]
testData <- testData[complete.cases(testData), ]

summary(trainData)
summary(testData)

str(trainData)
str(testData)

model1 = lm(formula = trainData$RealExam ~ trainData$PracticeExam) 
model2 = lm(trainData$RealExam ~ trainData$PracticeExam + (trainData$PracticeExam*trainData$PracticeExam))

summary(model1)
summary(model2)

predictedScores=predict(model2, data=testData)

sm1<-summary(model1)
sm2<-summary(model2)


mserror = (sm2$sigma)^2

actuals_preds <- data.frame(cbind(actuals=trainData$RealExam, predicteds=predictedScores))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)
head(actuals_preds)


plot(model1)


coef(model1)
resid(model1) 
abline(model1)

