trainData <- read.csv("Car Worth - Train.csv",strip.white =TRUE, stringsAsFactors = FALSE, na.strings = c("NA", 0))
testData <- read.csv("Car Worth - Test.csv",strip.white =TRUE, stringsAsFactors = FALSE, na.strings = c("NA", 0)) 


trainData  = na.omit(trainData)
testData  = na.omit(testData)

trainData <- trainData[complete.cases(trainData), ]
testData <- testData[complete.cases(testData), ]

str(testData)
str(trainData)


model1 = lm(formula = trainData$Price ~ ., data = trainData) 


summary(model1)


predictedPrice=predict(model1, data=testData)
sm1<-summary(model1)
mserror = (sm1$sigma)^2

actuals_preds <- data.frame(cbind(actuals=testData$Price, predicteds=predictedPrice))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)
head(actuals_preds)

plot(model1)
coef(model1)
resid(model1) 
abline(model1)


