library(caret)
library(doMC)
registerDoMC(cores=4)
library(kernlab)
data(spam)
# Split the data into train and test
inTrain <- createDataPartition(y = spam$type, p = 0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)
# Fit a model
set.seed(32343)
system.time(modelFit <- train(type ~ ., data=training, method="glm"))
modelFit
# Final model
system.time(modelFit <- train(type ~ ., data=training, method="glm"))
modelFit$finalModel
# Prediction
predictions <- predict(modelFit, newdata = testing)
predictions
# Confusion Matrix
confusionMatrix(predictions, testing$type)
