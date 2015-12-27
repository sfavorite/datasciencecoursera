library(caret)


WindowsMultiProcess <- function() {
      print("Setting Up Multiprocess for Windows")
      library(doSNOW) 
      library(foreach) 
      c1 <- makeCluster(8) 
      registerDoSNOW(c1)        
}

UnixMultiProcess <- function() {
      print("Setting up multiprocess for Unix")
      library(doParallel)
      c1 <- makeCluster(8)
      registerDoParallel(c1)
}

switch(Sys.info()[['sysname']], 
      Windows = { WindowsMultiProcess()},
      Linux = { UnixMultiProcess()},
      Darwin = { UnixMultiProcess()})
set.seed(32323)
#download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", destfile="train_data.csv", method = "curl")
training <- read.csv("train_data.csv")

#download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", destfile="test_data.csv", method = "curl")
#testing <- read.csv("test_data.csv")
# Exploratory

test
# Break training into a test and train
inTrain <- createDataPartition(y = training$classe, p=.75, list=FALSE)
train <- training[inTrain,]
test <- training[-inTrain,]
dim(train)
dim(test)

# Cleaning data

# No column has all NA's
#clean.train <- training[,colSums(is.na(training)) != nrow(training)]

# Remove a column if less then 60% of the observations have the data.
clean.train <- train[lapply(train, function(x) sum(is.na(x)) / length(x)) == 0]

clean.train$user_name <- NULL
clean.train$new_window <- NULL
clean.train$num_window <- NULL
clean.train$X <- NULL
remove <- grepl("^X|timestamp|window", names(clean.train))
clean.train <- clean.train[, !remove]
clean.train <- clean.train[, sapply(clean.train, is.numeric)]
clean.train$classe <- train$classe


clean.test <- testing[lapply(testing, function(x) sum(is.na(x)) / length(x)) == 0]

clean.test$user_name <- NULL
clean.test$new_window <- NULL
clean.test$num_window <- NULL
clean.test$X <- NULL
remove <- grepl("^X|timestamp|window", names(clean.test))
clean.test <- clean.test[, !remove]
clean.test <- clean.test[, sapply(clean.test, is.numeric)]
clean.test$classe <- testing$problem_id

clean.test <- testing

#trainRaw <- training[, colSums(is.na(training)) ==0 ]
#temp <- testing[, names(testing) %in% names(clean.train)]
#temp$problem_id <- NULL
#temp$problem_id <- testing$problem_id


# Training
#folds <- createTimeSlices(y = training$classe, initialWindow = 20, horizon = 10)
#fd.train <- folds[[1]]

## K-folds ???

# Repeated cross validaion
ctrlRepeat <- trainControl(method = "repeatedcv", repeats=2)
# Cross validation
ctrl <- trainControl(method="cv", number=10)
modelRf <- train(classe ~ ., data = clean.train, method="lvq", trControl=ctrl)

predictions <- predict(modelRf, test)
confusionMatrix(predictions, test$classe)
# Leave one out Cross Validation
ctrl <- trainControl(method="LOOCV")

#system.time(modelFit <- train(classe ~ ., method = "rpart", data=training, trControl = ctrl, preProc = c("center", "scale")))

modelRf <- train(classe ~ ., data = clean.train, method="rf")
modelRF <- train(classe ~ ., data = clean.train, method = "rf", trControl = ctrl)

pred <- predict(modelRf$finalModel, newdata=clean.test)
table(pred, clean.test)

randomForest(classe ~ ., data=clean.train, importance=T )


ozone <- clean.train[order(clean.train$classe),]

