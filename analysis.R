library(caret)
set.seed(12345)

##Download and read files
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",
              destfile = "train.csv")
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",
              destfile = "test.csv")
data <- read.csv("train.csv")
finalTest <- read.csv("test.csv")

##Remove blank columns
data <- data[ , colSums(is.na(data)) == 0]
data <- data[ , -c(1:7)]
fullNSV <- nearZeroVar(data)
data$classe <- as.factor(data$classe)


##Create training, test, and validations sets
inTrain <- createDataPartition(data$classe, p = .6, list = FALSE)
train <- data[inTrain, ]
trainNsv <- nearZeroVar(train)
all(nsv) == all(fullNSV)
train <- train[ , -fullNSV]
inVal <- createDataPartition(testBoth$classe, p = .5, list = FALSE)
testBoth <- data[-inTrain, -fullNSV]
test <- testBoth[-inVal, ]
validation <- testBoth[inVal, ] 


##Classification Tree
modTree <- train(classe ~. , data = train, method = "rpart")
predTree <- predict(modTree, test)
TreeAcc <- confusionMatrix(test$classe, predTree)$overall[1]

##Random Forest
modRF <- train(classe ~. , data = train, method = "rf")
predRF <- predict(modRF, test)
rfAcc <- confusionMatrix(test$classe, predRF)$overall[1]

##Bagging
modBag <- train(classe ~. , data = train, method = "treebag")
predBag <- predict(modBag, test)
bagAcc <- confusionMatrix(test$classe, predBag)$overall[1]


##Linear Discriminant Analysis
modLDA <- train(classe ~. , data = train, method = "lda")
predLDA <- predict(modLDA, test)
ldaAcc <- confusionMatrix(test$classe, predLDA)$overall[1]


accuracies <- data.frame(Tree = TreeAcc, RandForest = rfAcc, 
                         Bagging = bagAcc, LDA = ldaAcc)
round(accuracies, 3)


##Test out of sample accuracy
predVal <- predict(modRF, validation)
confusionMatrix(predVal, validation$classe)

##Final test for the quiz
rbind(1:20, as.character(predict(modRF, finalTest)))
