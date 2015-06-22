train <- read.csv('trainset.csv', header = TRUE, row.names = 1)
train$Made.Donation.in.March.2007 <- factor(train$Made.Donation.in.March.2007)
str(train)
dim(train)

validation <- read.csv('testset.csv', header = TRUE)

summary(train)
pairs(train)
library(ggplot2)
library(reshape2)
longtrain <- melt(train, id=c('Made.Donation.in.March.2007'))
qplot(Made.Donation.in.March.2007, value,
      geom = 'boxplot',
      data = longtrain,
      fill = Made.Donation.in.March.2007) +
    facet_grid(variable ~ ., scales = 'free_y')
      

library(modelUtils)
library(caret)
set.seed(1234)
inTrain <- createDataPartition(y=train$Made.Donation.in.March.2007, p=0.8, list=FALSE)
training <- train[inTrain,]
testing <- train[-inTrain,]
tc <- trainControl(classProbs = TRUE, method = "repeatedcv",
                   number = 3,
                   repeats = 10)
glmModel <- testModel(Made.Donation.in.March.2007 ~ ., as.data.frame(training),
                        as.data.frame(testing),
                        'Made.Donation.in.March.2007',
                        'glm', trControl = tc)

rpartModel <- testModel(Made.Donation.in.March.2007 ~ ., as.data.frame(training),
                        as.data.frame(testing),
                        'Made.Donation.in.March.2007',
                        'rpart', trControl = tc)

rf <- testModel(Made.Donation.in.March.2007 ~ ., training, testing,
                'Made.Donation.in.March.2007',
                'rf',
                trControl = tc)

rpart.test <- rpart(Made.Donation.in.March.2007 ~ ., training)
predictions <- predict(rpart.test, newdata = testing)
table(testing$Made.Donation.in.March.2007, predictions[,1] > 0.5)


resamp <- resamples(
    list(GLM=glmModel$fit,
         RPART=rpartModel$fit,
         RF=rf$fit))
summary(resamp)
bwplot(resamp)

head(rpartModel$predictions)
rpartProbs <- extractProb(list(rpartModel$fit), unkX = validation)
rpartSubmission <- data.frame(X = validation$X, `Made Donation in March 2007` = rpartProbs$X1)
write.table(rpartSubmission, 'rpartSubmission.csv', row.names = FALSE, col.names = c('', 'Made Donation in March 2007'), sep=',', quote = FALSE)