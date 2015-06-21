train <- read.csv('trainset.csv', header = TRUE)
train$Made.Donation.in.March.2007 <- factor(train$Made.Donation.in.March.2007)
str(train)
dim(train)

validation <- read.csv('testset.csv', header = TRUE)

summary(train)
pairs(train)
library(ggplot2)
library(reshape)
longtrain <- melt(train, id=c('X', 'Made.Donation.in.March.2007'))
qplot(Made.Donation.in.March.2007, value,
      geom = 'boxplot',
      data = longtrain,
      fill = Made.Donation.in.March.2007) +
    facet_grid(variable ~ ., scales = 'free_y')
      

library(modelUtils)
library(caret)
inTrain <- createDataPartition(y=train$Made.Donation.in.March.2007, p=0.8, list=FALSE)
training <- train[inTrain,]
testing <- train[-inTrain,]
tc <- trainControl(classProbs = TRUE, method = "repeatedcv",
                   number = 10,
                   repeats = 10)
glmModel <- testModel(Made.Donation.in.March.2007 ~ ., as.data.frame(training),
                        as.data.frame(testing),
                        'Made.Donation.in.March.2007',
                        'glm',
                        trainControl = tc)

rpartModel <- testModel(Made.Donation.in.March.2007 ~ ., as.data.frame(training),
                        as.data.frame(testing),
                        'Made.Donation.in.March.2007',
                        'rpart')

rf <- testModel(Made.Donation.in.March.2007 ~ ., training, testing,
                'Made.Donation.in.March.2007',
                'rf',
                trainControl = tc)

rpart.test <- rpart(Made.Donation.in.March.2007 ~ ., training)
predictions <- predict(rpart.test, newdata = testing)
table(testing$Made.Donation.in.March.2007, predictions[,1] > 0.5)

