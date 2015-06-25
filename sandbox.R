train <- read.csv('trainset.csv', header = TRUE, row.names = 1)
train$Made.Donation.in.March.2007 <- factor(train$Made.Donation.in.March.2007, levels = c(0,1), labels = c('No', 'Yes'))
str(train)
dim(train)

validation <- read.csv('testset.csv', header = TRUE)

isRVD <- function(recency, frequency, monetary, time) {
    recency <= 6 && frequency >= 4 && monetary >= 200 && time > 24
}

donationFrequency <- function(frequency, time) {
    frequency / time
}

train$RVD <- mapply(isRVD, train$Months.since.Last.Donation, train$Number.of.Donations, train$Total.Volume.Donated..c.c.., train$Months.since.First.Donation)
train$Donation.Frequency <- mapply(donationFrequency, train$Number.of.Donations, train$Months.since.First.Donation)
validation$RVD <- mapply(isRVD, validation$Months.since.Last.Donation, validation$Number.of.Donations, validation$Total.Volume.Donated..c.c.., validation$Months.since.First.Donation)
validation$Donation.Frequency <- mapply(donationFrequency, validation$Number.of.Donations, validation$Months.since.First.Donation)

dummies <- dummyVars( ~ . - Made.Donation.in.March.2007, data = train, fullRank = TRUE)
train <- data.frame(predict(dummies, newdata = train), Made.Donation.in.March.2007 = train$Made.Donation.in.March.2007)
dummies <- dummyVars(X ~ ., data = validation, fullRank = TRUE)
validation <- data.frame(X=validation$X, predict(dummies, newdata = validation))

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
                    number = 5,
                    repeats = 10)
tc2 <- trainControl(classProbs = TRUE, method = "repeatedcv",
                   number = 2,
                   repeats = 10, summaryFunction = twoClassSummary)
set.seed(1234)
glmModel <- testModel(Made.Donation.in.March.2007 ~ ., as.data.frame(training),
                        as.data.frame(testing),
                        'Made.Donation.in.March.2007',
                        'glm', trControl = tc, metric = "Kappa")

rpartModel <- testModel(Made.Donation.in.March.2007 ~ ., training,
                        testing,
                        'Made.Donation.in.March.2007',
                        'rpart', trControl = tc, metric = "Kappa")

rf <- testModel(Made.Donation.in.March.2007 ~ ., training, testing,
                'Made.Donation.in.March.2007',
                'rf',
                trControl = tc, metric = "Kappa")

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
rpartSubmission <- data.frame(X = validation$X, `Made Donation in March 2007` = rpartProbs$Yes)
write.table(rpartSubmission, 
            paste0('rpartSubmission', format(Sys.time(), "%Y%m%d_%H%M%S"), '.csv'),
            row.names = FALSE,
            col.names = c('', 'Made Donation in March 2007'), sep=',', quote = FALSE)

rfProbs <- extractProb(list(rf$fit), unkX = validation)
rfSubmission <- data.frame(X = validation$X, `Made Donation in March 2007` = rfProbs$Yes)
write.table(rfSubmission, 
            paste0('rfSubmission', format(Sys.time(), "%Y%m%d_%H%M%S"), '.csv'),
            row.names = FALSE,
            col.names = c('', 'Made Donation in March 2007'), sep=',', quote = FALSE)

library(rpart.plot)
prp(rpartModel$fit$finalModel)


##### RVD based

myTrain <- function(train) {
    set.seed(1234)
    inTrain <- createDataPartition(y=train$Made.Donation.in.March.2007, p=0.8, list=FALSE)
    training <- train[inTrain,]
    testing <- train[-inTrain,]
    tc <- trainControl(classProbs = TRUE, method = "repeatedcv",
                       number = 5,
                       repeats = 10)
    tc2 <- trainControl(classProbs = TRUE, method = "repeatedcv",
                        number = 2,
                        repeats = 10, summaryFunction = twoClassSummary)
    set.seed(1234)
    glmModel <- testModel(Made.Donation.in.March.2007 ~ ., as.data.frame(training),
                          as.data.frame(testing),
                          'Made.Donation.in.March.2007',
                          'glm', trControl = tc, metric = "Kappa")
    
    rpartModel <- testModel(Made.Donation.in.March.2007 ~ ., training,
                            testing,
                            'Made.Donation.in.March.2007',
                            'rpart', trControl = tc, metric = "Kappa")
    
    rf <- testModel(Made.Donation.in.March.2007 ~ ., training, testing,
                    'Made.Donation.in.March.2007',
                    'rf',
                    trControl = tc, metric = "Kappa")
    list(glmModel = glmModel, rpartModel = rpartModel, rf = rf)
}

models <- list()
models[['RVDTRUE']] <- myTrain(train[train$RVDTRUE == 1,])
models[['RVDFALSE']] <- myTrain(train[train$RVDTRUE == 0,])

resamp <- resamples(
    list(GLMTRUE=models$RVDTRUE$glmModel$fit,
         RPARTTRUE=models$RVDTRUE$rpartModel$fit,
         RFTRUE=models$RVDTRUE$rf$fit,
         GLMFALSE=models$RVDFALSE$glmModel$fit,
         RPARTFALSE=models$RVDFALSE$rpartModel$fit,
         RFFALSE=models$RVDFALSE$rf$fit
         ))
summary(resamp)
bwplot(resamp)

myExtractProbs <- function(validation, model) {
    rpartProbs <- extractProb(list(model$fit), unkX = validation)
    data.frame(X = validation$X, `Made Donation in March 2007` = rpartProbs$Yes)
}

library(plyr)
submission <- rbind(myExtractProbs(validation[validation$RVD == 1,], models$RVDTRUE$rpartModel),
                    myExtractProbs(validation[validation$RVD == 0,], models$RVDTRUE$rpartModel))

submission <- join(validation, submission, by = 'X', match = 'first')[,c(1,7)]

write.table(submission, 
            paste0('submission', format(Sys.time(), "%Y%m%d_%H%M%S"), '.csv'),
            row.names = FALSE,
            col.names = c('', 'Made Donation in March 2007'), sep=',', quote = FALSE)
