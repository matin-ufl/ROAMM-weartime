#########################################################
# Wear-time detection using accelerometer features      #
#                                                       #
# _____________________________________________________ #
# Matin Kheirkhahan                                     #
#########################################################

# Functions ---------------------------------------
checkTimeDifference <- function(reference, anotherTimestamp) {
     ref.time <- strptime(reference, format = "%Y-%m-%d %H:%M")
     another.time <- strptime(anotherTimestamp, format = "%Y-%m-%d %H:%M")
     a <- as.numeric(as.difftime(ref.time - another.time, units = "mins"))
     abs(a)
}

majorityVote <- function(predictions.toConsider) {
     votes <- levels(as.factor(predictions.toConsider))
     for(v in votes) {
          if(sum(predictions.toConsider == v) >= (length(predictions.toConsider) / 2)) {
               return(as.character(v))
          }
     }
     predictions.toConsider[1]
}

classifierByMovingAverage <- function(timestamps, predictions) {
     result <- rep(F, length(predictions))
     for(i in 1:length(predictions)) {
          predictions.toConsider <- c()
          for(j in (i-2):(i+2)) {
               if(j > 0 && j < length(predictions)) {
                    if(checkTimeDifference(timestamps[i], timestamps[j]) < 3) {
                         predictions.toConsider <- c(predictions.toConsider, predictions[j])
                    }
               }
          }
          result[i] <- majorityVote(predictions.toConsider)
     }
     result
}

accuracyFunction <- function(actual.vector, predicted.vector) {
     t <- table(actual.vector, predicted.vector)
     actualTrue <- sum(t[2, ])
     TP <- t[2, 2]
     TP / actualTrue * 100
}

# Loading and checking the dataset -------------------------

# Gathering all smartwatch data into one data.frame
load("~/Dropbox/Work-Research/Current Directory/Smart Data Collection - Realtime Weartime Detection/Datasets/Raw watch data/110216/rawROAMM_110216.Rdata")
nov02 <- weartime_dataset.df
load("~/Dropbox/Work-Research/Current Directory/Smart Data Collection - Realtime Weartime Detection/Datasets/Raw watch data/110316/rawROAMM_110316.Rdata")
nov03 <- weartime_dataset.df
load("~/Dropbox/Work-Research/Current Directory/Smart Data Collection - Realtime Weartime Detection/Datasets/Raw watch data/111016/collectedData/rawROAMM_111016.Rdata")
nov10 <- weartime_dataset.df
weartime_dataset.df <- rbind(nov02, nov03, nov10)
rm(list = ls(pattern = "^nov.*$"))


# Summary of features across groups
wear.idx <- which(weartime_dataset.df$status == "wear")
nonwear.idx <- which(weartime_dataset.df$status == "nonwear")
inBag.idx <- which(weartime_dataset.df$status == "in-bag")
# minute
wear.feature <- round(length(wear.idx) * 15 / 60)
nonwear.feature <- round(length(nonwear.idx) * 15 / 60)
inBag.feature <- round(length(inBag.idx) * 15 / 60)
totalMinutes <- round(nrow(weartime_dataset.df) * 15 / 60)
print(paste("minutes & ", wear.feature, " (", round(wear.feature / totalMinutes * 100), ") & ",
            inBag.feature, " (", round(inBag.feature / totalMinutes * 100), ") & ",
            nonwear.feature, " (", round(nonwear.feature / totalMinutes * 100), ") \\", sep = ""))

for(i in 3:9) {
     featurename <- colnames(weartime_dataset.df)[i]
     weartime_dataset.df[which(is.na(weartime_dataset.df[, i])), i] <- mean(weartime_dataset.df[, i], na.rm = T)
     wear.feature <- weartime_dataset.df[wear.idx, i]
     nonwear.feature <- weartime_dataset.df[nonwear.idx, i]
     inBag.feature <- weartime_dataset.df[inBag.idx, i]
     print(paste(featurename, " & ",
                 round(mean(wear.feature), digits = 2), " (", round(sd(wear.feature), digits = 2), ") & ",
                 round(mean(inBag.feature), digits = 2), " (", round(sd(inBag.feature), digits = 2), ") & ",
                 round(mean(nonwear.feature), digits = 2), " (", round(sd(nonwear.feature), digits = 2), ") \\", sep = ""))
}

# First dataset: nonwear vs moving --------------------------
allDataset.df <- weartime_dataset.df[, c("dateTime", "mvm", "sdvm", "mangle", "sdangle", "p625", "df", "fpdf", "status")]
allDataset.df$nonwear <- as.factor(allDataset.df$status == "nonwear")
allDataset.df <- allDataset.df[, -(ncol(allDataset.df) - 1)] # Discarding status
# Outcome has: 1. actual
#              2. LDA
#              3. 3-NN    --> Scaled dataset should be used
#              4. 5-NN
#              5. 10-NN
#              6. Decision Tree
#              7. Random Forest
#              7. SVM
#              8. Decision Tree + Moving Average (4)

alloutcome.df <- data.frame(matrix(F, nrow = nrow(allDataset.df), ncol = 9)); colnames(alloutcome.df) <- c("actual", "LDA", "3-NN", "5-NN", "10-NN", "Decision Tree", "Random Forest", "SVM", "DT.MA4")
alloutcome.df$actual <- allDataset.df$nonwear

# LDA
library(MASS)
for(i in 1:nrow(allDataset.df)) {
     test.sample <- allDataset.df[i, -c(1, ncol(allDataset.df))]
     training.set <- allDataset.df[-i, -1]
     myFit <- lda(nonwear~., data = training.set)
     alloutcome.df$LDA[i] <- as.character(predict(myFit, newdata = test.sample)$class)
}
rm(training.set, test.sample, i, myFit)

# kNN
library(class)
scaledAllDataset.df <- allDataset.df
for(i in 2:(ncol(scaledAllDataset.df) - 1)) {
     scaledAllDataset.df[, i] <- (scaledAllDataset.df[, i] - min(scaledAllDataset.df[, i])) / (max(scaledAllDataset.df[, i]) - min(scaledAllDataset.df[, i]))
}

# 3-NN
for(i in 1:nrow(scaledAllDataset.df)) {
     test.sample <- scaledAllDataset.df[i, -c(1, ncol(scaledAllDataset.df))]
     training.set <- scaledAllDataset.df[-i, -c(1)]
     alloutcome.df$`3-NN`[i] <- as.character(knn(train = training.set[, -ncol(training.set)], test = test.sample, k = 3, cl = training.set$nonwear))
}
rm(training.set, test.sample, i, myFit)

# 5-NN
for(i in 1:nrow(scaledAllDataset.df)) {
     test.sample <- scaledAllDataset.df[i, -c(1, ncol(scaledAllDataset.df))]
     training.set <- scaledAllDataset.df[-i, -c(1)]
     alloutcome.df$`5-NN`[i] <- as.character(knn(train = training.set[, -ncol(training.set)], test = test.sample, k = 5, cl = training.set$nonwear))
}
rm(training.set, test.sample, i, myFit)

# 10-NN
for(i in 1:nrow(scaledAllDataset.df)) {
     test.sample <- scaledAllDataset.df[i, -c(1, ncol(scaledAllDataset.df))]
     training.set <- scaledAllDataset.df[-i, -c(1)]
     alloutcome.df$`10-NN`[i] <- as.character(knn(train = training.set[, -ncol(training.set)], test = test.sample, k = 10, cl = training.set$nonwear))
}
rm(training.set, test.sample, i, myFit)
rm(scaledAllDataset.df)

# Decision Tree
library(rpart)
for(i in 1:nrow(allDataset.df)) {
     test.sample <- allDataset.df[i, -c(1, ncol(allDataset.df))]
     training.set <- allDataset.df[-i, -c(1)]
     myFit <- rpart(nonwear~., data = training.set, control = rpart.control(minsplit = 15, maxdepth = 20))
     alloutcome.df$`Decision Tree`[i] <- as.character(predict(myFit, newdata = test.sample) < 0.5)
}
rm(training.set, test.sample, i, myFit)

# Random Forest
library(randomForest)
myFit <- randomForest(nonwear~., data = allDataset.df[, -1], mtry = 2, ntree = 10000)
alloutcome.df$`Random Forest` <- as.character(myFit$predicted)

# SVM
library(e1071)
for(i in 1:nrow(allDataset.df)) {
     test.sample <- allDataset.df[i, -c(1, ncol(allDataset.df))]
     training.set <- allDataset.df[-i, -c(1)]
     myFit <- svm(nonwear~., data = training.set)
     alloutcome.df$SVM[i] <- as.character(predict(myFit, newdata = test.sample))
}
rm(training.set, test.sample, i, myFit)

# Decisiton Tree + Moving Average (5)
alloutcome.df$DT.MA4 <- classifierByMovingAverage(allDataset.df$dateTime, alloutcome.df$`Decision Tree`)

# Printing non-wear accuracy of the methods
print(paste("non-wear accuracies: LDA (", accuracyFunction(alloutcome.df$actual, alloutcome.df$LDA), ") - ",
      "3-NN (", accuracyFunction(alloutcome.df$actual, alloutcome.df$`3-NN`), ") - ",
      "5-NN (", accuracyFunction(alloutcome.df$actual, alloutcome.df$`5-NN`), ") - ",
      "10-NN (", accuracyFunction(alloutcome.df$actual, alloutcome.df$`10-NN`), ") - ",
      "DT (", accuracyFunction(alloutcome.df$actual, alloutcome.df$`Decision Tree`), ") - ",
      "RF (", accuracyFunction(alloutcome.df$actual, alloutcome.df$`Random Forest`), ") - ",
      "SVM (", accuracyFunction(alloutcome.df$actual, alloutcome.df$SVM), ") - ",
      "DT.MA4 (", accuracyFunction(alloutcome.df$actual, alloutcome.df$DT.MA4), ")", sep = ""))

DT.HR.inBagAccuracy <- (length(inBag.idx) - alloutcome.df$`Decision Tree`[inBag.idx]) / length(inBag.idx) * 100


# Second dataset: wear vs in-bag ----------------------------
wearDataset.df <- weartime_dataset.df[weartime_dataset.df$status != "nonwear", c("dateTime", "mvm", "sdvm", "mangle", "sdangle", "p625", "df", "fpdf", "status")]

wearDataset.df$wear <- as.factor(wearDataset.df$status == "wear")
wearDataset.df <- wearDataset.df[, -(ncol(wearDataset.df) - 1)] # Discarding status
# Outcome has: 1. actual
#              2. LDA
#              3. 3-NN    --> Scaled dataset should be used
#              4. 5-NN
#              5. 10-NN
#              6. Decision Tree
#              7. Random Forest
#              7. SVM
#              8. Decision Tree + Moving Average (4)

wearoutcome.df <- data.frame(matrix(F, nrow = nrow(wearDataset.df), ncol = 9)); colnames(wearoutcome.df) <- c("actual", "LDA", "3-NN", "5-NN", "10-NN", "Decision Tree", "Random Forest", "SVM", "DT.MA4")
wearoutcome.df$actual <- wearDataset.df$wear

# LDA
library(MASS)
for(i in 1:nrow(wearDataset.df)) {
     test.sample <- wearDataset.df[i, -c(1, ncol(wearDataset.df))]
     training.set <- wearDataset.df[-i, -1]
     myFit <- lda(wear~., data = training.set)
     wearoutcome.df$LDA[i] <- as.character(predict(myFit, newdata = test.sample)$class)
}
rm(training.set, test.sample, i, myFit)

# kNN
library(class)
scaledAllDataset.df <- wearDataset.df
for(i in 2:(ncol(scaledAllDataset.df) - 1)) {
     scaledAllDataset.df[, i] <- (scaledAllDataset.df[, i] - min(scaledAllDataset.df[, i])) / (max(scaledAllDataset.df[, i]) - min(scaledAllDataset.df[, i]))
}

# 3-NN
for(i in 1:nrow(scaledAllDataset.df)) {
     test.sample <- scaledAllDataset.df[i, -c(1, ncol(scaledAllDataset.df))]
     training.set <- scaledAllDataset.df[-i, -c(1)]
     wearoutcome.df$`3-NN`[i] <- as.character(knn(train = training.set[, -ncol(training.set)], test = test.sample, k = 3, cl = training.set$wear))
}
rm(training.set, test.sample, i)

# 5-NN
for(i in 1:nrow(scaledAllDataset.df)) {
     test.sample <- scaledAllDataset.df[i, -c(1, ncol(scaledAllDataset.df))]
     training.set <- scaledAllDataset.df[-i, -c(1)]
     wearoutcome.df$`5-NN`[i] <- as.character(knn(train = training.set[, -ncol(training.set)], test = test.sample, k = 5, cl = training.set$wear))
}
rm(training.set, test.sample, i)

# 10-NN
for(i in 1:nrow(scaledAllDataset.df)) {
     test.sample <- scaledAllDataset.df[i, -c(1, ncol(scaledAllDataset.df))]
     training.set <- scaledAllDataset.df[-i, -c(1)]
     wearoutcome.df$`10-NN`[i] <- as.character(knn(train = training.set[, -ncol(training.set)], test = test.sample, k = 10, cl = training.set$wear))
}
rm(training.set, test.sample, i)
rm(scaledAllDataset.df)

# Decision Tree
library(rpart)
for(i in 1:nrow(wearDataset.df)) {
     test.sample <- wearDataset.df[i, -c(1, ncol(wearDataset.df))]
     training.set <- wearDataset.df[-i, -c(1)]
     myFit <- rpart(wear~., data = training.set, control = rpart.control(minsplit = 15, maxdepth = 20))
     wearoutcome.df$`Decision Tree`[i] <- as.character(predict(myFit, newdata = test.sample) < 0.5)
}
rm(training.set, test.sample, i, myFit)

# Random Forest
library(randomForest)
myFit <- randomForest(wear~., data = wearDataset.df[, -1], mtry = 2, ntree = 10000)
wearoutcome.df$`Random Forest` <- as.character(myFit$predicted)

# SVM
library(e1071)
for(i in 1:nrow(wearDataset.df)) {
     test.sample <- wearDataset.df[i, -c(1, ncol(wearDataset.df))]
     training.set <- wearDataset.df[-i, -c(1)]
     myFit <- svm(wear~., data = training.set)
     wearoutcome.df$SVM[i] <- as.character(predict(myFit, newdata = test.sample))
}
rm(training.set, test.sample, i, myFit)

# Decisiton Tree + Moving Average (5)
wearoutcome.df$DT.MA4 <- classifierByMovingAverage(wearDataset.df$dateTime, wearoutcome.df$`Decision Tree`)

# Printing wear accuracy of the methods
print(paste("wear accuracies: LDA (", accuracyFunction(wearoutcome.df$actual, wearoutcome.df$LDA), ") - ",
            "3-NN (", accuracyFunction(wearoutcome.df$actual, wearoutcome.df$`3-NN`), ") - ",
            "5-NN (", accuracyFunction(wearoutcome.df$actual, wearoutcome.df$`5-NN`), ") - ",
            "10-NN (", accuracyFunction(wearoutcome.df$actual, wearoutcome.df$`10-NN`), ") - ",
            "DT (", accuracyFunction(wearoutcome.df$actual, wearoutcome.df$`Decision Tree`), ") - ",
            "RF (", accuracyFunction(wearoutcome.df$actual, wearoutcome.df$`Random Forest`), ") - ",
            "SVM (", accuracyFunction(wearoutcome.df$actual, wearoutcome.df$SVM), ") - ",
            "DT.MA4 (", accuracyFunction(wearoutcome.df$actual, wearoutcome.df$DT.MA4), ")", sep = ""))

# Printing in-bag accuracy of the methods
print(paste("in-bag accuracies: LDA (", accuracyFunction(wearoutcome.df$actual == F, wearoutcome.df$LDA == F), ") - ",
            "3-NN (", accuracyFunction(wearoutcome.df$actual == F, wearoutcome.df$`3-NN` == F), ") - ",
            "5-NN (", accuracyFunction(wearoutcome.df$actual == F, wearoutcome.df$`5-NN` == F), ") - ",
            "10-NN (", accuracyFunction(wearoutcome.df$actual == F, wearoutcome.df$`10-NN` == F), ") - ",
            "DT (", accuracyFunction(wearoutcome.df$actual == F, wearoutcome.df$`Decision Tree` == F), ") - ",
            "RF (", accuracyFunction(wearoutcome.df$actual == F, wearoutcome.df$`Random Forest` == F), ") - ",
            "SVM (", accuracyFunction(wearoutcome.df$actual == F, wearoutcome.df$SVM == F), ") - ",
            "DT.MA4 (", accuracyFunction(wearoutcome.df$actual == F, wearoutcome.df$DT.MA4 == F), ")", sep = ""))

