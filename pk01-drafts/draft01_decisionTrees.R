##########################################
# Decision Trees for BHI paper           #
# ______________________________________ #
# Matin (matinkheirkhahan@ufl.edu)       #
##########################################

load("~/Dropbox/Work-Research/Current Directory/Smart Data Collection - Realtime Weartime Detection/Datasets/Raw watch data/111016/collectedData/rawROAMM_111016.Rdata")

library(rpart)
library(rpart.plot)


dataset.df <- weartime_dataset.df[, -c(1, 2, 10, 11)]
myFit <- rpart(status~sdvm+sdangle, data = dataset.df, control = rpart.control(maxdepth = 3))
rpart.plot(myFit)

dataset2.df <- dataset.df[dataset.df$status != "wear", ]
myFit2 <- rpart(status~sdvm+mvm, data = dataset2.df, control = rpart.control(maxdepth = 2))
rpart.plot(myFit2)
