# Case Study Decision Tree

install.packages("mlbench")
# load the mlbench package which has the BreastCancer data set
require(mlbench)
# if you don't have any required package, use the install.packages() command

# load the data set
data(BreastCancer)

# some algorithms don't like missing values, so remove rows with missing values
BreastCancer <- na.omit(BreastCancer)
table(is.na(BreastCancer))

# remove the unique identifier, which is useless and would confuse the machine learning algorithms
BreastCancer$Id <- NULL

# partition the data set for 80% training and 20% evaluation (adapted from ?randomForest)
train<-sample(1:nrow(BreastCancer),0.7*nrow(BreastCancer))
traindata <- BreastCancer[train,]
testdata <- BreastCancer[-train,]

## method 1: create model using recursive partitioning on the training data set
require(rpart)
x.rp <- rpart(Class ~ ., data=traindata)
# predict classes for the evaluation data set
x.rp.pred <- predict(x.rp, type="class", newdata=testdata)
x.rp.pred

# score the evaluation data set (extract the probabilities)
x.rp.prob <- predict(x.rp, type="prob", newdata=testdata)
x.rp.prob 

# To view the decision tree, uncomment this line.
dev.new()
plot(x.rp, main="Decision tree fm rpart")

## method 2: create model using conditional inference trees
install.packages("party")
require(party)
x.ct <- ctree(Class ~ ., data=traindata)
x.ct.pred <- predict(x.ct, testdata)
x.ct.prob <-  1- unlist(treeresponse(x.ct, testdata), use.names=F)[seq(1,nrow(testdata)*2,2)]
plot(x.ct)
# load the ROCR package which draws the ROC curves
install.packages("ROCR")
require(ROCR)
# create an ROCR prediction object from rpart() probabilities
x.rp.prob.rocr <- prediction(x.rp.prob[,2], testdata$Class)
x.rp.prob.rocr

# prepare an ROCR performance object for ROC curve (tpr=true positive rate, fpr=false positive rate)
x.rp.perf <- performance(x.rp.prob.rocr, "tpr","fpr")
x.rp.perf

# plot it
dev.new()
plot(x.rp.perf, col=2, main="ROC curve")

