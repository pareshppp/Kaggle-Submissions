# set WD
setwd("~/Code/R/R Projects/Iris")

# read data
iris <- read.csv('iris_data.csv', header = F)
colnames(iris) <- c('sepal.length', 'sepal.width', 
                       'petal.length', 'petal.width', 'class')

# iris.train and iris.test datasets
# iris.train <- iris[1:abs(nrow(iris) * 0.8), ]
# iris.test <- iris[(nrow(iris.train)+1):nrow(iris), ]
iris.train <- iris[c(1:40, 51:90, 101:140), ]
iris.test <- iris[c(41:50, 91:100, 141:150), ]

library(dplyr)
iris <- tbl_df(iris)
iris.train <- tbl_df(iris.train)
iris.test <- tbl_df(iris.test)

# Summarize
dim(iris.train)
sapply(X = iris.train, FUN = class)
summary(iris.train)

# distribution
iris.train.distr <- iris.train %>% group_by(class) %>% 
    summarise(cnt = n()) %>% 
    mutate(percent = cnt / nrow(iris.train))

# Visualize
library(ggplot2)
library(caret)
# 
# ggplot(data = iris.train, 
#        aes(x = iris.train$sepal.length, 
#            y = iris.train$sepal.width, 
#            color = iris.train$class)) + 
#     geom_point() 

# pairs(x = iris.train[, 1:4], col = iris.train$class)

featurePlot(x = iris.train[, 1:4], y = iris.train[, 5], plot = 'pairs')
featurePlot(x = iris.train[, 1:4], y = iris.train[, 5], plot = 'box')



# 10-fold cross validation
control <- trainControl(method = 'cv', number = 10, verboseIter = T)
metric <- 'Accuracy'

# Building models
# LDA
set.seed(7)
fit.lda <- train(class ~ ., data = iris.train, method = 'lda', 
                 metric = metric, trControl = control)

# CART
set.seed(7)
fit.cart <- train(class ~ ., data = iris.train, method = 'rpart', 
                 metric = metric, trControl = control)

# KNN
set.seed(7)
fit.knn <- train(class ~ ., data = iris.train, method = 'knn', 
                  metric = metric, trControl = control)

# SVM
set.seed(7)
fit.svm <- train(class ~ ., data = iris.train, method = 'svmRadial', 
                 metric = metric, trControl = control)

# Random Forest
set.seed(7)
fit.rf <- train(class ~ ., data = iris.train, method = 'rf', 
                 metric = metric, trControl = control)


# Results
result <- resamples(list(fit.lda, fit.cart, fit.knn, fit.rf, fit.svm))
summary(result)

dotplot(result)

# Best Model
print(fit.lda)


# Prediction
prediction <- predict(fit.lda, iris.test[, 1:4])
confusionMatrix(data = prediction, reference = iris.test$class)
