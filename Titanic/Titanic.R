setwd("~/Code/R/R Projects/Titanic")

library('dplyr')
library('ggplot2')
library('caret')


train <- read.csv('train.csv')
test <- read.csv('test.csv')

str(train)
table(train$Survived)
prop.table(table(train$Survived))

# Assuming everyone in test set died
test$Survived <- rep(0, 418)

# Creating Submission file
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(x = submit, file = 'theyallperish.csv', row.names = F)

# compare sex with survived
prop.table(table(train$Sex, train$Survived), margin = 1)
# margin = 1 provides proportion for each row

# Assuming all females in test set survived and all males died
test$Survived[test$Sex == 'female'] <- 1


# Creating Submission file
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(x = submit, file = 'allfemalessurivive.csv', row.names = F)


# Dealing with Age
summary(train$Age)

# Creating Child age group
train$Child <- 0
train$Child[train$Age < 18] <- 1
# here we are assuming all NA ages are adults

# Finding survival based on Sex and Child
# aggregate(Survived ~ Sex + Child, data = train, FUN = sum)
# aggregate(Survived ~ Sex + Child, data = train, FUN = length)
aggregate(Survived ~ Sex + Child, data = train, 
          FUN = function(x){ sum(x) / length(x) })

# No New conclusion here. Females survive.


# Checking the Pclass and Fare
# Since Fare is continous, we divide it into discrete groups
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

# Finding survival based on Fare, Pclass & Sex
aggregate(Survived ~ Fare2 + Pclass + Sex, data = train, 
          FUN = function(x){ sum(x) / length(x) })

# we notice that most of the class 3 women who paid more 
# than $20 for their ticket actually have low survival rate
# Adding this to submission
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0

# Creating Submission file
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(x = submit, file = 'class3fare20femalesperish.csv', row.names = F)


##########################################################################

# Using Decision Trees
library('rpart')
fit <- rpart(formula = Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare, 
             data = train, method = 'class')

# new packages for better graphics
library('rattle')
library('rpart.plot')
library('RColorBrewer')

# generating the Decision Tree
fancyRpartPlot(model = fit)

# generating predictions
Prediction <- predict(fit, newdata = test, type = 'class')

# Creating Submission file
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(x = submit, file = 'myfirstdtree.csv', row.names = F)

# Going deeper into dtree
fit <- rpart(formula = Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare, 
             data = train, method = 'class', 
             control = rpart.control(minsplit = 2, cp = 0))
fancyRpartPlot(model = fit)

# generating predictions
Prediction <- predict(fit, newdata = test, type = 'class')


# Creating Submission file
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(x = submit, file = 'maxdtree.csv', row.names = F)

# Interactive Pruning
#new.fit <- prp(x = fit, snip = T)$obj
#fancyRpartPlot(new.fit)

###########################################################################

# Feature Engineering

# Getting original train and test datasets
train2 <- train
train <- train[, 1:12]
train.org <- train

test2 <- test
test <- test[, 1:11]
test.org <- test

# Adding NA Survived to test and combining both
test$Survived <- NA
combi <- rbind(train, test)

# Working on Name field
combi$Name <- as.character(combi$Name) 
combi$Name[1]

# Getting Title for the first name
strsplit(x = combi$Name[1], split = '[,.]')
strsplit(x = combi$Name[1], split = '[,.]') [[1]]
strsplit(x = combi$Name[1], split = '[,.]') [[1]] [2]

# Getting Title of all names
combi$Title <- sapply(X = combi$Name, FUN = function(x){
    strsplit(x , split = '[,.]') [[1]] [2]})
                      
# trimming spaces from Title
combi$Title <- sub(pattern = ' ', replacement = '', x = combi$Title)

# Looking at all the titles
table(combi$Title)

# Combining Redudant titles
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Dona', 'Jonkheer', 'Lady', 'the Countess')] <- 'Lady'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'

# Turning Title into factor
combi$Title <- as.factor(combi$Title)

# Getitng FamilySize
combi$FamilySize <- combi$SibSp + combi$Parch + 1

# Getting Surname
combi$Surname <- sapply(X = combi$Name, FUN = function(x){
    strsplit(x , split = '[,.]') [[1]] [1]})

# Creating FamilyID by combining Surname
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep = '')

# Three single Johnsons would all have the same Family ID. 
# Given we were originally hypothesising that large families might have 
# trouble sticking together in the panic, let’s knock out any family size
# of two or less and call it a “small” family. 
# This would fix the Johnson problem too.
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

# check the familyid
table(combi$FamilyID)

# a few seemed to have slipped through the cracks here. 
# There’s plenty of FamilyIDs with only one or two members, 
# even though we wanted only family sizes of 3 or more.
# Lets clean it up
famID <- data.frame(table(combi$FamilyID))

# subset this dataframe to show only those unexpectedly small FamilyID groups
famID <- famID[famID$Freq <= 2, ]

# convert FamilyID with size <= 2 to 'Small'
combi$FamilyID[combi$FamilyID %in% famID$Var1] <- 'Small'
combi$FamilyID <- as.factor(combi$FamilyID)


# Splitting Combi into Train and Test
train <- combi[1:891, ]
test <- combi[892:1309, ]

# Predict using Decision Tree
fit <- rpart(formula = Survived ~ Pclass + Sex + Age + SibSp + Parch + 
                 Fare + Embarked + Title + FamilySize + FamilyID, 
             data = train, method = 'class')

fancyRpartPlot(model = fit)

Prediction <- predict(fit, newdata = test, type = 'class')

# Creating Submission file
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(x = submit, file = 'featuredtree.csv', row.names = F)

########################################################################

# Random Forest

# Cleaning Age variable by predicting age using Anova
summary(combi$Age)

# We now also want to use the method="anova" version of our decision tree, 
# as we are not trying to predict a category any more, but a continuous variable. 
#So let’s grow a tree on the subset of the data with the age values available, 
# and then replace those that are missing
Agefit <- rpart(formula = Survived ~ Pclass + Sex + Age + SibSp + Parch + 
    Fare + Embarked + Title + FamilySize, 
    data = combi[!is.na(combi$Age), ], method = 'anova')

combi$Age[is.na(combi$Age)] <- predict(object = Agefit, 
                                   newdata = combi[is.na(combi$Age), ])

# no more NA ages
summary(combi$Age)

# check for any more NA in combi
summary(combi)

# Embarked has a blank for two passengers. 
# Because it’s so few observations and such a large majority 
# boarded in Southampton, let’s just replace those two with “S”.
combi$Embarked[which(combi$Embarked == '')] <- 'S'
combi$Embarked <- as.factor(combi$Embarked)

# Fare has an NA. so let’s find out which one it is and replace it with the 
# median fareso let’s find out which one it is and replace it with the median fare
combi$Fare[which(is.na(combi$Fare))] <- median(combi$Fare, na.rm = T)

# no more NAs
summary(combi)


# Random Forests in R can only digest factors with up to 32 levels.
# reducing the number of levels in familyid to keep it under the threshold
combi$FamilyID2 <- as.character(combi$FamilyID)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- as.factor(combi$FamilyID2)

# Splitting Combi into Train and Test
train <- combi[1:891, ]
test <- combi[892:1309, ]


# Starting RF
library('randomForest')

set.seed(415)

fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                        Fare + Embarked + Title + FamilySize + FamilyID2, 
                    data = train, importance = T, ntree = 2000)

# let’s look at what variables were important
varImpPlot(x = fit)

# Predict
Prediction <- predict(fit, newdata = test)

# Creating Submission file
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(x = submit, file = 'firstforest.csv', row.names = F)

############################################################################
# forest of conditional inference trees

library('party')
set.seed(415)

fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                        Fare + Embarked + Title + FamilySize + FamilyID, 
                    data = train, 
               controls = cforest_unbiased(ntree = 2000, mtry = 3))

Prediction <- predict(object = fit, newdata = test, OOB = T, type = 'response')

# Creating Submission file
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(x = submit, file = 'cforest.csv', row.names = F)
