
#############################################################################################################################################################
## Objective:  Machine learning of iris species classification with decision tree                                                                           #
## Data source: iris data set                                                                                                                               #
## Please install "rpart" package: install.packages("rpart") for decision trees                                                                             #
## Please install "party" package: install.packages("party") for recursive partitioning                                                                     #
#############################################################################################################################################################


## load the library
library(rpart)

## BUILD MODEL
## randomly choose 70% of the data set as training data
set.seed(777)
train.index <- sample(1:nrow(iris), 0.7*nrow(iris))
iris.train <- iris[train.index,]
dim(iris.train)
## select the 30% left as the testing data
iris.test <- iris[-train.index,]
dim(iris.test)

# Default decision tree model
    # Builds a decision tree from the iris dataset to predict
    # species given all other columns as predictors
iris.tree <- rpart(Species~.,data=iris.train)

## VISUALIZE THE MODEL
## plot the tree structure
plot(iris.tree, margin=c(.25))
title(main = "Decision Tree Model of Iris Data")
text(iris.tree, use.n = TRUE)
## print the tree structure
summary(iris.tree)

## MODEL EVALUATION
## make prediction using decision model
iris.predictions <- predict(iris.tree, iris.test, type = "class")
head(iris.predictions)

## Comparison table
iris.comparison <- iris.test
iris.comparison$Predictions <- iris.predictions
iris.comparison[ , c("Species", "Predictions")]

## View misclassified rows
disagreement.index <- iris.comparison$Species != iris.comparison$Predictions
iris.comparison[disagreement.index,]

## If instead you wanted probabilities.
# iris.predictions <- predict(iris.tree, iris.test, type = "prob")

## Extract the test data species to build the confusion matrix
iris.confusion <- table(iris.predictions, iris.test$Species)

iris.confusion

#### Parameter Tuning ####

## Setting control parameters for rpart
## Check ?rpart.control for what the parameters do
tree.params <- rpart.control(minsplit=20, minbucket=7, maxdepth=30, cp=0.01)

## Fit decision model to training set
## Use parameters from above and Gini index for splitting
iris.tree <- rpart(Species ~ ., data = iris.train, 
                       control=tree.params, parms=list(split="gini"))

### Regression Decision Tree ####
# Use method as "anova" as a parameter.
iris.tree <- rpart(Petal.Length ~ ., data = iris.train, method="anova")


#MY NOTES
data(iris)
nrow(iris)
1:nrow(iris) #only indices

train.index <- sample(1:nrow(iris), 0.7*nrow(iris)) #create train index

train.index

iris.train <- iris[train.index,]
iris.test <- iris[-train.index,] #just use minus sign to create test it will undestand

dim(iris.train) 
dim(iris.test)

iris.tree <- rpart(Species~., data=iris.train) #rpart creates decision tree, You want to predict species. 
# tilde means a separator, part of sintax. The dot means all columns in the training data to use
#Or can say variable x1 + x2

#default is entropy
head(iris.train)

iris.tree

boxplot(Petal.Width ~ Species, data=iris.train)
boxplot(Petal.Length ~ Species, data=iris.train)

levels(iris$Species)


iris.predictions <- predict(iris.tree, iris.test, type="class") #gives you labes

iris.predictions <- predict(iris.tree, iris.test, type="prob") #gives you probabilites


## MODEL EVALUATION
## make prediction using decision model
iris.predictions <- predict(iris.tree, iris.test, type = "class")
head(iris.predictions)

## Comparison table
iris.comparison <- iris.test
iris.comparison$Predictions <- iris.predictions
iris.comparison[ , c("Species", "Predictions")]

## View misclassified rows
disagreement.index <- iris.comparison$Species != iris.comparison$Predictions
iris.comparison[disagreement.index,]

## If instead you wanted probabilities.
# iris.predictions <- predict(iris.tree, iris.test, type = "prob")

## Extract the test data species to build the confusion matrix
iris.confusion <- table(iris.predictions, iris.test$Species)

#### Parameter Tuning ####

## Setting control parameters for rpart
## Check ?rpart.control for what the parameters do
tree.params <- rpart.control(minsplit=20, minbucket=7, maxdepth=30, cp=0.01)

## Fit decision model to training set
## Use parameters from above and Gini index for splitting
iris.tree <- rpart(Species ~ ., data = iris.train, 
                   control=tree.params, parms=list(split="gini"))

### Regression Decision Tree ####
# Use method as "anova" as a parameter.
iris.tree <- rpart(Petal.Length ~ ., data = iris.train, method="anova")


