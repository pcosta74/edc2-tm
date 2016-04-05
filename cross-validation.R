library(class)
library(rpart)
library(nnet)
library(e1071)
library(RWeka)

# *************************************************
# Generate sample sizes for k-fold cross validation
# on a dataset of size n

cv.sizes <- function(n, k=10) {
  sizes <- c()
  for (i in 1:k) {
    first <- 1 + (((i - 1) * n) %/% k)
    last  <- ((i * n) %/% k)
    sizes <- append(sizes, last - first + 1)
  }
  return(sizes)
} # End function cv.sizes


# *************************************************
# Generate testing sample indices for k-fold cross 
# validation on a data set of size n

cv.testing <- function(n, k=10) {
  indices <- list()
  sizes   <- cv.sizes(n, k)
  values  <- 1:n
  for (i in 1:k) {
    # take a random sample of given size
    s <- sample(values, sizes[i])
    # append random sample to list of indices
    indices[[i]] <- s
    # remove sample from values
    values <- setdiff(values, s)
  }
  return(indices)
} # End function cv.testing 


# *************************************************
# Run k-fold cross validation with an arbitrary classifier
# classifier: function which takes a training set, 
#             a test set and a formula
# data: data frame where each column is an attribute and
#       each row is an observation,
# formula: the formula for the classifier
# folds: number of folds

cross.validation <- function(CLASS.FUN, formula, data, folds=10, simplify=T, ...) {
  test.indices <- cv.testing(dim(data)[1])
  class  <- all.vars(update(formula, .~0))
  result <- list()
  for (i in 1:folds) {
    test.ndx <- test.indices[[i]]
    train <- data[-test.ndx,]
    test  <- data[test.ndx,]
    result[[i]] <- CLASS.FUN(formula, train, test, class, ...)
  }
  class(result) <- 'cv.list'
  return(result)
} # End function cross.validation


# *************************************************
# Decision Tree

cv.rpart <- function(formula, train, test, class, ...) {
  lvls  <- levels(train[[class]])
  model <- rpart(formula, train, ...)
  pred  <- predict(model, test, type=class) 
  return(table(
    factor(test[[class]], levels=lvls),
    factor(pred, levels=lvls), 
    dnn=list('actual','pred')
  ))
} # End function cv.rpart


# *************************************************
# Neural Network

cv.nnet <- function(formula, train, test, class, ...) {
  lvls  <- levels(train[[class]])
  model <- nnet(formula, train, ...)
  pred  <- predict(model, test, type=class)
  return(table(
    factor(test[[class]], levels=lvls),
    factor(pred, levels=lvls), 
    dnn=list('actual','pred')
  ))
} # End function cv.nnet


# *************************************************
# Support Vector Machine

cv.svm <- function(formula, train, test, class, ...) {
  lvls  <- levels(train[[class]])
  model <- svm(formula, train, ...)
  pred  <- predict(model, test, type=class)
  return(table(
    factor(test[[class]], levels=lvls),
    factor(pred, levels=lvls), 
    dnn=list('actual','pred')
  ))
} # End function cv.svm

# *************************************************
# Naive Bayes

cv.naivebayes <- function(formula, train, test, class, use.weka=F, ...) {
  lvls  <- levels(train[[class]])
  if(use.weka) {
    wekaNaiveBayes <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")
    model <- wekaNaiveBayes(formula, train, ...)
  } else {
    model <- naiveBayes(formula, train, ...)
  }
  pred  <- predict(model, test, type=class)
  return(table(
    factor(test[[class]], levels=lvls),
    factor(pred, levels=lvls), 
    dnn=list('actual','pred')
  ))
} # End function cv.naivebayes

# *************************************************
# K-Nearest Neighbour

cv.knn <- function(formula, train, test, class, k=3, ...) {
  lvls  <- levels(train[[class]])
  pred  <- knn(subset(train, select=-c(class)),
               subset(test, select=-c(class)),
               train[,class], k, ...)
  return(table(
    factor(test[[class]], levels=lvls),
    factor(pred, levels=lvls), 
    dnn=list('actual','pred')
  ))
} # End function cv.knn