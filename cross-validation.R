library(class)
library(rpart)
library(nnet)
library(e1071)
library(RWeka)
library(randomForest)
library(sampling)

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

cv.testing <- function(values, strats, seed, k=10) {
  indices <- list()
  sizes   <- cv.sizes(length(values), k)

  if(!missing(seed)) {
    set.seed(seed)
  }
  
  for (i in 1:k) {
    if(missing(strats)) {
      # take a sample of given size
      s <- sort(sample(values, sizes[i]))
    } else {
      # verify strats length
      stopifnot(length(values)==length(strats))
      
      # take a stratified sample of given size
      s <- .stratified.sample(values, strats, sizes[i])
      # remove sample from strats
      strats <- strats[-which(values %in% s)]
    }
    # append sample to list of indices
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

cross.validation <- function(CLASS.FUN, formula, data, seed=NULL, folds=10, 
                             simplify=T, stratify=T, ...) {
  class  <- all.vars(update(formula, .~0))
  values <- 1:as.integer(dim(data)[1])
  
  if(stratify) {
    test.indices <- cv.testing(values, strats=data[,class], seed=seed)
  } else { 
    test.indices <- cv.testing(values, seed=seed)
  }
  
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
# Random Forest

cv.rforest <- function(formula, train, test, class, ...) {
  lvls  <- levels(train[[class]])
  model <- randomForest(formula, train, ...)
  pred  <- predict(model, test, type=class)
  return(table(
    factor(test[[class]], levels=lvls),
    factor(pred, levels=lvls), 
    dnn=list('actual','pred')
  ))
} # End function cv.rforest

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



.stratified.sample <- function(x, strats, size, prob=NULL) {
  # return if population equals sample size
  if(size == length(x))
    return(x)
  
  # build sample
  x <- as.data.frame(x)
  x <- cbind(x, strats)
  c <- ncol(x)
  l <- length(unique(strats))

  adjust <- sample(1:l, size%%l)
  v.size <- rep(size%/%l, l)
  v.size[adjust] <- v.size[adjust]+1
  
  # return stratified sample
  strats <- strata(x, stratanames=names(x[c]),
            size=v.size, method="srswor", pik = prob)
  return(x[rownames(strats),-c])
}