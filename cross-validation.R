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
#             a test set, and a class column index
# data: data frame where each column is an attribute and
#       each row is an observation,
# class.col: column index for the attribute to be predicted

cross.validation <- function(classifier, data, k=10, 
                             class.col=ncol(data), ...) {
  result <- list()
  test.indices <- cv.testing(dim(data)[1])
  for (i in 1:k) {
    test.ndx <- test.indices[[i]]
    train <- data[-test.ndx,]
    test  <- data[test.ndx,]
    result[[i]] <- classifier(train, test, class)
  }
  return(result)
}

classifier.naivebayes = function(train, test, class) {
  # simple example of a classifier
  # requires library(e1071)
  model = naiveBayes(train[,-class], train[,class])
  table(predict(model, test[,-class]), test[,class])
}
