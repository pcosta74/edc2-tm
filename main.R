source(file.path('.','cross-validation.R'))
source(file.path('.','information.R'))
source(file.path('.','utils.R'))
source(file.path('.','text-mining.R'))

# *************************************************
# Properties

.DATA.ENCODING <- 'ISO-8859-1'
.DATA.LANGUAGE <- 'portuguese'
.DATA.FILEPATH <- file.path('.','data','dset_FPessoa_ALL_v3.csv')
.DTMX.FILEPATH <- file.path('.','data','docterm_matrix.csv')

.CLASSIFIERS <- enum('DTREE', 'NNET', 'SVM', 'SVM2', 'NBAYES', 'WEKA.NB', '1NN', 'KNN') 

# *************************************************
# Split dataset into training and test datasets

split.dataset <- function(data, n, class.col=ncol(data), random=F) {
  # Get data number of rows
  last.row <- nrow(data)
  
  # Test n parameter
  if(n>=last.row)
    stop('Invalid ',sQuote('n'), ' value: expected value < ',last.row)
  
  # Randomize lines
  if(random)
    data <- data[sample(1:last.row),]

  # Generate the training data with the appropriate lines of data
  train <- data[1:n, ]
  # Generate the test data with the appropriate lines of data
  test  <- data[n+1:last.row, -class.col]
  
  return(list(train=train, test=test))
} # End function split.dataset


# *************************************************
# Main

# Load data
data  <- read.data(.DATA.FILEPATH, .DATA.ENCODING)
cat('Load data','\n')

# Create corpus
mapping <- list(id = "Autor", content = "Poema")
corpus  <- create.corpus(data, mapping, .DATA.LANGUAGE)
cat('Create corpus','\n')

# Create document term matrix dataframe
dtm.df <- create.dtm.dataframe(corpus, sparse=0.99, minWordLength=2, minDocFreq=2, 
                               stemDocument=T, weighting=weightTfIdf)
cat('Create document term matrix','\n')

# Garbage collection
rm(list=c('data', 'mapping', 'corpus'))

# Write as CVS for DTM dataframe for 3rd party testing
write.csv2(dtm.df, file=.DTMX.FILEPATH, fileEncoding=.DATA.ENCODING, row.names=F)

# Create formula
c.form <- create.dtm.formula(dtm.df)

# Decision Tree
if('DTREE' %in% .CLASSIFIERS) {
  results <- cross.validation(cv.rpart, c.form, dtm.df, 10)
  cat('Decision Tree','\n')
  print(results)
}

# Neural Network
if('NNET' %in% .CLASSIFIERS) {
  results <- cross.validation(cv.nnet, c.form, dtm.df, 10, size=2, trace=F,
                              rang=0.1, decay=5e-4, maxit=200, MaxNWts = 2000)
  cat('Neural Network','\n')
  print(results)
}

# Support Vector Machine
if('SVM' %in% .CLASSIFIERS) {
  results <- cross.validation(cv.svm, c.form, dtm.df, 10)
  cat('Support Vector Machine','\n')
  print(results)
}  

# Tunned Support Vector Machine
if('SVM2' %in% .CLASSIFIERS) {
  results <- cross.validation(cv.svm, c.form, dtm.df, 10, kernel='radial', cost=50)
  cat('Tunned Support Vector Machine','\n')
  print(results)
}  

# Naive Bayes (e1071)
if('NBAYES' %in% .CLASSIFIERS) {
  results <- cross.validation(cv.naivebayes, c.form, dtm.df, 10)
  cat('Naive Bayes (e1071)','\n')
  print(results)
} 

# Naive Bayes (WEKA)
if('WEKA.NB' %in% .CLASSIFIERS) {
  results <- cross.validation(cv.naivebayes, c.form, dtm.df, 10, use.weka=T)
  cat('Naive Bayes (WEKA)','\n')
  print(results)
}

# 1-Nearest Neighbour
if('1NN' %in% .CLASSIFIERS) {
  results <- cross.validation(cv.knn, c.form, dtm.df, folds=10, k=1)
  cat('1-Nearest Neighbour','\n')
  print(results)
}

# K-Nearest Neighbours
if('KNN' %in% .CLASSIFIERS) {
  results <- cross.validation(cv.knn, c.form, dtm.df, folds=10, k=3)
  cat('K-Nearest Neighbours','\n')
  print(results)
}