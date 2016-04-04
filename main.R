library(parallel)

source(file.path('.','cross-validation.R'))
source(file.path('.','information.R'))
source(file.path('.','utils.R'))
source(file.path('.','text-mining.R'))

# *************************************************
# Configuration

.DATA.ENCODING <- 'ISO-8859-1'
.DATA.LANGUAGE <- 'portuguese'
.DATA.FILEPATH <- file.path('.','data','dset_FPessoa_ALL_v3.csv')
.DTMX.FILEPATH <- file.path('.','data','docterm_matrix.csv')


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
# Build cube of 2x2 matrices per class

as.array.cv.list <- function(x) {
  clas.lbls <- rownames(x[[1]])
  mtxs.lbls <- c('TRUE','FALSE')
  arr.dims  <- c(length(x), length(clas.lbls), 
                length(mtxs.lbls), length(mtxs.lbls))
  dim.nams  <- list(NULL, clas.lbls, mtxs.lbls, mtxs.lbls)
  
  cube <- array(NA, arr.dims, dim.nams)
  for(i in 1:length(x)) {
    for(j in 1:nrow(x[[i]])) {
      cube[i,j,,] <- c(x[[i]][j,j], sum(x[[i]][-j,j]), sum(x[[i]][j,-j]), sum(x[[i]][-j,-j]))
    }
  }
  return(cube)
} # End function as.cube


# *************************************************
# Get statistical measures of prediction

get.measures <- function(a) {
  dims <- dim(a)
  measures <- sapply(seq(dims[2]), function(j) {
    class.meas <- sapply(seq(dims[1]), function(i) {
      tp <- a[i,j,1,1]; fn <- a[i,j,2,1]
      fp <- a[i,j,1,2]; tn <- a[i,j,2,2]
      
      ac <- (tp+tn)/(tp+fp+fn+tn)
      pr <- tp/(tp+fp)
      rc <- tp/(tp+fn)
      f1 <- 2*pr*rc/(pr+rc)
      
      c(accuracy=ifelse(is.na(ac),0,ac), 
        precision=ifelse(is.na(pr),0,pr),
        recall=ifelse(is.na(rc),0,rc),
        f1=ifelse(is.na(f1),0,f1))
    })
    apply(class.meas, 1, mean)
  })
  measures <- cbind(measures,apply(measures,1,mean))
  colnames(measures) <- c(colnames(a),'Macro')
  return(measures)
} # End function get.measures


# *************************************************
# Main

# Load data
cat('Load data','\n')
data <- read.data(.DATA.FILEPATH, .DATA.ENCODING, trace=F)

# Create corpus
cat('Create corpus','\n')
mapping <- list(id = "Autor", content = "Poema")
corpus  <- create.corpus(data, mapping, .DATA.LANGUAGE, trace=F)

# Create document term matrix dataframe
cat('Create document term matrix','\n')
dtm.df <- create.dtm.dataframe(corpus, sparse=0.99, trace=T, minWordLength=2, minDocFreq=2, 
                               stemDocument=T, weighting=weightTfIdf)

# Garbage collection
rm(list=c('data', 'mapping', 'corpus'))

# Write as CVS for DTM dataframe for 3rd party testing
write.csv2(dtm.df, file=.DTMX.FILEPATH, fileEncoding=.DATA.ENCODING, row.names=F)

# Create formula
c.form <- create.dtm.formula(dtm.df)

# List of classifiers to use
.CLASSIFIERS <- list(
  'RPART'   = list(cv.rpart, c.form, dtm.df),
  'NNET'    = list(cv.nnet, c.form, dtm.df, size=2, trace=F, rang=0.1,
                   decay=5e-4, maxit=200, MaxNWts = 2000),
  'SVM'     = list(cv.svm, c.form, dtm.df),
  'RAD.SVM' = list(cv.svm, c.form, dtm.df, kernel='radial', cost=50),
  'NBAYES'  = list(cv.naivebayes, c.form, dtm.df),
  'WEKA.NB' = list(cv.naivebayes, c.form, dtm.df,use.weka=T),
  '1NN'     = list(cv.knn, c.form, dtm.df, k=1),
  'KNN'     = list(cv.knn, c.form, dtm.df, k=3)
)

# For every classifier in list
for(classif in names(.CLASSIFIERS)) {
  # Run classifier
  cat('Classifier:', classif, '\n')
  results <- do.call(cross.validation,.CLASSIFIERS[[classif]])
  
  cat('Collect statistics','\n')
  measures <- get.measures(as.array(results))
  print(measures)
}
