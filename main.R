library('rpart')

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

# Create corpus
mapping <- list(id = "Autor", content = "Poema")
corpus  <- create.corpus(data, mapping, .DATA.LANGUAGE)

# Create document term matrix dataframe
dtm.df <- create.dtm.dataframe(corpus, sparse=0.99, minWordLength=2, minDocFreq=2, 
                               stemDocument=T, weighting=weightTfIdf)

# Garbage collection
rm(list=c('data', 'mapping', 'corpus'))

# Write as CVS for DTM dataframe for 3rd party testing
write.csv2(dtm.df, file=.DTMX.FILEPATH, fileEncoding=.DATA.ENCODING, row.names=F)

# create formula
c.form <- create.dtm.formula(dtm.df)

# Decision tree
dec.tree <- rpart(class.f, dtm.df)


