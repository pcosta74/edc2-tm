library(wordcloud)

source(file.path('.','cross-validation.R'))
source(file.path('.','information.R'))
source(file.path('.','utils.R'))
source(file.path('.','text-mining.R'))

# *************************************************
# Enumerations
.WEIGH.FUN <- enum('tf', 'tfidf', 'bin', 'smart') 
.TOKEN.FUN <- enum('words', 'ngram', 'sents')

# *************************************************
# Configuration

.DATA.STEMMING <- FALSE
.DATA.TOKENIZE <- match.enum('words',.TOKEN.FUN)
.DATA.WEIGHTIN <- match.enum('tfidf',.WEIGH.FUN)

.DATA.ENCODING <- 'ISO-8859-1'
.DATA.LANGUAGE <- 'portuguese'
.DATA.FILEPATH <- file.path('.','data','dset_FPessoa_ALL_v3.csv')
.CLDW.FILEPATH <- file.path('.','data',
                            paste(.DATA.TOKENIZE,.DATA.WEIGHTIN,'%AUTHOR%_cloudword.png',sep="-"))
.DTMX.FILEPATH <- file.path('.','data',
                            paste(.DATA.TOKENIZE,.DATA.WEIGHTIN,'docterm_matrix.csv',sep="-"))
.FREQ.FILEPATH <- file.path('.','data',
                            paste(.DATA.TOKENIZE,.DATA.WEIGHTIN,'term_freq.csv',sep="-"))
.RPRT.FILEPATH <- file.path('.','data',
                            paste(.DATA.TOKENIZE,.DATA.WEIGHTIN,'measures.csv',sep="-"))


# *************************************************
# N-gram tokenizer
ngram.tokenizer <- function(x,n=2) {
  require(RWeka)
  NGramTokenizer(x, Weka_control(min = n, max = n))
} # End function ngram.tokenizer


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
corpus  <- create.corpus(data, mapping, .DATA.LANGUAGE, 
                         trace=F, stem=.DATA.STEMMING)

# Decide on the weighting method
weight.FUN <- switch(
  as.character(.DATA.WEIGHTIN),
  'tf'=weightTf,
  'tfidf'=weightTfIdf,
  'bin'=weightBin,
  'smart'=weightSMART,
  stop('Invalid weighting function')
)

# Decide on the weighting method
token.FUN <- switch(
  as.character(.DATA.TOKENIZE),
  'words'=words,
  'ngram'=ngram.tokenizer,
  'sents'=stems,
  stop('Invalid tokenize function')
)

# Create document term matrix dataframe
cat('Create document term matrix','\n')
dtm.df <- create.dtm.dataframe(corpus, trace=TRUE, sparse=0.99, 
                               stem=.DATA.STEMMING, minWordLength=2,
                               minDocFreq=2, stemDocument=TRUE,
                               weighting=weight.FUN,
                               tokenize=token.FUN)

# Create formula
c.form <- create.dtm.formula(dtm.df)

# Determine word frequences
freq <- aggregate(. ~ class, data = dtm.df, sum)
freq <- as.data.frame(t(freq[,-1]))
rownames(freq) <- gsub('\\.','',rownames(freq))
colnames(freq) <- levels(dtm.df$class)

# Write as CVS the frequency table for 3rd party testing
write.csv2(dtm.df, file=.FREQ.FILEPATH, fileEncoding=.DATA.ENCODING, 
           row.names=F)

layout(matrix(c(1, 2), nrow=2), heights=c(.5, 10))
par(oma=c(0,0,4,0), mai=rep(0,4))
for(i in 1:ncol(freq)) {
  # Sort frequences by class
  clas.freq <- matrix(freq[,i],dimnames=list(rownames(freq),NULL))
  
  clas.freq <- sort(rowSums(clas.freq), decreasing=TRUE)
  clas.freq <- data.frame(word=names(clas.freq),
                          freq=clas.freq, row.names = NULL)
  cat('50 most frequent words for', colnames(freq)[i],'\n')
  print(head(clas.freq, 50))

  # Draw wordcloud
  filename <- gsub('%AUTHOR%',
                   abbreviate(toupper(colnames(freq)[i]),6),
                   .CLDW.FILEPATH)
  tryCatch({
    png(filename, antialias='subpixel',width=640, height=640, res=100)
    
    plot.new()
    title(colnames(freq)[i], line=-1)
    wordcloud(words = clas.freq$word, freq = clas.freq$freq, min.freq = 1,
              max.words=100, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
    dev.off()
  },
  error=function(err){
  },
  warning=function(wrn) {
  })
}

# Garbage collection
rm(list=c('data', 'mapping', 'corpus', 'freq'))

# Write as CVS for DTM dataframe for 3rd party testing
write.csv2(dtm.df, file=.DTMX.FILEPATH, fileEncoding=.DATA.ENCODING, 
           row.names=F)

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

report <- data.frame()
# For every classifier in list
for(classif in names(.CLASSIFIERS)) {
  # Run classifier
  cat('Classifier:', classif, '\n')
  results <- do.call(cross.validation,.CLASSIFIERS[[classif]])
  
  # Get statistics
  cat('Collect statistics','\n')
  measures <- get.measures(as.array(results))
  
  tmp.df <- data.frame(classifier=rep(classif,nrow(measures)),
                       measures=rownames(measures),
                       measures, row.names = NULL)
  report <- rbind(report,tmp.df)
}

# Write as CVS for DTM dataframe for 3rd party testing
write.csv2(report, file=.RPRT.FILEPATH, fileEncoding=.DATA.ENCODING, 
           row.names=F)
