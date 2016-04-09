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
.DATA.MINWRDLN <- 4
.DATA.MINBOUND <- 4
.DATA.SPARCITY <- 0.99
.DATA.TOKENIZE <- match.enum('words',.TOKEN.FUN)
.DATA.WEIGHTIN <- match.enum('tfidf',.WEIGH.FUN)

.DATA.ENCODING <- 'ISO-8859-1'
.DATA.LANGUAGE <- 'portuguese'
.DATA.FILEPATH <- file.path('.','data','FPessoa_NormRR_EqualFreq_08Abr.csv')
.CLDW.FILEPATH <- file.path('.','data',
                            paste(.DATA.TOKENIZE,.DATA.WEIGHTIN,'%AUTHOR%_cloudword.png',sep="-"))
.CRPS.FILEPATH <- file.path('.','data',
                            paste('corpus.csv',sep="-"))
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
# Get statistical measures of prediction

get.measures <- function(x) {
  rx <- Reduce(`+`,x)
  nn  <- length(diag(rx))
  
  ac <- rep(sum(diag(rx))/sum(rx),nn)
  pr <- diag(rx)/apply(rx,1,sum)
  rc <- diag(rx)/apply(rx,2,sum)
  f1 <- (2*pr*rc)/(pr+rc)
  
  measures <- matrix(c(ac,pr,rc,f1),4,nn,byrow=T)
  measures <- cbind(measures,apply(measures,1,mean))
  colnames(measures) <- c(colnames(rx),'Macro')
  rownames(measures) <- c('accuracy','precision','recall','f1')
  
  return(measures)
} # End function get.measures

# *************************************************
# Get statistical measures of prediction

boxplot.word.count <- function(x, t) {
  n <- length(levels(x$class))
  x.ticks <- 1:n
  y.ticks <- pretty(0:max(x$count), n=10)
  
  boxplot(count ~ class, data=x, 
          xaxt='n', yaxt='n', 
          ylim=c(0,max(y.ticks)), cex.axis=0.7)
  axis(1, at=x.ticks, cex.axis=0.7, 
       labels=sub('\\s+','\n',as.character(levels(x$class))))
  axis(2, at=y.ticks, cex.axis=0.7, las=1)
  title(main=t$main, xlab=t$x, ylab=t$y)
} #

# *************************************************
# Main

# Load data
cat('Load data','\n')
data <- read.data(.DATA.FILEPATH, .DATA.ENCODING, trace=F)

layout(matrix(c(1,2), ncol=2), widths=c(1, 1), heights=c(1.5,1.5), respect = TRUE)

# Boxplot word.count
df<-data.frame(class=data$Autor, count=sapply(data$Poema,word.count))
boxplot.word.count(df,list(main='Raw data', x='Author', y='Words'))

# Create corpus
cat('Create corpus','\n')
mapping <- list(id = "Autor", content = "Poema")
corpus  <- create.corpus(data, mapping, .DATA.LANGUAGE, 
                         trace=F, stem=.DATA.STEMMING)

corp.df <- data.frame(class=sapply(corpus, function(c) c$meta$id),
                      corpus=sapply(corpus, function(c) c$content),
                      count=sapply(corpus, function(c) word.count(c$content)),
                      row.names = NULL)
# Write corpus as CVS for 3rd party testing
write.csv2(corp.df, file=.CRPS.FILEPATH, fileEncoding=.DATA.ENCODING, 
           row.names=F)
# Boxplot word.count after cleaning
boxplot.word.count(corp.df,list(main='Corpus data', x='Author', y='Words'))

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
dtm.df <- create.dtm.dataframe(corpus, trace=TRUE, sparse=.DATA.SPARCITY, 
                               stem=.DATA.STEMMING,
                               wordLengths=c(.DATA.MINWRDLN,Inf),
                               bounds = list(global = c(.DATA.MINBOUND,Inf)),
                               stemDocument=TRUE,
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
rm(list=c('data', 'mapping', 'corpus', 'corp.df', 'freq'))

# Write as CVS for DTM dataframe for 3rd party testing
write.csv2(dtm.df, file=.DTMX.FILEPATH, fileEncoding=.DATA.ENCODING, 
           row.names=F)

# List of classifiers to use
.CLASSIFIERS <- list(
  'RPART'   = list(cv.rpart, c.form, dtm.df, seed=7919),
  'FOREST'  = list(cv.rforest, c.form, dtm.df, seed=7919),
  'NNET'    = list(cv.nnet, c.form, dtm.df, seed=7919, 
                   size=2, trace=F, rang=0.1,
                   decay=5e-4, maxit=200, MaxNWts = 2500),
  'SVM'     = list(cv.svm, c.form, dtm.df, seed=7919, kernel='polynomial', degree=1, gamma=1),
  'RAD.SVM' = list(cv.svm, c.form, dtm.df, seed=7919, kernel='radial', cost=50),
  'NBAYES'  = list(cv.naivebayes, c.form, dtm.df, seed=7919),
  'WEKA.NB' = list(cv.naivebayes, c.form, dtm.df, seed=7919, use.weka=T),
  '1NN'     = list(cv.knn, c.form, dtm.df, seed=7919, k=1),
  'KNN'     = list(cv.knn, c.form, dtm.df, seed=7919, k=3)
)

report <- data.frame()
# For every classifier in list
for(classif in names(.CLASSIFIERS)) {
  # Run classifier
  cat('Classifier:', classif, '\n')
  results <- do.call(cross.validation,.CLASSIFIERS[[classif]])
  
  # Get statistics
  cat('Collect statistics','\n')
  measures <- get.measures(results)
  
  tmp.df <- data.frame(classifier=rep(classif,nrow(measures)),
                       measures=rownames(measures),
                       measures, row.names = NULL)
  report <- rbind(report,tmp.df)
}

# Write as CVS for DTM dataframe for 3rd party testing
write.csv2(report, file=.RPRT.FILEPATH, fileEncoding=.DATA.ENCODING, 
           row.names=F)
