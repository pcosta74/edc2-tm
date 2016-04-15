library('tm')
library('SnowballC')


# *************************************************
# Create corpus and apply pre-processing functions

create.corpus <- function(dataframe, mapping, language="en", 
                          pre.process=T, trace=F, stem=F) {
  # Create corpus
  readTab <- readTabular(mapping)
  corpus  <- Corpus(DataframeSource(dataframe), 
                    readerControl = list(reader = readTab, 
                                         language = language))
  
  if(pre.process) {
    # Pre-processing
    stemDocs  <- function(x) stemDocument(x, language=meta(x,language))
    functions <- list(content_transformer(tolower), stripWhitespace,
                      removePunctuation, removeNumbers)
    if(stem) {
      functions <- append(functions, stemDocs)
    }
  
    corpus <- tm_map(corpus, FUN=tm_reduce, tmFuns = functions)
    corpus <- tm_map(corpus, removeWords, stopwords(kind=language))
  }
  
  if(trace) {
    hst <- hist(sapply(corpus, function(c) sapply(strsplit(c$content, "\\s+"), length)), plot=F)
    dst <- paste(hst$breaks[-length(hst$breaks)], hst$breaks[-1], sep="-")
    nms <- unique(names(corpus))
    message('No. documents: ',length(corpus),'\n',
            'Distinct ids: ',length(nchar(nms)),'\n',
            paste(' ',nms, collapse="\n"),'\n',
            'Word count distribution:','\n',
            paste(format(dst, width=max(nchar(dst))+1, justify="right"),
                  format(hst$count, width=max(nchar(hst$count))+1, justify="right"),
                  sep=':', collapse="\n"))
  }
  
  # return corpus
  return(corpus)
} # End function create.corpus


# *************************************************
# Create Document Term Matrix as a dataframe

create.dtm.dataframe <- function(corpus, sparse=0.95, min.info=0.01, stem=F, trace=F, ...) {
  # Create document term matrix
  options <- list(...)
  dtm.mx  <- DocumentTermMatrix(corpus, control = options)
  
  # Remove sparse terms
  if(!missing(sparse))
    dtm.mx  <- removeSparseTerms(dtm.mx, sparse)
  
  
  # Convert DTM to dataframe
  dtm.df  <- as.data.frame(as.matrix(dtm.mx))
  rownames(dtm.df) <- 1:nrow(dtm.mx)
  
  # Append "." to the end of terms to prevent errors in formula construction
  if(stem)
    colnames(dtm.df) <- paste(colnames(dtm.df),'',sep='.')

  
  # Prune non-informtive terms
  if(!missing(min.info))
    dtm.df <- dtm.df[,findInformativeTerms(dtm.df, min.info)]
  
  # Append class column
  class <- sapply(corpus, function(x) x$meta$id)
  dtm.df  <- cbind(dtm.df, class)
  
  if(trace) {
  }
  
  return(dtm.df)
} # End function create.dtm.dataframe


# *************************************************
# Create formula from DTM dataframe column names

create.dtm.formula <- function(data, class.col=ncol(data), simplify=F) {
  last.col <- ncol(data)
  if(class.col > last.col)
    stop('Invalid ',sQuote('class.col'), ' value: expected value < ',last.col)
  
  terms  <- colnames(data)
  class  <- terms[class.col]
  if(simplify) {
    # short version
    terms <- '.'
  } else {
    # explicit version
    terms  <- terms[-class.col]
    terms  <- paste(terms,collapse='+')
  }
  c.form <- as.formula( paste(class, terms, sep='~') )
  return(c.form)
} # End function create.dtm.formula 

