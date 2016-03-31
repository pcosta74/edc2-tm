library(tm)

# *************************************************
# Read data from CSV
# return: data.frame with UTF-16 encoding

read.data <- function(filepath, encoding="") {
  data <- read.csv(.DATA.FILEPATH, fileEncoding = encoding, 
                   blank.lines.skip=T, header=T, sep=";",
                   stringsAsFactors=F, strip.white=T)
  #iconv(data, encoding, 'UTF-32')
} # End read.data


# *************************************************
# Main

# Properties
.DATA.ENCODING <- 'ISO-8859-1'
.DATA.FILEPATH <- file.path('.','dset_FPessoa_ALL_v2.csv')
.DTMX.FILEPATH <- file.path('.','docterm_matrix.csv')

# Load data
data <- read.data(.DATA.FILEPATH, .DATA.ENCODING)

# Create corpus
mapping <- list(id = "Autor", content = "Poema")
readTab <- readTabular(mapping)
corpus  <- Corpus(DataframeSource(data), 
                  readerControl = list(reader = readTab, language = "pt"))
classes <- data[,1]

# Garbage collection
rm(data)

# Pre-processing
skipWords <- function(x) removeWords(x, stopwords('portuguese'))
functions <- list(content_transformer(tolower),
                  removePunctuation, removeNumbers, stripWhitespace, skipWords)
corpus    <- tm_map(corpus, FUN = tm_reduce, tmFuns = functions)


# Create document term matrix
options <- list(minWordLength = 2,  minDocFreq=2, stemDocument=T, weighting=weightTfIdf)
dtm.mx  <- DocumentTermMatrix(corpus, control = options)
dtm.mx  <- removeSparseTerms(dtm.mx, 0.99)

dtm.df  <- as.data.frame(as.matrix(dtm.mx))
rownames(dtm.df) <- 1:nrow(dtm.mx)
dtm.df  <- cbind(dtm.df, classes)

write.csv2(dtm.df, file=.DTMX.FILEPATH, fileEncoding=.DATA.ENCODING, row.names=F)

# # Find top 10 frequent terms 
# # freqterms10 <- findFreqTerms(dtm.mx, 10, 10)
