library(tm)

# *************************************************
# Read data from CSV
# return: data.frame with UTF-16 encoding

read.data <- function(filepath, encoding="") {
  data <- read.csv(.DATA.FILEPATH, fileEncoding = encoding, 
                   blank.lines.skip=T, header=T, 
                   stringsAsFactors=F, strip.white=T)
  #iconv(data, encoding, 'UTF-32')
} # End read.data


# *************************************************
# Main


# Load data
.DATA.FILEPATH <- file.path('.','dset_FPessoa_ALL_v2.csv')
FernandoPessoa <- read.data(.DATA.FILEPATH, 'ISO-8859-1')

# Create corpus
mapping <- list(id = "Autor", content = "Poema")
readTab <- readTabular(mapping)
corpus  <- Corpus(DataframeSource(FernandoPessoa), 
                  readerControl = list(reader = readTab, language = "pt"))

# Pre-processing
skipWords <- function(x) removeWords(x, stopwords('portuguese'))
rmvAccent <- function(x) chartr('àáâãéêíóõôúç','aaaaeeiooouc',x)
functions <- list(content_transformer(tolower), #content_transformer(rmvAccent),
                  removePunctuation, removeNumbers, stripWhitespace, skipWords)
corpus.p  <- tm_map(corpus, FUN = tm_reduce, tmFuns = functions)


# Create document term matrix
options <- list(minWordLength = 2,  minDocFreq=2, stemDocument=T, weighting=weightTfIdf)
dtm.mx  <- DocumentTermMatrix(corpus.p, control = options)
dtm.mx  <- removeSparseTerms(dtm.mx, 0.99)
# inspect(dtm.mx)

# Find top 10 frequent terms 
freqterms10 <- findFreqTerms(dtm.mx, 10, 10)
