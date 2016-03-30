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

# Pre-process
skipWords <- function(x) removeWords(x, stopwords("portuguese"))
functions <- list(tolower, removePunctuation, removeNumbers, stripWhitespace, skipWords)
corpus.p  <- tm_map(corpus, FUN = tm_reduce, tmFuns = functions)


# Create document term matrix
mydtm <- DocumentTermMatrix(corpus.p, control = list(wordLengths = c(3,10)))
inspect(mydtm)
