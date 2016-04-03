# *************************************************
# Guess file encoding

guess.encoding <- function(filename, nrows=10, show.prog. = T) {
  code.pages <- iconvlist()
  
  if(show.prog.)
    prog.bar <- txtProgressBar(0, length(code.pages), 0, '|', 20, style=3)

  x <- lapply(seq_along(code.pages), function(i) {
    if(show.prog.) setTxtProgressBar(prog.bar,i)  
    tryCatch({
      read.csv(.DATA.FILEPATH, fileEncoding=code.pages[i], nrows=nrows, header=T)
    }, warning = function(wrn) {
    }, error = function(err) {
    })
  })
  
  if(show.prog.) cat('\n')
  
  maybe.ok <- sapply(x, function(x) isTRUE(all.equal(dim(x)[1], nrows)))
  code.pages[maybe.ok]
} # End function guess.encoding


# *************************************************
# Read data from CSV
# return: data.frame with UTF-16 encoding

read.data <- function(filepath, encoding='UTF-8', trace=F) {
  data <- read.csv(filepath, fileEncoding = encoding, 
                   blank.lines.skip=T, header=T, sep=";",
                   stringsAsFactors=F, strip.white=T)
  
  if(encoding != 'ISO-8859-1')
    data <- iconv(data, encoding, 'ISO-8859-1')

  if(trace) {
    message(sQuote(basename(filepath)), ': ', format(object.size(data)), ', ', 
            paste(dim(data), c('rows','cols'), collapse=' x '))
  }

  return(data)
} # End read.data


# *************************************************
# Make string "Camel Case"

camel.case<-function(x) {
  gsub('(\\w)(\\w*)', '\\U\\1\\L\\2', as.character(x), perl=TRUE)
} # End camel.case

# *************************************************
# Create enumeration

enum <- function(...) {
  c <- match.call()
  x <- unlist(list(...))
  tryCatch({
    e <- ordered(1:length(as.vector(x)), labels=as.vector(x))
    class(e)<-c(class(e),'enum')
    return(e)
  },
  error = function(e) {
    e$call<-c
    e$message<-sub('labels','x',e$message)
    stop(e)
  })  
} # End enum

# *************************************************
# Convert to enumeration

as.enum <- function(x) {
  enum(x)
} # End as.enum

# *************************************************
# Check if x is of class enum

is.enum <- function(x) {
  'enum' %in% class(x)
} # End is.enum


# *************************************************
# Match extension method for class enum

match.enum <- function(x , enum) {
  c<-match.call()
  tryCatch({
    if(!is.null(x)) x<- as.character(x)
    match.arg(x,enum)
  },
  error = function(e) {
    e$call<-c
    e$message<-sub('arg','x',e$message)
    stop(e)
  })
} # End match.enum
