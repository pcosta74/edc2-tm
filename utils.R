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