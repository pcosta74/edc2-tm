# *************************************************
# Calculates information gain relative to a list 
# specifying a distribution

info <- function(x) {
  inf.x <- 0
  sum.x <- sum(x)
  for (i in x) {
    prob.i <- i/sum.x
    inf.i <- (prob.i)*log2(prob.i)
    if (is.na(inf.i)) inf.i <- 0 
    inf.x <- inf.x - inf.i 
  } 
  return(inf.x)
} # End info function


# *************************************************
# Find informative terms

findInformativeTerms <- function(dtm.df, min.info) {
  ix.class <- ncol(dtm.df)
  default.info <- info(table(dtm.df[,ix.class])) 
  cat("default.info: ", default.info, "\n")
  
  n.atr <- ncol(dtm.df)-1 
  n.info.terms  <- 0 
  info.term.ixs <- vector() 
  col.names <- names(dtm.df)
  
  #Identifying Informative Terms (Columns)
  for(atr.i in 1:n.atr) {
    
    # Process all attributes
    if(sum(dtm.df[,atr.i])>0) { # begin if
      no.dif.atr.val <- length(table(dtm.df[,atr.i])) 
      atr.class.table <- table(dtm.df[,atr.i],dtm.df[,ix.class]) 
      n.rows <- nrow(dtm.df)
      atr.info <- 0
      
      for (atr.val in 1: no.dif.atr.val) { # begin for
        atr.peso <- sum( atr.class.table[atr.val,]) / n.rows 
        atr.info1 <- atr.peso * info(atr.class.table[atr.val,]) 
        atr.info <- atr.info + atr.info1
      }
      
      info.gain <- default.info - atr.info
      if (info.gain > min.info) {
        info.term.ixs[n.info.terms] <- atr.i
        n.info.terms <- n.info.terms+1 
      }
    } #end for
  } # end if
  
  # Identifying Informative Terms (Columns)
  cat("Kept ", n.info.terms, " atributtes: ", "\n") 
  cat(col.names[info.term.ixs[1:10]], " etc. ")
  cat(col.names[info.term.ixs[n.info.terms-1]],"\n")
  cat("Discarded ", n.atr-n.info.terms, " atributtes", "\n")
  return(col.names[info.term.ixs])
} # end function
