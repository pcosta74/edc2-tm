source(file.path('.','utils.R'))

SETUPS <- enum(c('4x4','12x5'))
VARIABLES <- enum(c('QNTT','PRIC','BETA','HIST','FCON','VCON','PROD','UCST','ASEC'))
INDICATORS <- enum(c('quantities','production','prices','wealth','utility'))

dim.setup<-function(x) {
  s<-as.character(match.enum(x,SETUPS))
  as.integer(unlist(strsplit(s,"x")))
}