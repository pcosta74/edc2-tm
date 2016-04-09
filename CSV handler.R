df<- read.csv(file = "FPessoa_ALL_v3.csv",sep=",",stringsAsFactors = F)

#/////Retirar poemas em Inglês de Álvaro Campos////
df <- df[-265,]
df <- df[-302,]

#////escreve novo file com separador ";" /////
write.table(df, file = "FPessoa_semicolon.csv", row.names = F, sep = ";", qmethod = "double")

#Juntar poemas de Ricardo Reis

rr<-df[df$Autor=="Ricardo Reis",]
rr.new <- head(rr,0)

i<-1
while (i <= dim(rr)[1]) {
  if (i==dim(rr)[1]) {
    rr_temp <- data.frame(rr[i,1],rr[i,2],rr[i,3],stringsAsFactors = F)
    dimnames(rr_temp)[[2]] <- dimnames(rr.new)[[2]]
    rr.new <- rbind(rr.new,rr_temp)
    break
  } else if (rr[i,2]==rr[i+1,2]) {
    rr_temp <- data.frame(rr[i,1],rr[i,2],paste(rr[i,3],rr[i+1,3],sep = " "),stringsAsFactors = F)
    dimnames(rr_temp)[[2]] <- dimnames(rr.new)[[2]]
    rr.new <- rbind(rr.new,rr_temp)
    i<-i+2
  } else {
    rr_temp <- data.frame(rr[i,1],rr[i,2],rr[i,3],stringsAsFactors = F)
    dimnames(rr_temp)[[2]] <- dimnames(rr.new)[[2]]
    rr.new <- rbind(rr.new,rr_temp)
    i<-i+1
  }
}

#Criar novo ficheiro com poemas de Ricardo Reis unidos

df.no.rr <- df[df$Autor!="Ricardo Reis",]
df.new<- rbind(df.no.rr,rr.new)
write.table(df.new, file = "FPessoa_semicolon_NormRReis.csv", row.names = F, sep = ";", qmethod = "double")


#A criação de dataset com classes equilibradas fois posteriormente feito com o Knime
