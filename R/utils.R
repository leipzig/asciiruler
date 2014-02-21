#This is like seq except it will always include the 'to'
#> seq(1,30,by=10)
#[1]  1 11 21
#> seq_inclusive(1,30,by=10)
#[1]  1 11 21 31
seq_inclusive<-function(from,to,by){
  set<-seq(from,to,by)
  i<-set[length(set)]+by
  while (to>max(set)) {
    set<-c(set,i)
    i<-i+by
  }
  return(set)
}

#Paste with logical defaults
glue<-function(...,sep="",collapse=""){
  strings<-list(...)
  do.call("paste", c(strings, list(sep = sep, collapse = collapse)))
}

repme<-function(this,manyTimes){
  return(glue(rep(this,manyTimes)))
}

spaces<-function(n){
  glue(repme(' ',n))
}

#Break line of text into blocks of size blocksize
blocks<-function(line,blocksize=10L,sep=" "){
  if(blocksize<=0){return(line)}
  blockstarts<-seq(1,stringr:::str_length(line),by=blocksize)
  minLength<-max(blocksize,stringr:::str_length(line))
  blockends<-seq_inclusive(from=blocksize,to=minLength,by=blocksize)
  paste(substring(line,blockstarts,blockends),collapse=sep)
}