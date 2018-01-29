#' Generate an ascii ruler
#' 
#''   \preformatted{
#''   v borders
#''   +-----------------------------------------------------------------+<-borders
#''   |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||<-dense_ticks
#''   |  |    |    |    |    |    |    |    |    |    |    |    |    |  |<-sparse_ticks
#''   |-30       -20       -10         0         10        20        30 |
#''   +-----------------------------------------------------------------+<-borders
#''   }
#' @param low the range start, can be negative
#' @param high the range end, can be negative
#' @param sparse_ticks intermittent ticks appear every sparse_ticks, set to 0 to hide row
#' @param dense_ticks display a row of ticks at every position
#' @param block_space break up the ruler with a space every block_space blocks encountered, a multiple of sparse_ticks, set to 0 to disable
#' @param borders display borders
#' @param numbers_down display the ruler so the numbers are below the ticks
#' @param line_break the line break character(s)
#' @param strict_width hide numbers whose display would force the ruler to be wider than width(high-low)
#' @return asciiruler object with the following slots:
#' \describe{
#'  \item{output}{delimited ruler string ready to cat}
#'  \item{content}{vector of lines comprising the ruler}
#'  \item{width}{width of ruler}
#'  \item{leftmargin}{position of the first tick relative to the left edge of the ruler}
#' }
#' @examples
#' asciiruler(low=-30,high=30,borders=TRUE)
#' @references Inspired by \url{http://codegolf.stackexchange.com/questions/4910/ascii-ruler-generation}
#' @import stringr
#' @export
asciiruler<-function(low=0L,high=50L,sparse_ticks=5L,dense_ticks=TRUE,block_space=0L,borders=FALSE,numbers_down=TRUE,line_break="\n",strict_width=FALSE){
  if(borders){e<-'|'}else{e<-''}
  b<-'|'
  p<-"+"
  d<-'-'
  
  pad<-function(i){
    #returns special right flank padding for ends and kingpin
    right_flank<-function(x){
      #last number should be right flanked to get to high but no further
      if(x<high & x+sparse_ticks>high){return(spaces(abs(high-x-stringr::str_length(as.character(x))+1)))}
      #the highest non-positive position
      #should be flanked by spaces on both sides
      if(x<=0 & x+sparse_ticks>0){return(spaces(sparse_ticks-1))}
      return('')
    }
    #adjust for possible block spacing
    blockpad<-function(i){
      #first number - don't pad
      if(i==low | block_space==0){
        return('')
      }
      if(((i-low) %% (sparse_ticks*block_space)) == 0){
        return(' ')
      }
      return('')
    }
    #> sprintf('%10s',-10)
    #[1] "       -10"
    #> sprintf('%-10s',10)
    #[1] "10        "
    #negative numbers have their last digit aligned with the tick
    left_or_right<-function(i){
      #last digit has special rules
      if(i+sparse_ticks>high){
        return('%s')
      }else{
        glue('%',d[i>0],sparse_ticks,'s')
      }
    }
    
    strictify<-function(i){
      if((i==low | i==high) & stringr::str_length(as.character(i))>1 & strict_width==TRUE){return('_')}
      return(i)
    }
    glue(blockpad(i),sprintf(left_or_right(i),strictify(i)),right_flank(i))
  }  
  numbers<-stringr::str_replace_all(glue(stringr::str_trim(glue(sapply(seq(low,high,by=sparse_ticks),pad)),side="left")),'_',' ')
  if(substr(numbers,1,1)==d){
    left_margin<-unname(stringr::str_locate(numbers,' ')[1,"start"])-2
  }else{
    left_margin<-0
  }

  n<-stringr::str_length(numbers)
  
  tb_border<-glue(p,glue(repme(d,n)),p)
  sparse_tick_marks<-glue(e,spaces(left_margin),substr(blocks(glue(repme(glue(b,spaces(sparse_ticks-1)),n)),blocksize=block_space*sparse_ticks),1,n-left_margin),e)
  
  #prefer to see dense ticks even before the 0 in -30 unless dealing with blocks
  edge_ticks<-function(){if(block_space>0){return(' ')}else{return(b)}}
  
  dense_tick_marks<-glue(e,repme(edge_ticks(),left_margin),substr(blocks(repme(b,n),blocksize=block_space*sparse_ticks),1,n-left_margin),e)
  width<-stringr::str_length(dense_tick_marks)
  numbers<-glue(e,numbers,e)
  sparse_bool<-!(is.null(sparse_ticks) | sparse_ticks==0)
  log_vector<-c(borders,dense_ticks,sparse_bool,TRUE,borders)
  content<-c(tb_border,dense_tick_marks,sparse_tick_marks,numbers,tb_border)[log_vector]
  output<-''
  if(!numbers_down){
    content<-rev(content)
  }
  structure(list(output=paste(content,collapse=line_break),content=content,left_margin=left_margin,width=width), class = "asciiruler")
}

#' @export
#' @param x the asciiruler
#' @rdname asciiruler
default.asciiruler<-function(x){return(x$output)}

#' Print an asciiruler
#' @export
#' @param x the asciiruler
#' @param ... additional arguments
print.asciiruler<-function(x,...){cat(x$output,...)}

#' @export
as.character.asciiruler<-function(x,...){as.character(x$output)}


cat <- function(..., file = "", sep = " ", fill = FALSE, labels = NULL, append = FALSE) {
  #http://stackoverflow.com/questions/21890835/how-do-i-dispatch-cat-in-r-s3
  UseMethod("cat")
}

#' @export
cat.character <- function(..., file = "", sep = " ", fill = FALSE, labels = NULL, append = FALSE) {
  base::cat(..., file = file, sep = sep, fill = fill, labels = labels, 
            append = append)
}

#' @export
cat.default <- function(..., file = "", sep = " ", fill = FALSE, labels = NULL, append = FALSE) {
  base::cat(..., file = file, sep = sep, fill = fill, labels = labels, 
            append = append)
}

#' @export
cat.asciiruler <- function(..., file = "", sep = " ", fill = FALSE, labels = NULL,
                    append = FALSE) {
  dots <- list(...)
  sapply(dots,function(x){base::cat(x[['output']],"\n")})
}

#' Get the total width of an ascii ruler
#' @export
#' @param x the asciiruler
#' @param ... additional arguments
width<-function(x,...) UseMethod("width")

#' @export
width.asciiruler<-function(x,...){x$width}


#' Generate a GenBank sequence block
#' 
#''   \preformatted{
#''     1    6     11   16    21   26    31   36    41   46    51   56   
#''     |    |     |    |     |    |     |    |     |    |     |    |    
#''     |||||||||| |||||||||| |||||||||| |||||||||| |||||||||| ||||||||||
#''   1 GATCACAGGT CTATCACCCT ATTAACCACT CACGGGAGCT CTCCATGCAT TTGGTATTTT
#''  61 CGTCTGGGGG GTGTGCACGC GATAGCATTG CGAGACGCTG GAGCCGGAGC ACCCTATGTC
#'' 121 GCAGTATCTG TCTTTGATTC CTGCCCCATC CTATTATTTA TCGCACCTAC GTTCAATATT
#'' 181 ACAGGCGAAC ATACTTACTA AAGTGTGTTA ATTAATTAAT GCTTGTAGGA CATAATAATA
#'' 241 ACAATTGAAT GTCTGCACAG CCGCTTTCCA CACAGACATC ATAACAAAAA ATTTCCACCA
#'' 301 AACCCCCCCT CCCCCGCTTC TGGCCACAGC ACTTAAACAC ATCTCTGC
#'' }
#' @param string the sequence string to display
#' @param start the substring start, should be 1 or greater
#' @param end substring end, should be >start, negative to trim
#' @param blocksize number of characters in each block
#' @param width width of the sequence block
#' @param sep space character between blocks
#' @param line_break the line break character(s)
#' @param ruler display an ascii ruler
#' @param ... additional arguments passed to asciiruler
#' @return delimited GenBank block string ready to cat
#' @examples
#' my_sequence<-'GATCACAGGTCTATCACCCTATTAACCACTCACGGGAGCTCTCCATGCATTTGGTATTTTCGTCTGGGGG
#' GTATGCACGCGATAGCATTGCGAGACGCTGGAGCCGGAGCACCCTATGTCGCAGTATCTGTCTTTGATTC
#' CTGCCTCATCCTATTATTTATCGCACCTACGTTCAATATTACAGGCGAACATACTTACTAAAGTGTGTTA
#' ATTAATTAATGCTTGTAGGACATAATAATAACAATTGAATGTCTGCACAGCCACTTTCCACACAGACATC'
#' my_seqblock<-genbank_seqblock(my_sequence)
#' cat(my_seqblock)
#' @export
genbank_seqblock<-function(string,start=1L,end=0L,blocksize=10L,width=60L,sep=" ",line_break="\n",ruler=TRUE,...){
  if (nchar(string) == 0L || length(string) == 0L || length(start) == 0L || length(end) == 0L) {
    return(vector("character", 0L))
  }
  
  # Convert negative values into actual positions
  len <- stringr::str_length(string)
  
  if(is.na(start) || start < 1L){start <- 1L}
  
  #allow negative values to trim, 0 is full length
  if(is.na(end) || end < 1L){end<-end+len}
  if(end>len){end<-len}
  if(is.na(width) || width < 1L){width=60L}
  #width<-min(width,end-start)
  
  #remove whitespace
  string<-stringr::str_replace_all(string,'\\s','')
  
  subseq<-substring(string, start, end)
  
  #if a seq ends at 1000-1060 and width is 60 it must be at least 4 digits wide
  offset_char_width<-stringr::str_length(as.character(end-width))
  linestarts<-seq(start,end,by=width)
  linenumbers<-sprintf(glue('%',offset_char_width,'s'),linestarts)
  lineends<-seq_inclusive(from=start+width-1,to=end,by=width)
  lines<-substring(subseq,linestarts,lineends)

  textblocks<-sapply(lines,blocks,sep=sep,blocksize=blocksize)
  bound<-rbind(num=linenumbers,text=textblocks)
  ascii<-''
  if(ruler){
    ascii<-asciiruler(1,high=width,sparse_ticks=5,block_space=2,numbers_down=FALSE,...)
    asciishift<-paste(repme(' ',offset_char_width),ascii$content,collapse=line_break)
    return(paste(asciishift,paste(as.list(bound[1,]),as.list(bound[2,]),sep=" ",collapse=line_break),sep=line_break))
  }
  return(paste(as.list(bound[1,]),as.list(bound[2,]),sep=" ",collapse=line_break))
}

#This is like seq except it will always include the 'to'
#> seq(1,30,by=10)
#[1]  1 11 21
#> seq_inclusive(1,30,by=10)
#[1]  1 11 21 31
seq_inclusive<-function(from,to,by){
  if(from>to){return(to)}
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
    blockstarts<-seq(1,stringr::str_length(line),by=blocksize)
    minLength<-max(blocksize,stringr::str_length(line))
    blockends<-seq_inclusive(from=blocksize,to=minLength,by=blocksize)
    paste(substring(line,blockstarts,blockends),collapse=sep)
}