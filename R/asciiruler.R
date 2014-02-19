#' Generate an ascii ruler
#' 
#''   \preformatted{v borders
#''   +-----------------------------------------------------------------+<-borders
#''   |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||<-dense_ticks
#''   |  |    |    |    |    |    |    |    |    |    |    |    |    |  |<-sparse_ticks
#''   |-30       -20       -10         0         10        20        30 |
#''   +-----------------------------------------------------------------+<-borders
#''   }
#'   @param low range start
#'   @param high range end
#'   @param sparse_ticks intermittent ticks appear every sparse_ticks, set to 0 to hide row
#'   @param dense_ticks display a row of ticks at every position
#'   @param block_ticks break up the ruler with a space every block_ticks encountered, a multiple of sparse_ticks, set to 0 to disable
#'   @param display borders
#'   @param numbers_down display the ruler so the numbers are below the ticks
#'   @param line_break the line break character(s)
#'   @return asciiruler object
#'   @examples
#'   asciiruler(low=-30,high=30,borders=TRUE)
#'   @references Inspired by \code{\url{http://codegolf.stackexchange.com/questions/4910/ascii-ruler-generation}}
asciiruler<-function(low=0L,high=50L,sparse_ticks=5L,dense_ticks=TRUE,block_ticks=0L,borders=FALSE,numbers_down=TRUE,line_break="\n"){
  if(borders){e<-'|'}else{e<-''}
  b<-'|'
  p<-"+"
  d<-'-'
  w<-' '
  

  repme<-function(this,manyTimes){
    return(glue(rep(this,manyTimes)))
  }

  pad<-function(i){
    #returns special right flank padding for ends and kingpin
    right_flank<-function(x){
      #last number should be right flanked to get to high but no further
      if(x<high & x+sparse_ticks>high){return(glue(repme(w,abs(high-x))))}
      #the highest non-positive position
      #should be flanked by spaces on both sides
      if(x<=0 & x+sparse_ticks>0){return(glue(repme(w,sparse_ticks-1)))}
      return('')
    }
    #adjust for possible block spacing
    blockpad<-function(i){
      #first number - don't pad
      if(i==low | block_ticks==0){
        return('%s')
      }
      if(((i-low) %% (sparse_ticks*block_ticks)) == 0){
        return(glue('%',(sparse_ticks+1),'s'))
      }
      return('%s')
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
    sprintf(blockpad(i),glue(sprintf(left_or_right(i),i),right_flank(i)))
  }  
  numbers<-glue(str_trim(glue(sapply(seq(low,high,by=sparse_ticks),pad)),side="left"))
  if(substr(numbers,1,1)==d){left_margin<-unname(str_locate(numbers,w)[1,"start"])-2}else{left_margin<-0}

  n<-str_length(numbers)
  
  tb_border<-glue(p,glue(repme(d,n)),p)
  sparse_tick_marks<-glue(e,repme(w,left_margin),substr(blocks(glue(repme(glue(b,repme(w,sparse_ticks-1)),n)),blocksize=block_ticks*sparse_ticks),1,n-left_margin),e)
  
  #prefer to see dense ticks even before the 0 in -30 unless dealing with blocks
  edge_ticks<-function(){if(block_ticks>0){return(w)}else{return(b)}}
  
  dense_tick_marks<-glue(e,repme(edge_ticks(),left_margin),substr(blocks(repme(b,n),blocksize=block_ticks*sparse_ticks),1,n-left_margin),e)
  width<-str_length(dense_tick_marks)
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

default.asciiruler<-function(x){x$output}
print.asciiruler<-function(x,...){cat(x$output,...)}
as.character.asciiruler<-function(x,...){as.character(x$output)}

width<-function(x,...) UseMethod("width")
width.asciiruler<-function(x){x$width}

#' Generate a GenBank sequence block
#'     1    6     11   16    21   26    31   36    41   46    51   56   
#'     |    |     |    |     |    |     |    |     |    |     |    |    
#'     |||||||||| |||||||||| |||||||||| |||||||||| |||||||||| ||||||||||
#'   1 GATCACAGGT CTATCACCCT ATTAACCACT CACGGGAGCT CTCCATGCAT TTGGTATTTT
#'  61 CGTCTGGGGG GTGTGCACGC GATAGCATTG CGAGACGCTG GAGCCGGAGC ACCCTATGTC
#' 121 GCAGTATCTG TCTTTGATTC CTGCCCCATC CTATTATTTA TCGCACCTAC GTTCAATATT
#' 181 ACAGGCGAAC ATACTTACTA AAGTGTGTTA ATTAATTAAT GCTTGTAGGA CATAATAATA
#' 241 ACAATTGAAT GTCTGCACAG CCGCTTTCCA CACAGACATC ATAACAAAAA ATTTCCACCA
#' 301 AACCCCCCCT CCCCCGCTTC TGGCCACAGC ACTTAAACAC ATCTCTGCCA AACCCCAAAA
#' 361 ACAAAGAACC CTAACACCAG CCTAACCAGA TTTCAAATTT TATCTTTTGG CGGTATGCAC
#' 421 TTTTAACAGT CACCCCCCAA CTAACACATT ACTTTCCCCT CCCACTCCCA TACTACTAAT
#' 481 CTCATCAATA CAACCCCCGC CCATCCTACC CAGCACACAC ACCGCTGCTA ACCCCATACC
#' 541 CCGAACCAAC CAAACCCCAA AGACACCCCC CACAGTTTAT GTAGCTTACC TCCTCAAAGC
#' 601 AATACACTGA AAATGTTTAG ACGGGCTCAC
#' @param start, end substring start and end
#' @param blocksize number of characters in each block
#' @param sep space character between blocks
#' @param line_break the line break character(s)
#' @param ruler display an ascii ruler
#' @return character
genbank_seqblock<-function(string,start=1L,end=-1L,blocksize=10L,width=60L,sep=" ",line_break="\n",ruler=TRUE,...){
  if (length(string) == 0L || length(start) == 0L || length(end) == 0L) {
    return(vector("character", 0L))
  }
  
  # Convert negative values into actual positions
  len <- str_length(string)
  
  if(!is.na(start) & start < 0L){start <- start + len + 1L}
  if(!is.na(end) & end < 0L){end<-end+len+1L}
  
  subseq<-substring(string, start, end)
  
  #if a seq ends at 1000-1060 and width is 60 it must be at least 4 digits wide
  offset_char_width<-str_length(as.character(end-width))
  linestarts<-seq(start,end,by=width)
  linenumbers<-sprintf(glue('%',offset_char_width,'s'),linestarts)
  lineends<-seq_inclusive(from=start+width-1,to=end,by=width)
  lines<-substring(subseq,linestarts,lineends)

  textblocks<-sapply(lines,blocks,sep=sep,blocksize=blocksize)
  #paste(unname(sapply(lines,blocks)),collapse=line_break)
  bound<-rbind(num=linenumbers,text=textblocks)
  ascii<-
  if(ruler){
    ascii<-asciiruler(1,high=width,sparse_ticks=5,block_ticks=2,numbers_down=FALSE)
    asciishift<-paste(repme(' ',offset_char_width),ascii$content,collapse=line_break)
  }
  return(paste(asciishift,paste(as.list(bound[1,]),as.list(bound[2,]),sep=" ",collapse=line_break),sep=line_break))
}

#' This is like seq except it will always include the 'to'
#' > seq(1,30,by=10)
#' [1]  1 11 21
#' > seq_inclusive(1,30,by=10)
#' [1]  1 11 21 31
#' @param from, to start and end values
#' @param by increment value
seq_inclusive<-function(from,to,by){
  set<-seq(from,to,by)
  i<-set[length(set)]+by
  while (to>max(set)) {
    set<-c(set,i)
    i<-i+by
  }
  return(set)
}

#' Paste with logical defaults
glue<-function(...,sep="",collapse=""){
  strings<-list(...)
  do.call("paste", c(strings, list(sep = sep, collapse = collapse)))
}


#' Break line of text into blocks of size blocksize
#' @param line text
#' @param blocksize number of characters in each block
#' @param sep space character between blocks
blocks<-function(line,blocksize=10L,sep=" "){
    if(blocksize<=0){return(line)}
    blockstarts<-seq(1,str_length(line),by=blocksize)
    minLength<-max(blocksize,str_length(line))
    blockends<-seq_inclusive(from=blocksize,to=minLength,by=blocksize)
    paste(substring(line,blockstarts,blockends),collapse=sep)
}