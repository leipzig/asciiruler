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
#' @param start, end substring start and end
#' @param blocksize number of characters in each block
#' @param sep space character between blocks
#' @param line_break the line break character(s)
#' @param ruler display an ascii ruler
#' @return delimited GenBank block string ready to cat
#' @examples
#' my_sequence<-'GATCACAGGTCTATCACCCTATTAACCACTCACGGGAGCTCTCCATGCATTTGGTATTTTCGTCTGGGGG
#' GTATGCACGCGATAGCATTGCGAGACGCTGGAGCCGGAGCACCCTATGTCGCAGTATCTGTCTTTGATTC
#' CTGCCTCATCCTATTATTTATCGCACCTACGTTCAATATTACAGGCGAACATACTTACTAAAGTGTGTTA
#' ATTAATTAATGCTTGTAGGACATAATAATAACAATTGAATGTCTGCACAGCCACTTTCCACACAGACATC'
#' my_seqblock<-genbank_seqblock(my_sequence)
#' cat(my_seqblock)
#' @export
genbank_seqblock<-function(string,start=1L,end=-1L,blocksize=10L,width=60L,sep=" ",line_break="\n",ruler=TRUE,...){
  if (length(string) == 0L || length(start) == 0L || length(end) == 0L) {
    return(vector("character", 0L))
  }
  
  # Convert negative values into actual positions
  len <- stringr:::str_length(string)
  
  if(!is.na(start) & start < 0L){start <- start + len + 1L}
  if(!is.na(end) & end < 0L){end<-end+len+1L}
  
  #remove whitespace
  string<-stringr:::str_replace_all(string,'\\s','')
  
  subseq<-substring(string, start, end)
  
  #if a seq ends at 1000-1060 and width is 60 it must be at least 4 digits wide
  offset_char_width<-stringr:::str_length(as.character(end-width))
  linestarts<-seq(start,end,by=width)
  linenumbers<-sprintf(glue('%',offset_char_width,'s'),linestarts)
  lineends<-seq_inclusive(from=start+width-1,to=end,by=width)
  lines<-substring(subseq,linestarts,lineends)
  
  textblocks<-sapply(lines,blocks,sep=sep,blocksize=blocksize)
  bound<-rbind(num=linenumbers,text=textblocks)
  ascii<-''
  if(ruler){
    ascii<-asciiruler(1,high=width,sparse_ticks=5,block_space=2,numbers_down=FALSE)
    asciishift<-paste(repme(' ',offset_char_width),ascii$content,collapse=line_break)
    return(paste(asciishift,paste(as.list(bound[1,]),as.list(bound[2,]),sep=" ",collapse=line_break),sep=line_break))
  }
  return(paste(as.list(bound[1,]),as.list(bound[2,]),sep=" ",collapse=line_break))
}