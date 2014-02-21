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
#'   @param low range start, inclusive
#'   @param high range end, inclusive
#'   @param sparse_ticks intermittent ticks appear every sparse_ticks, set to 0 to hide row
#'   @param dense_ticks display a row of ticks at every position
#'   @param block_space break up the ruler with a space every block_space blocks encountered, a multiple of sparse_ticks, set to 0 to disable
#'   @param custom_space vector of spaces, overrides block_space, should be an IRangesList
#'   @param display borders
#'   @param numbers_down display the ruler so the numbers are below the ticks
#'   @param line_break the line break character(s)
#'   @param strict_width hide extreme numbers whose display would force the ruler to be wider than width(high-low)
#'   @return asciiruler object with the following slots:
#'   \describe{
#'    \item{output}{delimited ruler string ready to cat}
#'    \item{content}{vector of lines comprising the ruler}
#'    \item{width}{width of ruler}
#'    \item{leftmargin}{position of the first tick relative to the left edge of the ruler}
#'   }
#'   @examples
#'   asciiruler(low=-30,high=30,borders=TRUE)
#'   @references Inspired by \code{\url{http://codegolf.stackexchange.com/questions/4910/ascii-ruler-generation}}
#'   @import stringr
#'   @export
asciiruler<-function(low=0L,high=50L,sparse_ticks=5L,dense_ticks=TRUE,block_space=0L,custom_space=NULL,borders=FALSE,numbers_down=TRUE,line_break="\n",strict_width=FALSE){
  if(borders){e<-'|'}else{e<-''}
  b<-'|'
  p<-"+"
  d<-'-'
  
  pad<-function(i){
    #returns special right flank padding for ends and kingpin
    right_flank<-function(x){
      #last number should be right flanked to get to high but no further
      if(x<high & x+sparse_ticks>high){return(spaces(abs(high-x-stringr:::str_length(as.character(x))+1)))}
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
      if((i==low | i==high) & stringr:::str_length(as.character(i))>1 & strict_width==TRUE){return('_')}
      return(i)
    }
    glue(blockpad(i),sprintf(left_or_right(i),strictify(i)),right_flank(i))
  }  
  numbers<-stringr:::str_replace_all(glue(stringr:::str_trim(glue(sapply(seq(low,high,by=sparse_ticks),pad)),side="left")),'_',' ')
  if(substr(numbers,1,1)==d){
    left_margin<-unname(stringr:::str_locate(numbers,' ')[1,"start"])-2
  }else{
    left_margin<-0
  }

  n<-stringr:::str_length(numbers)
  
  tb_border<-glue(p,glue(repme(d,n)),p)
  sparse_tick_marks<-glue(e,spaces(left_margin),substr(blocks(glue(repme(glue(b,spaces(sparse_ticks-1)),n)),blocksize=block_space*sparse_ticks),1,n-left_margin),e)
  
  #prefer to see dense ticks even before the 0 in -30 unless dealing with blocks
  edge_ticks<-function(){if(block_space>0){return(' ')}else{return(b)}}
  
  dense_tick_marks<-glue(e,repme(edge_ticks(),left_margin),substr(blocks(repme(b,n),blocksize=block_space*sparse_ticks),1,n-left_margin),e)
  width<-stringr:::str_length(dense_tick_marks)
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
default.asciiruler<-function(x){x$output}

#' @export
print.asciiruler<-function(x,...){cat(x$output,...)}

#' @export
as.character.asciiruler<-function(x,...){as.character(x$output)}

#http://stackoverflow.com/questions/21890835/how-do-i-dispatch-cat-in-r-s3
#' @export
cat <- function(..., file = "", sep = " ", fill = FALSE, labels = NULL,
                append = FALSE) {
  UseMethod("cat")
}

#' @export
cat.default <- function(..., file = "", sep = " ", fill = FALSE, labels = NULL,
                        append = FALSE) {
  base::cat(..., file = file, sep = sep, fill = fill, labels = labels, 
            append = append)
}

#' @export
cat.asciiruler <- function(..., file = "", sep = " ", fill = FALSE, labels = NULL,
                    append = FALSE) {
  dots <- list(...)
  sapply(dots,function(x){base::cat(x[['output']],"\n")})
  #do.call(function(x){base::cat(x[['output']])},dots)
}

#' @export
width<-function(x,...) UseMethod("width")

#' @export
width.asciiruler<-function(x){x$width}