ruler<-function(x,...) UseMethod("ruler")

#' Display a ruled pairwise alignment
#' 
#' Numbering is relative to the alignment pattern
#' This becomes important when when diplaying insertions in the query
ruler.PairwiseAlignmentsSingleSubject <- function(pwa){
  ref<-as.character(pwa@pattern@unaligned[[1]])
  query<-as.character(pwa@subject@unaligned[[1]])
  pwa@pattern@range@start
  pwa@pattern@range@width
  pwa@subject@range@start
}
