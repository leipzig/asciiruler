\name{asciiruler}
\alias{asciiruler}
\title{Generate an ascii ruler}
\usage{
  asciiruler(low = 0L, high = 50L, sparse_ticks = 5L,
    dense_ticks = TRUE, block_ticks = 0L, borders = FALSE,
    numbers_down = TRUE, line_break = "\n")
}
\arguments{
  \item{low}{range start}

  \item{high}{range end}

  \item{sparse_ticks}{intermittent ticks appear every
  sparse_ticks, set to 0 to hide row}

  \item{dense_ticks}{display a row of ticks at every
  position}

  \item{block_ticks}{break up the ruler with a space every
  block_ticks encountered, a multiple of sparse_ticks, set
  to 0 to disable}

  \item{display}{borders}

  \item{numbers_down}{display the ruler so the numbers are
  below the ticks}

  \item{line_break}{the line break character(s)}
}
\value{
  asciiruler object
}
\description{
\preformatted{v borders
   +-----------------------------------------------------------------+<-borders
   |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||<-dense_ticks
   |  |    |    |    |    |    |    |    |    |    |    |    |    |  |<-sparse_ticks
   |-30       -20       -10         0         10        20        30 |
   +-----------------------------------------------------------------+<-borders
   }
}
\examples{
asciiruler(low=-30,high=30,borders=TRUE)
}
\references{
  Inspired by
  \code{\url{http://codegolf.stackexchange.com/questions/4910/ascii-ruler-generation}}
}

