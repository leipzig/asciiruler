\name{seq_inclusive}
\alias{seq_inclusive}
\title{This is like seq except it will always include the 'to'
> seq(1,30,by=10)
[1]  1 11 21
> seq_inclusive(1,30,by=10)
[1]  1 11 21 31}
\usage{
  seq_inclusive(from, to, by)
}
\arguments{
  \item{from,}{to start and end values}

  \item{by}{increment value}
}
\description{
  This is like seq except it will always include the 'to' >
  seq(1,30,by=10) [1] 1 11 21 > seq_inclusive(1,30,by=10)
  [1] 1 11 21 31
}

