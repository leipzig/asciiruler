# asciiruler: Render an ASCII Ruler

[![Build Status](https://travis-ci.org/leipzig/asciiruler.svg?branch=master)](https://travis-ci.org/leipzig/asciiruler)

## Description

An ASCII ruler is for measuring text and is especially useful for sequence analysis. Methods `asciiruler` to create ASCII rulers and `genbank_seqblock` to create "GenBank sequence blocks", known for their distinctive column-structure, are provided.

## Usage

```
devtools::install_github("leipzig/asciiruler")
library("asciiruler")
asciiruler(low=-30,high=30,borders=TRUE)
```

```
+----------------------------------------------------------------+
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
|  |    |    |    |    |    |    |    |    |    |    |    |    | |
|-30  -25  -20  -15  -10   -5    0    5    10   15   20   25   30|
+----------------------------------------------------------------+
```

```
my_sequence<-
'GATCACAGGTCTATCACCCTATTAACCACTCACGGGAGCTCTCCATGCATTTGGTATTTTCGTCTGGGGG
GTATGCACGCGATAGCATTGCGAGACGCTGGAGCCGGAGCACCCTATGTCGCAGTATCTGTCTTTGATTC
CTGCCTCATCCTATTATTTATCGCACCTACGTTCAATATTACAGGCGAACATACTTACTAAAGTGTGTTA
ATTAATTAATGCTTGTAGGACATAATAATAACAATTGAATGTCTGCACAGCCACTTTCCACACAGACATC'
my_seqblock<-genbank_seqblock(my_sequence)
cat(my_seqblock)
```

```
    1    6     11   16    21   26    31   36    41   46    51   56   
    |    |     |    |     |    |     |    |     |    |     |    |    
    |||||||||| |||||||||| |||||||||| |||||||||| |||||||||| ||||||||||
  1 GATCACAGGT CTATCACCCT ATTAACCACT CACGGGAGCT CTCCATGCAT TTGGTATTTT
 61 CGTCTGGGGG GTATGCACGC GATAGCATTG CGAGACGCTG GAGCCGGAGC ACCCTATGTC
121 GCAGTATCTG TCTTTGATTC CTGCCTCATC CTATTATTTA TCGCACCTAC GTTCAATATT
181 ACAGGCGAAC ATACTTACTA AAGTGTGTTA ATTAATTAAT GCTTGTAGGA CATAATAATA
241 ACAATTGAAT GTCTGCACAG CCACTTTCCA CACAGACATC
```
