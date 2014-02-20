# asciiruler: Render an ascii ruler

## Description

An ascii ruler is for measuring text and is especially useful for sequence analysis. Methods `asciiruler` to create ascii rulers and `genbank_seqblock` to create "GenBank sequence blocks", known for their distinctive column-structure, are provided.

## Usage



```
library("devtools")
install_github("asciiruler","leipzig")
library("asciiruler")
asciiruler(low=1,high=60)
```

```
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
|    |    |    |    |    |    |    |    |    |    |    |    
1    6    11   16   21   26   31   36   41   46   51   56   
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