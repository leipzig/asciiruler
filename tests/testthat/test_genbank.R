library(asciiruler)
context("border cases")

my_sequence<-'GATCACAGGTCTATCACCCTATTAACCACTCACGGGAGCTCTCCATGCATTTGGTATTTTCGTCTGGGGGGTATGCACGCGATAGCATTGCGAGACGCTGGAGCCGGAGCACCCTATGTCGCAGTATCTGTCTTTGATTCCTGCCTCATCCTATTATTTATCGCACCTACGTTCAATATTACAGGCGAACATACTTACTAAAGTGTGTTAATTAATTAATGCTTGTAGGACATAATAATAACAATTGAATGTCTGCACAGCCACTTTCCACACAGACATC'
expected_default<-"    1    6     11   16    21   26    31   36    41   46    51   56   \n    |    |     |    |     |    |     |    |     |    |     |    |    \n    |||||||||| |||||||||| |||||||||| |||||||||| |||||||||| ||||||||||\n  1 GATCACAGGT CTATCACCCT ATTAACCACT CACGGGAGCT CTCCATGCAT TTGGTATTTT\n 61 CGTCTGGGGG GTATGCACGC GATAGCATTG CGAGACGCTG GAGCCGGAGC ACCCTATGTC\n121 GCAGTATCTG TCTTTGATTC CTGCCTCATC CTATTATTTA TCGCACCTAC GTTCAATATT\n181 ACAGGCGAAC ATACTTACTA AAGTGTGTTA ATTAATTAAT GCTTGTAGGA CATAATAATA\n241 ACAATTGAAT GTCTGCACAG CCACTTTCCA CACAGACATC"

test_that("genbank sequence with negative start", {
  expect_equal(genbank_seqblock(my_sequence, start=-20, end=20),genbank_seqblock(my_sequence, start=1, end=20))
})

test_that("genbank sequence negative end", {
  expect_equal(genbank_seqblock(my_sequence, start=1, end=-20),genbank_seqblock(my_sequence, start=1,nchar(my_sequence)-20))
})

test_that("genbank sequence end too long", {
  expect_equal(genbank_seqblock(my_sequence, start=-20, end=1000),genbank_seqblock(my_sequence, start=1,nchar(my_sequence)))
})

test_that("width nonsense", {
  expect_equal(genbank_seqblock(my_sequence, width=-20),genbank_seqblock(my_sequence, width=60))
})

test_that("empty", {
  expect_equal(genbank_seqblock('', start=-20, end=1000),character(0))
  expect_equal(genbank_seqblock(NULL, start=-20, end=1000),character(0))
})

test_that("genbank sequence no ruler", {
  expect_equal(genbank_seqblock(my_sequence, ruler=FALSE),"  1 GATCACAGGT CTATCACCCT ATTAACCACT CACGGGAGCT CTCCATGCAT TTGGTATTTT\n 61 CGTCTGGGGG GTATGCACGC GATAGCATTG CGAGACGCTG GAGCCGGAGC ACCCTATGTC\n121 GCAGTATCTG TCTTTGATTC CTGCCTCATC CTATTATTTA TCGCACCTAC GTTCAATATT\n181 ACAGGCGAAC ATACTTACTA AAGTGTGTTA ATTAATTAAT GCTTGTAGGA CATAATAATA\n241 ACAATTGAAT GTCTGCACAG CCACTTTCCA CACAGACATC")
})