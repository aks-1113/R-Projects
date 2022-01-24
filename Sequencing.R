library(tidyverse)

enzs <- c("AanI","AatII","AccIII","Acc16I")
seqs_1 <- c("ttataa","gacgtc","tccgga","tgcgca")
seqs_4 <- charToRaw(seqs_1)
seqs_2 <- strsplit(seqs_1,"")
seqs_3 <- unlist
seq_df <- rbind.data.frame(enzs,seqs_2)

lengths <- nchar(seqs, type = "chars")
lengths_min <- min(lengths)

seqanal <- function(a) {
  for(a in lengths_min) {
    
  }
}

x <- c("asfef", "qwerty", "yuiop[", "b", "stuff.blah.yech")
substr(x, 2, 5)