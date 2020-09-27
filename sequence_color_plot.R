library(Biostrings)
library(tidyverse)
library(TxDb.Scerevisiae.UCSC.sacCer3.sgdGene)
library(GenomicRanges)
library(BSgenome.Scerevisiae.UCSC.sacCer3)

## Load sequences for testing
promoters <- trim(promoters(TxDb.Scerevisiae.UCSC.sacCer3.sgdGene,
                            upstream = 50, downstream = 25)) %>%
  subset(., seqnames %in% c("chrI", "chrII", "chrIII"))

seqs <- getSeq(genome, promoters)

seq_length <- unique(str_length(seqs))

seqs_split <- seqs %>%
  as.character %>%
  str_split(pattern = "", simplify = T) %>%
  as_tibble %>%
  setNames(1:seq_length) %>%
  rowid_to_column(var = "sequence") %>%
  pivot_longer(-sequence, names_to = "position", values_to = "base")

base_cols = c(
  "A" = "#109649",
  "C" = "#255C99",
  "G" = "#F7B32C",
  "T" = "#D62839"
)

ggplot(seqs_split, aes(x = position, y = sequence)) +
  geom_tile(aes(fill = base)) +
  theme_minimal() +
  scale_fill_manual(values = base_cols) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  ) 
