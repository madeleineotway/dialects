## ----echo = F, message = F, warning = F------------------------------------
library(dialects)
library(knitr)
library(kableExtra)
<<<<<<< HEAD
library(BiocStyle)
=======
>>>>>>> b3066e021987c65de1aab75562d8e7dd258e4467

## --------------------------------------------------------------------------
library(dialects)
library(knitr)
library(kableExtra)
<<<<<<< HEAD
library(BiocStyle)
=======
>>>>>>> b3066e021987c65de1aab75562d8e7dd258e4467

## --------------------------------------------------------------------------
human_fasta <- system.file("extdata",
                           "human_proteome_example.fasta",
                           package = "dialects")
human_proteome <- import.fasta(human_fasta)

## ----echo = FALSE, results = 'asis', fig.align='left'----------------------
human_proteome_s <- human_proteome
human_proteome_s$sequence <- gsub(".{160}",
          "",
          human_proteome_s$sequence)
human_proteome_s$sequence <- gsub("(.{21})",
          "\\1\t",
          human_proteome_s$sequence,
          perl = T)
kable(head(human_proteome_s), format = "html")  %>%
  kable_styling(bootstrap_options = "striped") %>%
  footnote("Protein sequences have been reduced to improve readibility of vignette")


## --------------------------------------------------------------------------
digest_human <- digest.fasta(human_proteome)

## ----echo = FALSE, results = 'asis'----------------------------------------
digest_human_char <- digest_human
digest_human_char$char <- nchar(digest_human_char$sequence)
digest_human_char <- digest_human_char[!(digest_human_char$char >= 30),]
digest_human_char$char <- NULL
rownames(digest_human_char) <- NULL
kable(head(digest_human_char), format = "html")  %>%
  kable_styling(bootstrap_options = "striped") %>%
  footnote("Peptides over 30 characters have been removed to improve readibility of vignette")

## --------------------------------------------------------------------------
peakview_example <- system.file("extdata",
                                "rat_srl_example.txt",
                                package = "dialects")

rat_srl_pv <- import.srl(peakview_example,
                         SRL.format = "peakview")

## ----echo = FALSE, results = 'asis'----------------------------------------
kable(head(rat_srl_pv[,1:6]), format = "html") %>%
  kable_styling(bootstrap_options = "striped") %>%
  footnote("Only the first 6 columns are shown to improve readibility of vignette")

## --------------------------------------------------------------------------
openswath_example <- system.file("extdata",
                                 "rat_srl_example_openswath.tsv",
                                 package = "dialects")

rat_srl_os <- import.srl(openswath_example,
                         SRL.format = "openswath")

## ----echo = FALSE, results = 'asis'----------------------------------------
rat_srl_os$char <- nchar(rat_srl_os$PeptideSequence)
rat_srl_os <- rat_srl_os[!(rat_srl_os$char >= 10),]
rat_srl_os$char <- NULL
rownames(rat_srl_os) <- NULL
kable(head(rat_srl_os[,1:5]), format = "html") %>%
  kable_styling(bootstrap_options = "striped") %>%
  footnote("Only the first 5 columns are shown to improve readibility of vignette")

## --------------------------------------------------------------------------
human_from_rat <- convert.species(digest_human,
                                  rat_srl_pv,
                                  SRL.format = "peakview")

## ----echo = FALSE, results = 'asis'----------------------------------------
rownames(human_from_rat) <- NULL
kable(head(human_from_rat[,1:6]), format = "html") %>%
  kable_styling(bootstrap_options = "striped") %>%
  footnote("Only the first 6 columns are shown to improve readibility of vignette")

## --------------------------------------------------------------------------
savepath <- system.file("extdata",
                        "human_from_rat_srl.txt",
                        package = "dialects")

export.srl(human_from_rat, savepath)

## --------------------------------------------------------------------------
sessionInfo()

