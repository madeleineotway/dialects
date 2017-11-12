#' Import an protein sequene database.
#'
#' This function imports a protein sequence database in fasta file format.
#'
#'  export.srl()
#'

import.fasta <- function(fasta.file){

  oldw <- getOption("warn")
  options(warn = -1)

  # Test arguments
  # Test if all arguments are present
  if(missing(fasta.file))
    stop("ERROR: Need to specify fasta filepath")

  # Test if SRL.format matches file format
  fasta_file.test <- fasta.file
  if(!(endsWith(fasta_file.test, ".fasta")))
    stop("ERROR: File not fasta format.")

  # create empty list
  fasta.list <- list()

  # import fasta into list
  fasta.list <- seqinr::read.fasta(fasta.file,
                           seqtype = "AA",
                           as.string = T,
                           set.attributes = T,
                           strip.desc = F)

  # create data frame with details from list
  fasta.df <- data.frame("accession" = (seqinr::getName.SeqFastaAA(fasta.list)),
                         "name" = (unlist(seqinr::getAnnot(fasta.list))),
                         "sequence" = (unlist(seqinr::getSequence.character(fasta.list))),
                         row.names = c(),
                         stringsAsFactors = F)

  # Remove the accession from the name of the protein
  fasta.df$name <- gsub("^[^ ]* ", "", fasta.df$name)

  fasta.df$accession <- as.character(fasta.df$accession)
  fasta.df$name <- as.character(fasta.df$name)
  fasta.df$sequence <- as.character(fasta.df$sequence)

  options(warn = oldw)

  return(fasta.df)

}
