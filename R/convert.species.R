#' Converts the SRL species.
#'
#' This function converts the species of the imported SRL to that of the imported fasta file.
#'
#'  convert.species()
#'

convert.species <- function(SRL.df, digest.df){

  oldw <- getOption("warn")
  options(warn = -1)

  # Test arguments
  # Test if all arguments are present
  if(missing(SRL.df))
    stop("ERROR: Need to specify SRL data frame")
  if(missing(digest.df))
    stop("ERROR: Need to specify the digested proteome data frame")

  # Test for SRL format
  peakview.test <- "Q1"
  openswath.test <- "PrecursorMz"
  SRL.colnames <- colnames(SRL.df[1])

  if(!((SRL.colnames == peakview.test) | (SRL.colnames == openswath.test)))
    stop("ERROR: SRL format not recognised.")

  # PeakView
  if((SRL.colnames == peakview.test) & (!(SRL.colnames == openswath.test))){
    SRL.df <- SRL.df[!(SRL.df$protein_name == "Retention time calibration protein"),]
    # find matching peptides
    SRL.df$match <- ifelse(SRL.df$stripped_sequence %in% digest.df$sequence, 1, 0)
    # create new SRL
    new.SRL.df <- SRL.df[!(SRL.df$match == "0"),]
    new.SRL.df$match <- NULL
    # match protein accession and name
    new.SRL.df$protein_name <- digest.df$name[match(new.SRL.df$stripped_sequence, digest.df$sequence)]
    new.SRL.df$uniprot_id <- digest.df$accession[match(new.SRL.df$stripped_sequence, digest.df$sequence)]
  }


  #OpenSWATH
  if((SRL.colnames == openswath.test) & (!(SRL.colnames == peakview.test))){
    # find matching peptides
    SRL.df$match <- ifelse(SRL.df$PeptideSequence %in% digest.df$sequence, 1, 0)
    # create new SRL
    new.SRL.df <- SRL.df[!(SRL.df$match == "0"),]
    new.SRL.df$match <- NULL
    # match protein accession and name
    new.SRL.df$protein_name <- digest.df$name[match(new.SRL.df$PeptideSequence, digest.df$sequence)]
    new.SRL.df$uniprot_id <- digest.df$accession[match(new.SRL.df$PeptideSequence, digest.df$sequence)]
  }

  options(warn = oldw)

  return(new.SRL.df)
}
