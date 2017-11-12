#' Digests a protein sequence database.
#'
#' This function performs an in silico trypsin digestions of the imported protein sequence database (fasta file).
#'
#'  digest.fasta()
#'


digest.fasta <- function(fasta.df){

  oldw <- getOption("warn")
  options(warn = -1)

  # Test arguments
  # Test if all arguments are present
  if(missing(fasta.df))
    stop("ERROR: Need to specify fasta data frame")

  # Test for correct data frame
  fasta.test <- c("accession",
                  "name",
                  "sequence")
  fasta.colnames <- colnames(fasta.df)
  if(!(all(fasta.colnames == fasta.test)))
    stop("ERROR: SRL format not recognised.")

  # Convert data frame to date table
  fasta.dt <- data.table::data.table(fasta.df)
  # Inset a comma at digestion site
  x <- gsub("((?<=[K])(?=[^P]))|((?<=[R])(?=[^P]))",
            ",", fasta.dt$sequence, perl = T)
  fasta.dt$sequence <- x
  # Digest sequences
  digest.dt <- splitstackshape::cSplit(fasta.dt, "sequence", sep = ",", direction = "long", drop = F, fixed = F)
  digest.dt$sequence <- as.character(digest.dt$sequence)
  # Remove all peptides less than 5 and more than 52 amino acids long
  digest.2.dt <- digest.dt[!(nchar(digest.dt$sequence) < 5 | nchar(digest.dt$sequence) > 52), ]
  # Remove all non-unique peptides
  digest.3.dt <- unique(digest.2.dt, by = "sequence")
  # Conert data table back to data frame
  digest.df <- data.frame(digest.3.dt)

  options(warn = oldw)

  return(digest.df)
}

