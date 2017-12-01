#' Tryptic digestion of a protein sequence dataframe.
#'
#' This function performs an in-silico trypsin digestion on a data frame from a
#' protein sequence fasta file.
#'
#' @details Data frame must have the vector names: accession, name, sequence
#'
#' @param fasta.df Data frame generated from \code{\link{import.fasta}}
#'
#' @return Returns a data frame of the peptide sequences of the digested
#'   proteins.
#'
#' @note This function removes all non-unique peptides. Removes all peptides
#'   less than 5 and greater than 52 amino acids.
#'
#' @examples
#' fasta <- system.file("extdata", "FASTA.DF", package = "dialects")
#' digest.fasta(fasta)
#'
#'
#' @author Madeleine J Otway \email{motway@@cmri.org.au}
#'
#' @family related
#' @seealso
#'
#' @export digest.fasta
#' @import data.table
#' @import splitstackshape
#'
#'
#'
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
  digest.dt <- splitstackshape::cSplit(fasta.dt, "sequence",
                                       sep = ",",
                                       direction = "long",
                                       drop = F,
                                       fixed = F)
  digest.dt$sequence <- as.character(digest.dt$sequence)
  # Remove all peptides less than 5 and more than 52 amino acids long
  digest.2.dt <- digest.dt[!(nchar(digest.dt$sequence) < 5 |
                           nchar(digest.dt$sequence) > 52), ]
  # Remove all non-unique peptides
  digest.3.dt <- unique(digest.2.dt, by = "sequence")
  # Conert data table back to data frame
  digest.df <- data.frame(digest.3.dt)

  options(warn = oldw)

  return(digest.df)
}

