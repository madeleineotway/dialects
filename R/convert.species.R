#' Conversion of an SRL from one species to another.
#'
#' Converts the species in the spectral reference library to the species in the
#' digested fasta data frame where full peptide identity occurs.
#'
#'
#' @param SRL.df A data frame of the spectral reference library from
#' \code{\link{import.srl}}
#' @param digest.df A data frame of the digested protein sequence from the
#' desired species generated from  \code{\link{digest.fasta}}
#'
#' @return A data frame of the new spectral reference library with the peptides
#' from the desired species.
#'
#' @examples
#' digest <- system.file("extdata", "DIGEST", package = "dialects")
#' srl <- ("extdata", "SRL", package = "dialects")
#' convert.species(digest, srl)
#'
#'
#' @author Madeleine J Otway \email{motway@@cmri.org.au}
#'
#' @family related
#' @seealso
#'
#' @export convert.species
#'
#'
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
