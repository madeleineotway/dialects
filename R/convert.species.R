#' Conversion of an SRL from one species to another.
#'
#' Converts the species in the spectral reference library to the species in the
#' digested fasta data frame where full peptide identity occurs.
#'
#'
#' @param SRL.df A data frame of the spectral reference library from
#' \code{\link{import.srl}}.
#' @param digest.df A data frame of the digested protein sequence from the
#' desired species generated from  \code{\link{digest.fasta}}.
#' @param SRL.format The format of the SRL. Either "peakview" for PeakView or
#' OneOmics SRLs or "openswath" for OpenSWATH SRLs. Defaults to "peakview".
#'
#' @return A data frame of the new spectral reference library with the peptides
#' from the desired species.
#'
#' @note This process will remove the retention time calibration protein from a
#' PeakView/OneOmics SRL.
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
convert.species <- function(SRL.df, digest.df, SRL.format = "peakview"){

  oldw <- getOption("warn")
  options(warn = -1)

  # Test arguments
  # Test if all arguments are present
  if(missing(SRL.df))
    stop("ERROR: Need to specify SRL data frame")
  if(missing(digest.df))
    stop("ERROR: Need to specify the digested proteome data frame")

  ## Test for SRL format
  ## PeakView/OneOmics
  if(SRL.format == "peakview" &
     (!(colnames(SRL.df[1]) == "Q1")))
    stop("ERROR: SRL is not formatted for PeakView or OneOmics.")
  ## OpenSWATH
  if(SRL.format == "openswath" &
     (!(colnames(SRL.df[1]) == "PrecursorMz")))
    stop("ERROR: SRL is not formatted for OpenSWATH.")

  ## Set column names
  ## PeakView
  if(SRL.format == "peakview"){
    names(SRL.df)[names(SRL.df) == "protein_name"] <- "protein"
    names(SRL.df)[names(SRL.df) == "uniprot_id"] <- "uniprot"
    names(SRL.df)[names(SRL.df) == "stripped_sequence"] <- "peptide"
  }
  ## OpenSWATH
  if(SRL.format == "openswath"){
    names(SRL.df)[names(SRL.df) == "ProteinName"] <- "protein"
    names(SRL.df)[names(SRL.df) == "UniprotID"] <- "uniprot"
    names(SRL.df)[names(SRL.df) == "PeptideSequence"] <- "peptide"
  }


  ## Remove retention time proteins from PeakView/OneOmics SRL
  if(SRL.format == "peakview"){
    SRL.df <- SRL.df[!(SRL.df$uniprot == "[ RT-Cal protein ]"),]
  }

  ## Find matching peptides
  SRL.df$match <- ifelse(SRL.df$peptide %in% digest.df$sequence, 1, 0)
  ## Create new SRL
  new.SRL.df <- SRL.df[!(SRL.df$match == "0"),]
  new.SRL.df$match <- NULL
  ## Match protein accession and name
  new.SRL.df$protein <-
    digest.df$name[match(new.SRL.df$peptide, digest.df$sequence)]
  new.SRL.df$uniprot <-
    digest.df$accession[match(new.SRL.df$peptide, digest.df$sequence)]

  ## Reverse names
  ## PeakView/OneOmics
  if(SRL.format == "peakview"){
    names(new.SRL.df)[names(new.SRL.df) == "protein"] <- "protein_name"
    names(new.SRL.df)[names(new.SRL.df) == "uniprot"] <- "uniprot_id"
    names(new.SRL.df)[names(new.SRL.df) == "peptide"] <- "stripped_sequence"
  }
  ## OpenSWATH
  if(SRL.format == "openswath"){
    names(new.SRL.df)[names(new.SRL.df) == "protein"] <- "ProteinName"
    names(new.SRL.df)[names(new.SRL.df) == "uniprot"] <- "UniprotID"
    names(new.SRL.df)[names(new.SRL.df) == "peptide"] <- "PeptideSequence"
  }

  options(warn = oldw)

  return(new.SRL.df)
}
