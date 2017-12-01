#' Import an SRL.
#'
#' This function imports a spectral reference library (SRL) in either
#' PeakView/OneOmics or OpenSWATH formats.
#'
#' @param filepath Directory and filename of the SRL.
#' @param SRL.format The format of the SRL. Either "peakview" for PeakView or
#' OneOmics SRLs or "openswath" for OpenSWATH SRLs. Defaults to "peakview".
#'
#' @examples
#' srl_pv <- system.file("extdata", "rat_srl_example.txt", package = "dialects")
#' import.srl(srl_pv)
#' import.srl(srl_pv, SRL.format = "peakview")
#'
#' #srl-os <- system.file("extdata", "SRL-OS", package = "dialects")
#' #import.srl(srl-os, SRL.format = "openswath")
#'
#'
#' @author Madeleine J Otway \email{motway@@cmri.org.au}
#'
#' @family related
#' @seealso
#'
#' @importFrom utils read.table
#' @export import.srl
#'
#'
import.srl <- function(filepath, SRL.format = "peakview"){

  oldw <- getOption("warn")
  options(warn = -1)

  ## Test arguments
  ## Test if all arguments are present
  if(missing(filepath))
    stop("ERROR: Need to specify SRL filepath")

  ## Test if SRL.format matches file format
  filepath.test <- filepath
  if(SRL.format == "peakview" & (!(endsWith(filepath.test, "txt"))))
    stop("ERROR: Incorrect PeakView format.")

  if(SRL.format == "openswath")
    if ((!(endsWith(filepath.test, ".csv"))) | (!(endsWith(filepath.test, ".tsv"))))
      stop("ERROR: Incorrect OpenSWATH format.")

  if((SRL.format != "peakview") & (SRL.format != "openswath"))
    stop("Error: SRL.format can be \"peakview\" or \"openswath\" only.")

  ## Find separator
  if((endsWith(filepath.test, "txt")) | (endsWith(filepath.test, "tsv")))
    srl.df <- read.table(filepath,
                         sep = "\t",
                         header = T,
                         quote = NULL,
                         stringsAsFactors = F)

  if((endsWith(filepath.test, ".csv")))
    srl.df <- read.table(filepath,
                         sep = ",",
                         header = T,
                         quote = NULL,
                         stringsAsFactors = F)

  ## Test expected header names
  ## PeakView
  if(SRL.format == "peakview" & (!(colnames(srl.df[1]) == "Q1")))
    stop("ERROR: SRL is not formatted for PeakView or OneOmics.")
  ## OpenSWATH
  if(SRL.format == "openswath" & (!(colnames(srl.df[1]) == "PrecursorMz")))
    stop("ERROR: SRL is not formatted for OpenSWATH.")

  options(warn = oldw)

  return(srl.df)
}

