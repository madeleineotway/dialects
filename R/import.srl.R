#' Import an SRL.
#'
#' This function imports a spectral reference library (SRL) in either
#' PeakView/OneOmics or OpenSWATH formats.
#'
#' @param filepath Directory and filename of the SRL
#' @param SRL.format The format of the SRL. Either "peakview" or "openswath".
#' Defaults to "peakview"
#'
#' @examples
#' srl-pv <- system.file("extdata", "SRL-PV", package = "dialects")
#' import.srl(srl-pv)
#' import.srl(srl-pv, SRL.format = "peakview")
#'
#' #' srl-os <- system.file("extdata", "SRL-OS", package = "dialects")
#' import.srl(srl-os, SRL.format = "openswath")
#'
#'
#' @author Madeleine J Otway \email{motway@@cmri.org.au}
#'
#' @family related
#' @seealso
#'
#' @export import.srl
#'
#'
import.srl <- function(filepath, SRL.format = "peakview"){

  oldw <- getOption("warn")
  options(warn = -1)

  # Test arguments
  # Test if all arguments are present
  if(missing(filepath))
    stop("ERROR: Need to specify SRL filepath")

  # Test if SRL.format matches file format
  filepath.test <- filepath
  if(SRL.format == "peakview" & (!(endsWith(filepath.test, ".txt"))))
    stop("ERROR: Incorrect SRL format.")
  if(SRL.format == "openswath" & (!(endsWith(filepath.test, ".csv"))))
      stop("ERROR: Incorrect SRL format.")

  #PEAKVIEW / ONEOMICS
  if(SRL.format == "peakview")
    srl.df <- read.table(filepath,
                         sep = "\t",
                         header = T,
                         quote = NULL,
                         stringsAsFactors = F)

  #OpenSWATH
  if(SRL.format == "openswath")
    srl.df <- read.table(filepath,
                         sep = ",",
                         header = T,
                         quote = NULL,
                         stringsAsFactors = F)

  options(warn = oldw)

  return(srl.df)
}

