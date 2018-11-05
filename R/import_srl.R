#' Import an SRL.
#'
#' This function imports a spectral reference library (SRL) in either
#' PeakView/OneOmics or OpenSWATH formats.
#'
#' @param filepath Full path to SRL file. I.e. /path/to/SRL.file
#' @param SRL.format The format of the SRL. Either "peakview" for PeakView or
#' OneOmics SRLs or "openswath" for OpenSWATH SRLs. Defaults to "peakview".
#'
#' @return SRL
#'
#' @examples
#' srl_pv <- system.file("extdata",
#'                       "rat_srl_example.txt",
#'                       package = "dialects")
#' importSRL(srl_pv)
#' importSRL(srl_pv, SRL.format = "peakview")
#'
#' #srl-os <- system.file("extdata", "SRL-OS", package = "dialects")
#' #importSRL(srl-os, SRL.format = "openswath")
#'
#' @note PeakView and OneOmics SRLs must be .txt format. OpenSWATH must be either .tsv or .csv format.
#'
#' @author Madeleine J Otway \email{motway@@cmri.org.au}
#'
#' @importFrom utils read.table
#' @export importSRL
#'
#'
importSRL <- function(filepath = NULL,
                      SRL.format = "peakview"){

  oldw <- getOption("warn")
  options(warn = -1)

  ## Test arguments
  ## Test if all arguments are present
  if(missing(filepath))
    stop("ERROR: Need to specify SRL filepath")

  ## Test if SRL.format matches file format
  filepath.test <- filepath
  if(SRL.format == "peakview" & (!(endsWith(filepath.test, "txt"))))
    stop("ERROR: Please check PeakView/OneOmics SRL file format is .txt")

  if(SRL.format == "openswath" & (!(endsWith(filepath.test, "csv"))))
    if(SRL.format == "openswath" & (!(endsWith(filepath.test, "tsv"))))
      stop("ERROR: Please check OpenSWATH SRL file format is either .csv or .tsv")

  if((SRL.format != "peakview") & (SRL.format != "openswath"))
    stop("Error: SRL.format can be \"peakview\" or \"openswath\" only.")

  ## Find separator
  if((endsWith(filepath.test, "txt")) | (endsWith(filepath.test, "tsv")))
    srl.df <- read.table(filepath,
                         sep = "\t",
                         header = TRUE,
                         quote = NULL,
                         stringsAsFactors = FALSE)

  if((endsWith(filepath.test, "csv")))
    srl.df <- read.table(filepath,
                         sep = ",",
                         header = TRUE,
                         quote = NULL,
                         stringsAsFactors = FALSE)

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

