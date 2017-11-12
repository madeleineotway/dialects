#' Import an SRL.
#'
#' This function imports a spectral reference library in either PeakView or OpenSWATH formats.
#'
#'  import.srl()
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

