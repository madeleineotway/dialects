#' Export an SRL.
#'
#' Exports a spectral reference library (SRL) in either PeakView/OneOmics or
#' OpenSWATH formats.
#'
#' @param SRL.df Data frame of an SRL
#' @param SRL.filepath The directory and filename of the SRL to be exported
#'
#' @examples
#' srl <- ("extdata", "SRL", package = "dialects")
#' export.srl(srl, "new-srl.txt")
#'
#'
#' @author Madeleine J Otway \email{motway@@cmri.org.au}
#'
#' @family related
#' @seealso
#'
#' @export export.srl
#'
#'
#'
export.srl <- function(SRL.df, SRL.filepath){

  oldw <- getOption("warn")
  options(warn = -1)

  # Test arguments
  # Test if all arguments are present
  if(missing(SRL.df))
    stop("ERROR: Need to specify SRL data frame")
  if(missing(SRL.filepath))
    stop("ERROR: Need to specify filepath for the SRL")

  # Test for SRL format
  peakview.test <- "Q1"
  openswath.test <- "PrecursorMz"
  SRL.colnames <- colnames(SRL.df[1])

  if(!((SRL.colnames == peakview.test) | (SRL.colnames == openswath.test)))
    stop("ERROR: SRL format not recognised.")

  # Test if SRL.format matches file format
  filepath.test <- SRL.filepath
  if((SRL.colnames == peakview.test) & (!(endsWith(filepath.test, ".txt"))))
    stop("ERROR: Incorrect SRL format.")
  if((SRL.colnames == openswath.test))
     if((!(endsWith(filepath.test, ".csv"))) |
        (!(endsWith(filepath.test, ".tsv"))))
       stop("ERROR: Incorrect SRL format.")

  if((endsWith(filepath.test, ".txt")) | (endsWith(filepath.test, ".tsv"))) {
    write.table(SRL.df,
              file = SRL.filepath,
              quote = F,
              sep = "\t",
              na = "",
              row.names = F,
              col.names = T)
  }

  if(endsWith(filepath.test, ".csv")) {
    write.table(SRL.df,
                file = SRL.filepath,
                quote = F,
                sep = ",",
                na = "",
                row.names = F,
                col.names = T)
  }

  options(warn = oldw)

  return(print("Output complete"))
}
