#' Export an SRL.
#'
#' Exports a spectral reference library (SRL) in either PeakView/OneOmics or
#' OpenSWATH formats.
#'
#' @param SRL.df Data frame of an SRL
#' @param SRL.filepath The directory and filename of the SRL to be exported
#'
#' @examples
#' data(rat_srl_example)
#' exportSRL(rat_srl_example, "new-srl.txt")
#'
#' ## Entire Workflow
#' fasta <- system.file("extdata",
#'                      "human_proteome_example.fasta",
#'                      package = "dialects")
#' human_proteome_example <- import.fasta(fasta)
#' human_digest_example <- digest.fasta(human_proteome_example)
#' srl_pv <- system.file("extdata",
#'                       "rat_srl_example.txt",
#'                       package = "dialects")
#' rat_srl_example <- import.srl(srl_pv, SRL.format = "peakview")
#' human_from_rat <- convert.species(human_digest_example, rat_srl_example)
#' exportSRL(human_from_rat, "human_from_rat_srl.txt")
#'
#'
#' @author Madeleine J Otway \email{motway@@cmri.org.au}
#'
#' @seealso For functions require to convert SRL before exporting SRL, see:
#' \code{\link[dialects]{import.fasta}}, \code{\link{digest.fasta}},
#' \code{\link{import.srl}}, \code{\link{convert.species}}
#'
#' @importFrom utils write.table
#' @export exportSRL
#'
#'
#'
exportSRL <- function(SRL.df, SRL.filepath){

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
              quote = FALSE,
              sep = "\t",
              na = "",
              row.names = FALSE,
              col.names = TRUE)
  }

  if(endsWith(filepath.test, ".csv")) {
    write.table(SRL.df,
                file = SRL.filepath,
                quote = FALSE,
                sep = ",",
                na = "",
                row.names = FALSE,
                col.names = TRUE)
  }

  options(warn = oldw)

  return(print("Output complete"))
}
