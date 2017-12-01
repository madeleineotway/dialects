#' Tryptic digestion of human proteome
#'
#' This dataset contains an in silico tryptic digestion of 7 human proteins
#' (generated in \code{\link{digest.fasta}}). This tryptic digestion is for example
#' purposes only.
#'
#'
#'@format A data frame containing 501 rows and 3 variables.
#' \describe{
#'   \item{accession}{(character) - UniProt accession code for a protein}
#'   \item{name}{(character) - Name a protein (as defined by UniProt)}
#'   \item{sequence}{(character) - Sequence of a trypticly digested peptide}
#'   }
#'
#' @details Each row corresponds to a single trypticly digested peptide.
#' @source In silico digestion performed at the Children's Medical Research Intitute in 2017 by Madeleine J Otway.
#'
"human_digest_example"



#' Tryptic digestion of human proteome
#'
#' This dataset contains 7 human proteins (downloaded from \url{www.uniprot.org/}).
#' This dataset is for example purposes only.
#'
#'
#'@format A data frame containing 7 rows and 3 variables.
#' \describe{
#'   \item{accession}{(character) - UniProt accession code for a protein}
#'   \item{name}{(character) - Name a protein (as defined by UniProt)}
#'   \item{sequence}{(character) - Sequence of a protein}
#'   }
#'
#' @details Each row corresponds to a protein.
#' @source Proteome downloaded from \url{www.uniprot.org/} at the Children's Medical Research Intitute in 2017 by Madeleine J Otway.
#'
"human_proteome_example"



#' PeakView SRL
#'
#' This dataset contains an altered rat spectral reference library created in
#' PeakView. This SRL is for example purposes only.
#'
#'
#' @format A data frame containing 10 rows and 24 variables.
#' \describe{
#'   \item{Q1}{(numeric) - Mass of the peptide}
#'   \item{Q3}{(numeric) - Mass of the fragment}
#'   \item{RT_detected}{(character) - Retention time of the peptide}
#'   \item{protein_name}{(character) - Name of the protein}
#'   \item{isotype}{(logical) - }
#'   \item{relative_intensity}{(numeric) - Relative intensity of the fragment}
#'   \item{stripped_sequence}{(character) - Unmodified sequence of the peptide}
#'   \item{modification_sequence}{(character) - Modified sequence of the peptide}
#'   \item{prec_z}{(numeric) - Charge of the peptide}
#'   \item{frg_type}{(character) - Type of fragment, either "y" or "b"}
#'   \item{frg_z}{(integer) - Charge of the fragment}
#'   \item{frg_nr}{(integer) - Fragment number or where on the peptide backbone has the fragmentation occurred}
#'   \item{iRT}{(numeric) - }
#'   \item{uniprot_id}{(character) - Accession code as defined by UniProt}
#'   \item{score}{(logical) - }
#'   \item{decoy}{(character) - Whether or not this peptide is a decoy, either "True" or "False"}
#'   \item{prec_y}{(numeric) - }
#'   \item{confidence}{(numeric) - The confidence that the mass of the peptide and fragment belong to the peptide sequence, between 0 and 1}
#'   \item{shared}{(character) - Whether or not this peptide is shared with any other proteins, either "True" or "False"}
#'   \item{N}{(integer) - }
#'   \item{rank}{(logical) - }
#'   \item{mods}{(logical) - }
#'   \item{nterm}{(character) - }
#'   \item{cterm}{(character) - }
#'  }
#'
#' @details Each row corresponds to a MS/MS ion from a Spectral Reference Library. There is only one ion per peptide in this example.
#' @source Spectral reference library created at the Children's Medical Research Intitute in 2017 by Madeleine J Otway.
#'
"rat_srl_example"
