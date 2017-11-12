\name{digest.fasta}
\alias{digest.fasta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{digest.fasta
%%  ~~function to do ... ~~
}
\description{
This function performs an in-silico trypsin digestion on a data frame from a protein sequence fasta file
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
digest.fasta(fasta.df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fasta.df}{
  Data frame generated from dialect::import.fasta
%%     ~~Describe \code{x} here~~
}
}
\details{
Data frame must have the vector names: accession, name, sequence
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a data frame of the peptide sequences of the digested proteins.
}
\author{
Madeleine J Otway
%%  ~~who you are~~
}
\note{
Removes all non-unique peptides.
Removes all peptides less than 5 and greater than 52 amino acids.
%%  ~~further notes~~
}

\seealso{
import.fasta
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
