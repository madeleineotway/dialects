\name{convert.species}
\alias{convert.species}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convert between two species
%%  ~~function to do ... ~~
}
\description{
Converts the species in the spectral reference library to the species in the digested fasta data frame where full peptide identity occurs.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
convert.species(SRL.df, digest.df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SRL.df}{
  A data frame of the spectral reference library from dialects::import.srl
}
  \item{digest.df}{
  A data frame of the digested protein sequence from the desired species generated from dialects::digest.fasta
}
}
\value{
A data frame of the new spectral reference library with the peptides from the desired species.
}

\author{
Madeleine J Otway
%%  ~~who you are~~
}

\seealso{
import.fasta, digest.fasta, import.srl
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
