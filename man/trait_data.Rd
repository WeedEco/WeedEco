\name{trait_data}
\alias{trait_data}
\docType{data}
\title{Trait data of 929  species
%%   ~~ data name/kind ... ~~
}
\description{ The trait data (SLA, ARNODE, LOGCAHN, LOGCADN, VEGPROP) of 929 relevant species, including the unique four-three species code for each species. The life history (lifehist) of each species is also included - it is required for understanding species inclusion in model 3
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}

\format{
  A data frame with 929 observations on the following 8 variables.
  \describe{
    \item{\code{Species}}{column containing the full Latin name}
    \item{\code{species.code}}{column containing the four-three species code}
    \item{\code{SLA}}{mean specific leaf area}
    \item{\code{ARNODE}}{leaf area:leaves per node}
    \item{\code{LOGCANH}}{maximum canopy height converted to a log scale (base 2)}
    \item{\code{LOGCAND}}{maximum canopy diameter converted to a log scale (base 2) }
    \item{\code{VEGPROP}}{vegetative propagation - 1 = yes 0 = no}
    \item{\code{lifehist}}{life history of the species - annual, biannual, perennial}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{


}
\keyword{datasets}
