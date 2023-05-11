\name{trait_data}
\alias{trait_data}
\docType{data}
\title{Trait data of 929  species
}
\description{ The trait data (SLA, ARNODE, LOGCAHN, LOGCADN, VEGPROP) of 929 relevant species, including the unique four-three species code for each species. The life history (lifehist) of each species is also included - it is required for understanding species inclusion in model 3
}

\format{
  A data frame with 929 observations on the following 9 variables.
  \describe{
   \item{\code{FlEurNo}}{column containing the Flora Europaea number (numbers >200000 have been created for species not in Flora Europaea)}
    \item{\code{Species}}{column containing the full Latin name}
    \item{\code{species.code}}{column containing the four-three species code}
    \item{\code{SLA}}{mean specific leaf area (mm\eqn{^2}/mg)}
    \item{\code{ARNODE}}{Leaf area (mm\eqn{^2}) per node/leaf thickness (mm)}
    \item{\code{LOGCANH}}{maximum canopy height (mm) converted to a log scale (base 2)}
    \item{\code{LOGCAND}}{maximum canopy diameter (mm) converted to a log scale (base 2) }
    \item{\code{VEGPROP}}{vegetative propagation (1 = yes, 0 = no)}
    \item{\code{LIFEHIST}}{life history of the species - annual, biannual, perennial}
  }
}
\details{ The data verion is 2021
}
\source{ORA DOI
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{References to submited paper, data paper and ORA website/github
}

\keyword{datasets}
