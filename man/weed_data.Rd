\name{weed_data}
\alias{weed_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A function to extract the functional attributes of named species from the trait database.
%%  ~~function to do ... ~~
}
\description{A function which extracts the functional attributes of named species \code{\link[WeedModels]{trait_data}}. This function extracts the SLA, ARNNODE, LOGCANH, LOGCAND and VEGPROP of a column of four-three coded species names.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
weed_data(x)

## Defaults
weed_data(x, trait="ALL")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The column of a dataframe containing the four-three species codes
%%     ~~Describe \code{x} here~~
}\item{trait}{The traits required. The default returns all traits, however indivaual traits can be returned by entering their names: SLA, ARNODE, LOGCAND,LOGCANH, VEGPROP
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{ \describe{
    \item{\code{species.code}}{column containing the four-three species code}
    \item{\code{SLA}}{mean specific leaf area}
    \item{\code{ARNODE}}{leaf area:leaves per node}
    \item{\code{LOGCANH}}{maximum canopy height converted to a log scale (base2)}
    \item{\code{LOGCAND}}{maximum canopy diameter converted to a log scale (base2) }
    \item{\code{VEGPRO}}{vegetative propagation - 1 = yes 0 = no}
    \item{\code{lifehist}}{life history of the species - annual, biannual, perennial}
    }
    }
\references{
%% ~put references to the literature/web site here ~
}
\author{Elizabeth Stroud
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Example data

species<-c("Chenopodium album" , "Anthemis cotula", "Brassica rapa ssp campestris",
"Raphanus raphanistrum", "Agrostemma githago" , "Poa annua" )
code<-c("chenalb", "anthcot", "brasrap","raphrap","agrogit", "poa_ann")
data<-as.data.frame(cbind(species, code))
## Usage
results<-weed_data(data$code)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
