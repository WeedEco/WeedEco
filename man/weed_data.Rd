\name{weed_data}
\alias{weed_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A function to extract the functional attributes of named species from the trait database.
%%  ~~function to do ... ~~
}
\description{A function which extracts the functional attributes of named species \code{\link[WeedEco]{trait_data}}. This function extracts the SLA, ARNNODE, LOGCANH, LOGCAND and VEGPROP of either four-three coded species names or Flora Europaea numbers. This function can also be used to extract the total list of Species and their 4_3 codes.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
weed_data(x=NULL, trait="all", WFO=FALSE)
}

\arguments{
  \item{x}{The four-three species code, Flora Europaea number or World Flora Online number. If this is left blank a list of species, Flora Europaea numbers, WFO numbers and 4_3 codes for the entire 929 species will be returned. Note that Flora Europaea numbers must be numeric and if using WFO numbers, the WFO aurgument must be set to TRUE.
}\item{trait}{The traits required. The default returns all traits, however individual traits can be returned by entering their names: SLA, ARNODE, LOGCAND,LOGCANH, VEGPROP
}\item{WFO}{TRUE or FALSE depending on whether x is the World Flora online numbers (TRUE) or not (FALSE)
}}

\value{ \describe{
    \item{\code{FlEurNo}}{the Flora Europaea number}
    \item{\code{WFO_code}}{the World Flora Online number}
    \item{\code{species.code}}{the species name}
    \item{\code{Species_authority}}{the species authority}
    \item{\code{Species}}{the Flora Europaea number}
    \item{\code{SLA}}{ mean specific leaf area (mm\eqn{^2}/mg)}
    \item{\code{ARNODE}}{Leaf area (mm\eqn{^2}) per node/leaf thickness (mm)}
    \item{\code{LOGCANH}}{maximum canopy height (mm) classified into a log scale (base2) bins}
    \item{\code{LOGCAND}}{maximum canopy diameter (mm) classified into a log scale (base2) bins}
    \item{\code{VEGPROP}}{vegetative propagation - 1 = yes 0 = no}
    \item{\code{lifehist}}{life history of the species - annual, biannual, perennial}
    }
    }
\references{<ORA reference>
%% ~put references to the literature/web site here ~
}
\author{Elizabeth Stroud
%%  ~~who you are~~
}

\examples{
## Example data
species<-c("Chenopodium album" , "Anthemis cotula", "Brassica rapa ssp campestris",
"Raphanus raphanistrum", "Agrostemma githago" , "Poa annua" )
code<-c("chenalb", "anthcot", "brasrap","raphrap","agrogit", "poa_ann")
data<-as.data.frame(cbind(species, code))
## Usage
results<-weed_data(data$code)
four_threecodes<-weed_data()
vegprop_only<-weed_data(data$code, trait="VEGPROP")
WFO_only<- weed_data("wfo-000856381", WFO=TRUE)
Flora_EU<- weed_data(41967)
}
