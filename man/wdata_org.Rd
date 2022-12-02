\name{wdata_org}
\alias{wdata_org}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for manipulating archaeobotanical scoresheets and obtaining functional trait data for \code{\link[WeedModels]{wmodel.LDA}}
%%  ~~function to do ... ~~
}
\description{Changes archaeobotanical data into presence/absence data and extracts the functional data for the listed species, and calculates the average of each functional attribute for each sample.
}
\usage{
wdata_org(abotdata, samples, codes,codename, model, fl_pr, sp_av, vg_pr)
## Defaults
wdata_org(abotdata, samples, codes,codename, model, fl_pr=NULL, sp_av=NULL, vg_pr=NULL)

}

\arguments{
  \item{abotdata}{The archaeobotanical data set. Data should be arranged with each sample as a column and each species as rows. One column needs to contain the species codes of the taxa (see X for list of taxa and corresponding codes).

}
\item{samples}{The column which the sample data starts

}
\item{codes}{The column which contains the species codes
}
\item{codename}{The name of the column containing the species codes

}
\item{model}{The model being used; 1,2 or 3 - this will determine which functional attribute are used
}

\item{fl_pr}{The flowering period data for the species (needs to be set up as two columns: one of species codes and one of flowering data labelled FLOWPER). This is only required for model 1 and model 3
}
\item{sp_av}{The averaged species data of composite taxa
}
\item{vg_pr}{Modified vegetative proprogation values - all values VEGPROP values need to be re-entered if this argument is used

}
}
\details{

}
\value{

}
\references{
}
\author{Elizabeth Stroud
}
\note{
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Random data
species<-c("Chenopodium album" , "Anthemis cotula", "Brassica rapa ssp campestris",
"Raphanus raphanistrum", "Agrostemma githago" , "Poa annua" )
code<-c("chenalb", "anthcot", "brasrap","raphrap","agrogit", "poa_ann")
s.1246<-sample(1:3, 6, replace=T)
s.46178<-sample(1:5, 6, replace=T)
s.1<-sample(0:8, 6, replace=T)
s.23<-sample(0:3, 6, replace=T)
s.987<-sample(3:9, 6, replace=T)
dataset<-data.frame(species,code,s.1246,s.46178,s.1,s.23,s.987)
code<-c("chenalb", "anthcot", "brasrap","raphrap","agrogit", "poa_ann")
FLOWPER<-sample(3:9, 6, replace=T)
x<-data.frame(code,FLOWPER)

## Use
results<-wdata_org(dataset, samples=3, codes=2, codename="code", model=1, fl_pr=x)
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
