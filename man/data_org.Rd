\name{data_org}
\alias{data_org}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for manipulating archaeobotanical scoresheets and obtaining FIBS data for LDA
%%  ~~function to do ... ~~
}
\description{Changes archaeobotanical data into presence/absence data and extracts the functional data for the listed species, and calculates the average of the functional attributes for each sample.
}
\usage{
data_org(abotdata,samples, codes, model, x)
}

\arguments{
  \item{abotdata}{the archaeobotanical data set. Data should be arranged with each sample as a column and each species as rows. One column needs to contain the species codes of the taxa (see X for list a taxa and codes).

}
\item{samples}{The column which the sample data starts

}
\item{codes}{The column which contains the species codes

}
\item{model}{The model being used; 1,2 or 3 - this will determine which functional attribute are used

}
\item{x}{The flowering period data for the species (needs to be set up as two columns: one of species codes and one of flowering data labelled FLOWPER)

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
