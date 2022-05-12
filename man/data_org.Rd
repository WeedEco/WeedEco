\name{data_org}
\alias{data_org}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for manipulating archabotanical scoresheets and obtaining FIBS data for LDA
%%  ~~function to do ... ~~
}
\description{Changes archaebotanical data into presence absences data and extracts the functional data for the listed species, and calcuates the SLA, ARNODE,etc for each sample (average).
}
\usage{
data_org(abotdata,samples=6, codes=3, x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{abotdata}{input archaebotanical data set which is set out with each sample as a column, and the species as rows. One column needs to contain the FIBS codes of the taxa (see X for list a taxa and codes).
%%     ~~Describe \code{x} here~~
}
\item{samples}{The column in which the sample data starts
%%     ~~Describe \code{x} here~~
}
\item{codes}{ The column which contains the FIBS codes
%%     ~~Describe \code{x} here~~
}
\item{x}{the flowering time data for the species (need to be set up as column of fibscodes and column labeled FLOWPER whcih contains the flowering period of the taxa)
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
