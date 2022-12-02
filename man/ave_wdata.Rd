\name{ave_wdata}
\alias{ave_wdata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to average the functional trait data of a group of species
%%  ~~function to do ... ~~
}
\description{This function averages the functional trait data (SLA, ARNODE, LOGCANH, LOGCAND, VEGPRO) of entered species. This should be used to average data of taxa whose identification is not to species level.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ave_wdata(newname, species1, species2, species3 = NULL, species4 = NULL, species5 =
NULL, species6 = NULL, species7 = NULL, species8 = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{newname}{The code of the composite species to be created
%%     ~~Describe \code{newname} here~~
}
  \item{species1}{The four-three code of the first species to be averaged
%%     ~~Describe \code{species1} here~~
}
  \item{species2}{The four-three code of the second species to be averaged
%%     ~~Describe \code{species2} here~~
}
  \item{species3}{The four-three code of the third species to be averaged
%%     ~~Describe \code{species3} here~~
}
  \item{species4}{The four-three code of the fourth species to be averaged
%%     ~~Describe \code{species4} here~~
}
}
\details{It is highly recommended that species are only averaged if their functional attributes are similar. MORE
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

av_sp<-ave_wdata("bromahs", "bromarv","bromhor", "bromsec")
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
