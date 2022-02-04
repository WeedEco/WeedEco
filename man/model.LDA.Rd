\name{model.LDA}
\alias{model.LDA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Linear Discriminant Analysis of Functional Attributes of Archaeological Weed Seeds
%%  ~~function to do ... ~~
}
\description{This function conducts linear discriminant analysis using one of two included modern models, which is then used to classify archaeobotanical samples into "high or low input".
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
model.LDA(model,x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{ Either 'temperate' or 'arid'. This argument allows the choice between two modern models against which the archaeological dataset is compared}
   \item{x}{The archaeological data which will be compared against the model. Note data must be set out is a specific way.
%%     ~~Describe \code{x} here~~
}
}
\details{ model.LDA requires the input dataset set up in a specific way. The columns providing the values for the different 5 functional attributes must be titled and ordered in the following way:SLA, ARNODE, LOGCAHN, LOGCADN, FLOWPER
%%  ~~ If necessary, more details than the description above ~~
}
\value{ A table containing the class the sample hass been assigned to, the posterior.1 and posterior.2 as per ????, the linear discriminant score (LD1), and the imput functional attributes of data x
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
\examples{model.LDA ('temperate', data)
##Create random dataset
SLA<-runif(40, min=20, max=30)
ARNODE<-runif(40, min=3000, max=22000)
LOGCAHN<-runif(40, min=5.5, max=6.5)
LOGCADN<-runif(40, min=5.5, max=7)
FLOWPER<-runif(40, min=3.5, max=7)
Study<-seq(1,40)
data<-as.data.frame(cbind(SLA,ARNODE,LOGCAHN,LOGCADN,FLOWPER,Study))

## Use the temperate model - for location with X rainfall or XX
model<-model.LDA('temperate',data)

##Use the arid model - for locations with X rainfall or XX
model<-model.LDA('arid',data)
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
