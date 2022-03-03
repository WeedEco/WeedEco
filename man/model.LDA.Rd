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
  \item{model}{ Either 'model1' or 'model2' (also 1 or 2). This argument allows the choice between two modern models against which the archaeological dataset is compared}
   \item{x}{The archaeological data which will be compared against the model. Note data must be set out is a specific way.
%%     ~~Describe \code{x} here~~
}
}
\details{ model.LDA requires the input dataset set up in a specific way. The columns providing the values for the different 5 functional attributes must be titled and ordered in the following way:SLA, ARNODE, LOGCAHN, LOGCADN, FLOWPER
%%  ~~ If necessary, more details than the description above ~~
}
\value{ A table containing the class the sample has been assigned to, the posterior.1 and posterior.2 value (the posterior probabilities of the two linear discriminant functions as per Venables and Ripley 2003 and the MASS package), the linear discriminant score (LD1), and the input functional attributes of data x
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
\examples{model.LDA ('model1', data)
##Create random dataset for medel 1
SLA<-runif(40, min=20, max=30)
ARNODE<-runif(40, min=3000, max=22000)
LOGCAHN<-runif(40, min=5.5, max=6.5)
LOGCADN<-runif(40, min=5.5, max=7)
FLOWPER<-runif(40, min=3.5, max=7)
Study<-seq(1,40)
data<-as.data.frame(cbind(SLA,ARNODE,LOGCAHN,LOGCADN,FLOWPER,Study))
##Create random dataset for model 2
SLA<-runif(40, min=20, max=30)
ARNODE<-runif(40, min=3000, max=22000)
LOGCAHN<-runif(40, min=5.5, max=6.5)
LOGCADN<-runif(40, min=5.5, max=7)
FLOWPER<-runif(40, min=3.5, max=7)
Study<-seq(1,40)
data<-as.data.frame(cbind(SLA,ARNODE,LOGCAHN,LOGCADN,FLOWPER,Study))
##Create random dataset for model 3
FLOWPER<-runif(40, min=3, max=6)
VEGPROP<-runif(40, min=0.15, max=0.8)
Study<-seq(1,40)
data<-as.data.frame(cbind(FLOWPER,VEGPROP,Study))

## Use of model1 -  modern data from Asturias, Spain and Haute Provence, France - see Boggard et al 2016
model<-model.LDA('model1',data)
model<-model.LDA(1,data)

##Use of model2 - modern data from Evvia, Greece; Asturias, Spain; Haute Provence, France; and oases and rain fed terraces in Morocco - see Bogaard et al 2018
model<-model.LDA('model2',data)
model<-model.LDA(2,data)
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
