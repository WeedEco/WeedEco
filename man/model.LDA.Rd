\name{model.LDA}
\alias{model.LDA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Linear Discriminant Analysis of Functional Attributes of Archaeological Weed Seeds
%%  ~~function to do ... ~~
}
\description{This function conducts linear discriminant analysis using one of three included modern models to classify archaeobotanical samples. The function provides the classification of the input data (class), as well as the discriminant scores of each sample (LD1) and the posterior probability 1 and 2 (posterior.1 and posterior.2)
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
model.LDA(model,x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{ Either "model1", "model2" or "model3" (also 1,2 or 3). This argument allows the choice between three modern models against which the archaeological dataset is classified}
   \item{x}{The archaeological data that will be compared against the model. Note data must be labelled with specific headers - see below
%%     ~~Describe \code{x} here~~
}
}
\details{ model.LDA requires the input dataset to have specific column names. The columns providing the values for the different functional attributes must be spelt as follows: SLA, ARNODE, LOGCAHN, LOGCADN, FLOWPER, VEGPROP
%%  ~~ If necessary, more details than the description above ~~
}
\value{ A table containing the samples classification, the posterior.1 and posterior.2 value (the posterior probabilities of the two linear discriminant functions as per Venables and Ripley 2003 and the MASS package), the linear discriminant score (LD1), and the input functional attributes of data x
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{model 1 - see Bogaard \emph{et al.} 2016 "Combining functional weed ecology and crop stable isotope ratios to identify cultivation intensity: a comparison of cereal production regimes in Haute Provence, France and Asturias, Spain" \emph{Vegetation History and Archaeobotany} 25, 57-73

model 2 - see Bogaard \emph{et al.} 2018 "From traditional Farming in Morocco to Early Urban Agroecology in Northern Mesopotamia: Combining Present-day Arable Weed Surveys and crop isotope analysis to reconstruct past Agrosystems in (Semi-)arid Regions" \emph{Environmental Archaeology} 23, 303-322

model 3 - see Bogaard et al 2022 "????"
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
##Create random dataset for examples
SLA<-runif(40, min=20, max=30)
ARNODE<-runif(40, min=3000, max=22000)
LOGCAHN<-runif(40, min=5.5, max=6.5)
LOGCADN<-runif(40, min=5.5, max=7)
FLOWPER<-runif(40, min=3.5, max=7)
VEGPROP<-runif(40, min=0.15, max=0.8)
Study<-sample(1:3, 40, replace=T)
data<-as.data.frame(cbind(SLA,ARNODE,LOGCAHN,LOGCADN,FLOWPER,VEGPROP,Study))
## Use of model 1 -  modern data from Asturias, Spain and Haute Provence, France - see Bogaard et al 2016
model<-model.LDA("model1",data)
model<-model.LDA(1,data)

##Use of model 2 - modern data from Evvia, Greece; Asturias, Spain; Haute Provence, France; and oases and rain fed terraces in Morocco - see Bogaard et al 2018
model<-model.LDA("model2",data)
model<-model.LDA(2,data)

##Use of model 3 - modern data from Highgrove and Laxton, UK - see Bogaard et al ???
model<-model.LDA("model3",data)
model<-model.LDA(3,data)
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
