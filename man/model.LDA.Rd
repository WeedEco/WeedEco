\name{model.LDA}
\alias{model.LDA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Linear Discriminant Analysis based on Functional Ecological Attributes of Archaeological Weed Species
%%  ~~function to do ... ~~
}
\description{This function conducts linear discriminant analysis classification using one of three modern models (i.e. sets of discriminated modern arable fields) to classify archaeobotanical samples on the basis of relevant functional ecological attributes of weed species. The function provides the classification of each sample (class), as well as the discriminant scores of each sample (LD1) and the posterior probability 1 and 2 (posterior.1 and posterior.2)
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
model.LDA(model,x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{ Either "model1", "model2" or "model3" (also 1,2 or 3). This argument allows the choice between three modern models against which the archaeobotanical dataset is classified}
   \item{x}{The archaeobotanical data that will be compared against the model. Note data must be labelled with specific headers - see below.
%%     ~~Describe \code{x} here~~
}
}
\details{ model.LDA requires the input dataset to have specific column names. The columns providing the values for the functional ecological attributes must be spelled as follows: SLA, ARNODE, LOGCAHN, LOGCADN, FLOWPER, VEGPROP.
%%  ~~ If necessary, more details than the description above ~~
}
\value{ A table containing the samples classification, the posterior.1 and posterior.2 value (the posterior probabilities of the two linear discriminant function classifications as per Venables and Ripley 2003 and the MASS package), the linear discriminant function score (LD1), and the input functional ecological attributes of data x.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{model 1 - see Bogaard, A., Hodgson, J., Nitsch, E., Jones, G., Styring, A., Diffey, C., Pouncett, J., Herbig, C., Charles, M., Ertuğ, F., Tugay, O. Filipović, D. and Fraser, R. (2016) 'Combining functional weed ecology and crop stable isotope ratios to identify cultivation intensity: a comparison of cereal production regimes in Haute Provence, France and Asturias, Spain' \emph{Vegetation History and Archaeobotany} 25, 57-73

model 2 - see Bogaard, A., Styring, A., Ater, M., Hmimsa, Y., Green, L., Stroud, E., Whitlam, J., Diffey, C., Nitsch, E., Charles, M., Jones, G. and Hodgson, J.2018 'From traditional farming in Morocco to Early Urban Agroecology in northern Mesopotamia: combining Present-day Arable Weed Surveys and crop isotope analysis to reconstruct past Agrosystems in (Semi-)arid Regions' \emph{Environmental Archaeology} 23, 303-322

model 3 - see Bogaard, A., Hodgson, J., Kropp, C., McKerracher, M. and Stroud, E. (in press 2022). ‘Lessons from Laxton, Highgrove and Lorsch: Building arable weed-based models for the investigation of early medieval agriculture in England’, in M. McKerracher and H. Hamerow (eds) \emph{New Perspectives on the Medieval ‘Agricultural Revolution’: Crop, Stock and Furrow} (Liverpool: Liverpool University Press).
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
