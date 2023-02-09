\name{wmodel.LDA}
\alias{wmodel.LDA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Linear discriminant analysis based on functional ecological attributes of weed species

}
\description{This function conducts linear discriminant analysis classification using one of three modern models (i.e. sets of discriminated modern arable fields) to classify unknown cases on the basis of relevant functional ecological attributes of weed species. The function provides the classification of each sample (class), as well as the discriminant scores of each sample (LD1) and the posterior probability 1 and 2 (posterior.1 and posterior.2)
}
\usage{
wmodel.LDA(model,x)
}

\arguments{
  \item{model}{ Either "model1", "model2" or "model3" (also 1, 2 or 3). This argument allows the choice between three modern models against which the unknown cases are classified}
   \item{x}{The data that will be compared against the model. Note data must be labelled with specific headers - see below.
%%     ~~Describe \code{x} here~~
}
}
\details{wmodel.LDA requires the input dataset to have specific column names. The columns providing the values of the functional ecological attributes must be spelled as follows: SLA, ARNODE, LOGCANH, LOGCAND, FLOWPER, VEGPROP.
%%  ~~ If necessary, more details than the description above ~~
}
\value{ The function prints an output table containing the samples classification, the posterior probabilities of the two linear discriminant function classifications as per Venables and Ripley (2003) and the MASS package, the linear discriminant function score (LD1*). For full details the output should be saved as a data frame. The full details show both the standardised and unstandarised results. Note that for comparibility with previous data anaylised in SPSS the columns highlighted with a * should be used.

 \item{CLASS_std*}{The standardised classification of the data as either a 1 or 2 - this classifies the samples with priors of 50:50. This column reflects SPSS outputs}
 \item{Prob.1_std*}{The standarised posterior probability of being classed as 1 shown in CLASS_std*}
 \item{Prob.2_std*}{The standarised posterior probability of being classed as 2 shown in CLASS_std*}
 \item{Ld1_std}{The standarised linear discriminant scores - this is not used in any graphing function}
 \item{Class}{The unstandarised classification of the data as either a 1 or 2 - this classifies the samples with priors based on the propotions of the model entered}
 \item{Prob.1}{The unstandarised posterior probability of being classed as 1 shown in Class }
 \item{Prob.2}{The unstandarised posterior probability of being classed as 2 shown in Class}
 \item{LD1*}{The unstandarised linear discriminant scores. This column is used in the graphing functions and reflects SPSS outputs}

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
\examples{
##Create random dataset for examples

SLA<-runif(40, min=20, max=30)
ARNODE<-runif(40, min=3000, max=22000)
LOGCANH<-runif(40, min=5.5, max=6.5)
LOGCAND<-runif(40, min=5.5, max=7)
FLOWPER<-runif(40, min=3.5, max=7)
VEGPROP<-runif(40, min=0.15, max=0.8)
Study<-sample(1:3, 40, replace=T)
data<-as.data.frame(cbind(SLA,ARNODE,LOGCANH,LOGCAND,FLOWPER,VEGPROP,Study))

## Use of model 1 -  modern data from Asturias, Spain and Haute Provence, France
#- see Bogaard et al. 2016

model<-wmodel.LDA("model1",data)
model<-wmodel.LDA(1,data)

##Use of model 2 - modern data from Evvia, Greece; Asturias, Spain; Haute Provence,
#France; and oases and rain fed terraces in Morocco - see Bogaard et al. 2018

model<-wmodel.LDA("model2",data)
model<-wmodel.LDA(2,data)

##Use of model 3 - modern data from Highgrove and Laxton, UK - see Bogaard et al.2022

model<-wmodel.LDA("model3",data)
model<-wmodel.LDA(3,data)
}

