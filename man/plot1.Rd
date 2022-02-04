\name{plot1}
\alias{plot1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{plot1
%%  ~~function to do ... ~~
}
\description{This function plots the resulting discriminant function scores from the model.LDA function. It plots the archaeological samples discriminant function scores against the model selected (arid vs temperate) and also plots the model’s group centroids
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot1(model, x, xlims, method, pch1, pch2, pch3, col1, col2, col3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{The model used to classify the data - either ‘temperate’ or ‘arid’}
  \item{x}{the column which contains the Linear discriminant scores (LD1) data of the archaeological samples from model.LDA}
\item{xlims}{The limits of the x axis}
\item{method}{Follows the beeswarm “method” – choices are swarm, center, hex or square}
\item{pch1}{Symbol of the centroids of the selected model}
\item{col1}{Colour of the centroids of the selected model}
\item{pch2}{Symbol of the selected model}
\item{col2}{Colour of the selected model}
\item{pch3}{Symbol of the archaeological data}
\item{col3}{Colour of the archaeological data}
}

\details{ This fuction uses the function Beeswarm from the package Beeswarm.
The col and pch follow base R documentation
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
##Randon data
LD1<-runif(40, min=-6.6, max=6)
data<-data.frame(LD1)

##If using the temperate model:
plot1("temperate", data$LD1,xlims = c(-7,7), method="swarm", pch1 = 17,pch2=19, pch3=18, col1 = "green", col2="black",col3="red")

##Different methods options using "swarm", "compactswarm", "center", "hex","square"

par(mfrow=c(3,2),
oma=c(0,0,0,0),
mar=c(0,0,1,1))
plot1("temperate", data$LD1,xlims = c(-7,7), method="swarm", pch1 = 17,pch2=19, pch3=18, col1 = "green", col2="black",col3="red")
mtext("Swarm", 3)
plot1("temperate", data$LD1,xlims = c(-7,7), method="compactswarm", pch1 = 17,pch2=19, pch3=18, col1 = "green", col2="black",col3="red")
mtext("compactswarm",3)
plot1("temperate", data$LD1,xlims = c(-7,7), method="center", pch1 = 17,pch2=19, pch3=18, col1 = "green", col2="black",col3="red")
mtext("center", 3)
plot1("temperate", data$LD1,xlims = c(-7,7), method="hex", pch1 = 17,pch2=19, pch3=18, col1 = "green", col2="black",col3="red")
mtext("hex",3)
plot1("temperate", data$LD1,xlims = c(-7,7), method="square", pch1 = 17,pch2=19, pch3=18, col1 = "green", col2="black",col3="red")
mtext("square")

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
