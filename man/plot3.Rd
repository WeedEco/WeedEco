\name{plot3}
\alias{plot3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plotting the model centroids, model and sample linear discriminant scores
%%  ~~function to do ... ~~
}
\description{A which plots the discriminant scores of the modern model from model.LDA{FIBS}, its groups’ centroids and the archaeological data’s discriminant scores.


%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot3(model, x, xlims, ticks, col1, col2, col3, pch1, pch2, pch3, compact, priority)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{the modern model to be plotted – either ‘temperate’ or ‘arid’
%%     ~~Describe \code{model} here~~
}
  \item{x}{the column x from the data frame which contains the linear discriminant scores of the archaeological data
%%     ~~Describe \code{x} here~~
}
  \item{xlims}{the limits of the x axis
%%     ~~Describe \code{xlims} here~~
}
  \item{ticks}{ the location of the x axis labels
%%     ~~Describe \code{ticks} here~~
}
  \item{col1}{the colour of the modern model's centroids
%%     ~~Describe \code{col1} here~~
}
  \item{col2}{the colour of the modern model's datapoints
%%     ~~Describe \code{col2} here~~
}
  \item{col3}{the colour of the archaeolohical datapoints
%%     ~~Describe \code{col3} here~~
}
  \item{pch1}{the symbol of the modern model's centroids
%%     ~~Describe \code{pch1} here~~
}
  \item{pch2}{the symbol of the modern model's datapoints- must be between 0 and 2
%%     ~~Describe \code{pch2} here~~
}
  \item{pch3}{the symbol of the archaeological datapoints
%%     ~~Describe \code{pch3} here~~
}
  \item{compact}{ follows swarmy {beeswarm} compact variable
%%     ~~Describe \code{compact} here~~
}
  \item{priority}{ follows swarmy {beeswarm} priority regarding the method of point layout
%%     ~~Describe \code{priority} here~~
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
%% ~put references to the literature/web site here ~
}
\author{
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
##example dataset
LD1<-runif(40, min=-6.6, max=6)
data<-data.frame(LD1)

##If using with defaults
plot3("temperate", data$LD1)

##If using the temperate model:
plot3("temperate",data$LD1,xlim=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=17, pch2=18, pch3=19, compact =F, priority = "ascending")

## arid model
plot3("arid",data$LD1,xlim=c(-7,7),ticks=-7:7, col1="green",  col2="red",col3="purple", pch1=17, pch2=18,pch3=19, compact =F, priority = "ascending")

##Different  prioriy options using "ascending", "descending", "density", "random","none"
par(mfrow=c(3,2),
    oma=c(0,0,0,0),
    mar=c(0,0,0,0))
plot3("temperate",data$LD1,xlim=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=17, pch2=18, pch3=19, compact =F, priority = "ascending")
mtext("ascending", 3,-1.5)
plot3("temperate",data$LD1,xlim=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=17, pch2=18, pch3=19, compact =F, priority = "descending")
mtext("decending", 3, -1.5)
plot3("temperate",data$LD1,xlim=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=17, pch2=18, pch3=19, compact =F, priority = "density")
mtext("density",3, -1.5)
plot3("temperate",data$LD1,xlim=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=17, pch2=18, pch3=19, compact =F, priority = "random")
mtext("random",3,-1.5)
plot3("temperate",data$LD1,xlim=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=17, pch2=18, pch3=19, compact =F, priority = "none")
mtext("none",3, -1.5)

## options with compact T and F using ascending and decending priority
par(mfrow=c(2,2),
    oma=c(0,0,0,0),
    mar=c(0,0,0,0))
plot3("temperate",data$LD1,xlim=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=17, pch2=18, pch3=19, compact =F, priority = "ascending")
mtext("ascending, TRUE", 3,-1.5)
plot3("temperate",data$LD1,xlim=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=17, pch2=18, pch3=19, compact =T, priority = "ascending")
mtext("ascending, FALSE", 3,-1.5)
plot3("temperate",data$LD1,xlim=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=17, pch2=18, pch3=19, compact =F, priority = "descending")
mtext("descending, TRUE", 3,-1.5)
plot3("temperate",data$LD1,xlim=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=17, pch2=18, pch3=19, compact =T, priority = "descending")
mtext("descending, FALSE", 3,-1.5)

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
