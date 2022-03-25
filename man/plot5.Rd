\name{plot5}
\alias{plot5}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plotting function for displaying the ouput of \code{\link[FIBS]{model.LDA}}, ploting the archaeological samples discriminant scores, the chosen model's centroids, and the model's discriminant scores with seperate symbols for the different modern study sites.
%%  ~~function to do ... ~~
}
\description{A function which plots the archaeological samples discriminant scores from following \code{\link[FIBS]{model.LDA}} in comparation with the chosen modern model's centroids, and the model's discriminant scores seperated according to modern study sites
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot5(model, x)

plot5(model, x, xlims = NULLL, ticks =NULL, col1 = NULL, col2=NULL, col3=NULL, pch1=NULL, pch2=NULL, pch3=NULL, compact=NULL, priority=NULL, lines=F, site= "Archaeological samples", legend=F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{the modern model to be plotted – either "model1", "model3" or "model3" following \code{\link[FIBS]{model.LDA}}
%%     ~~Describe \code{model} here~~
}
  \item{x}{the LD1 column from the results dataframe of \code{\link[FIBS]{model.LDA}}. Can also use linear discriminant score from other scores.
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
  \item{col3}{the colour of the archaeological datapoints
%%     ~~Describe \code{col3} here~~
}
  \item{pch1}{the symbol of the modern model's centroids- must be between 0 and 2
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
  \item{priority}{ follows \code{\link[beeswarm]{swarmy}} priority regarding the method of point layout
%%     ~~Describe \code{priority} here~~
}
 \item{lines}{ adds lines from group centroid to x-axis
%%     ~~Describe \code{priority} here~~
}
\item{site}{adds name of archaeological data
%%     ~~Describe \code{priority} here~~
}
\item{legend}{adds legend to the plot
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
##example dataset
LD1<-runif(40, min=-6.6, max=6)
Study<-sample(1:3, 40, replace=T)
data<-data.frame(Study,LD1)

##If using with defaults
plot5("model1", data$LD1)

## Use of model1 -  modern data from Asturias, Spain and Haute Provence, France - see Boggard et al 2016

plot5("model1",data$LD1,xlim=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=17, pch2=18, pch3=19, compact =F, priority = "ascending", lines=T)

##Use of model2 - modern data from Evvia, Greece; Asturias, Spain; Haute Provence, France; and oases and rain fed terraces in Morocco - see Bogaard et al 2018

plot3("model2",data$LD1,xlim=c(-7,7),ticks=-7:7, col1="green",  col2="red",col3="purple", pch1=17, pch2=18,pch3=19, compact =F, priority = "ascending", lines=T)

##Different  prioriy options using "ascending", "descending", "density", "random","none"
par(mfrow=c(3,2),
    oma=c(0,0,0,0),
    mar=c(0,0,0,0))
plot3(1,data$LD1,xlim=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=17, pch2=18, pch3=19, compact =F, priority = "ascending")
mtext("ascending", 3,-1.5)
plot3(1,data$LD1,xlim=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=17, pch2=18, pch3=19, compact =F, priority = "descending")
mtext("decending", 3, -1.5)
plot3(1,data$LD1,xlim=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=17, pch2=18, pch3=19, compact =F, priority = "density")
mtext("density",3, -1.5)
plot3(1,data$LD1,xlim=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=17, pch2=18, pch3=19, compact =F, priority = "random")
mtext("random",3,-1.5)
plot3(1,data$LD1,xlim=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=17, pch2=18, pch3=19, compact =F, priority = "none")
mtext("none",3, -1.5)

## options with compact T and F using ascending and decending priority
par(mfrow=c(2,2),
    oma=c(0,0,0,0),
    mar=c(0,0,0,0))
plot3(1,data$LD1,xlim=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=17, pch2=18, pch3=19, compact =F, priority = "ascending")
mtext("ascending, TRUE", 3,-1.5)
plot3(1,data$LD1,xlim=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=17, pch2=18, pch3=19, compact =T, priority = "ascending")
mtext("ascending, FALSE", 3,-1.5)
plot3(1,data$LD1,xlim=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=17, pch2=18, pch3=19, compact =F, priority = "descending")
mtext("descending, TRUE", 3,-1.5)
plot3(1,data$LD1,xlim=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=17, pch2=18, pch3=19, compact =T, priority = "descending")
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
