\name{plot4}
\alias{plot4}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plotting the samples and model centroids
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot4(model, df, x, xlims, ticks, col1, col3, pch1, pch3, compact, priority)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{ either 'temperate' or 'arid'
%%     ~~Describe \code{model} here~~
}
  \item{df}{ the dataframe containing the output of model.LDA
%%     ~~Describe \code{df} here~~
}
  \item{x}{the LD1 column from df
%%     ~~Describe \code{x} here~~
}
  \item{xlims}{ the x limits , !!!! not yet !!!!defaults to -7 to 7
%%     ~~Describe \code{xlims} here~~
}
  \item{ticks}{ the location of the x axis labels
%%     ~~Describe \code{ticks} here~~
}
  \item{col1}{ the colour of the centroids of the selected model
%%     ~~Describe \code{col1} here~~
}
  \item{col3}{ the colour of the archaeological data
%%     ~~Describe \code{col3} here~~
}
  \item{pch1}{the symbol of the the model's centroids
%%     ~~Describe \code{pch1} here~~
}
  \item{pch3}{the symbol of the archaeological data
%%     ~~Describe \code{pch3} here~~
}
  \item{compact}{follows swarmy function from the beeswarm package
%%     ~~Describe \code{compact} here~~
}
  \item{priority}{follows the swarmy function from the beeswarm package relating to the method of point layout
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
\references{NEED TO ADD SOMETHING
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
data<-data.frame(LD1)

##If using the temperate model:
plot4("temperate",data,data$LD1,xlim=c(-7,7),ticks=-7:7, col1="green",col3="purple", pch1=17,  pch3=19, compact =F, priority = "ascending")

## arid model
plot4("arid",data,data$LD1,xlim=c(-7,7),ticks=-7:7, col1="green", col3="purple", pch1=17, pch3=19, compact =F, priority = "ascending")

##Different  prioriy options using "ascending", "descending", "density", "random","none"
par(mfrow=c(3,2),
    oma=c(2,0,0,0),
    mar=c(0,0,0,0))
plot4("temperate",data,data$LD1,xlim=c(-7,7),ticks=-7:7, col1="green",col3="purple", pch1=17, pch3=19, compact =F, priority = "ascending")
mtext("ascending", 3,-2)
plot4("temperate",data,data$LD1,xlim=c(-7,7),ticks=-7:7, col1="green",col3="purple", pch1=17,pch3=19, compact =F, priority = "descending")
mtext("decending", 3, -2)
plot4("temperate",data,data$LD1,xlim=c(-7,7),ticks=-7:7, col1="green",col3="purple", pch1=17,  pch3=19, compact =F, priority = "density")
mtext("density",3, -2)
plot4("temperate",data,data$LD1,xlim=c(-7,7),ticks=-7:7, col1="green", col3="purple", pch1=17, pch3=19, compact =F, priority = "random")
mtext("random",3,-2)
plot4("temperate",data,data$LD1,xlim=c(-7,7),ticks=-7:7, col1="green",col3="purple", pch1=17, pch3=19, compact =F, priority = "none")
mtext("none",3, -2)

## options with compact T and F using ascending and decending priority
par(mfrow=c(2,2),
    oma=c(2,0,0,0),
    mar=c(0,0,0,0))
plot4("temperate",data,data$LD1,xlim=c(-7,7),ticks=-7:7, col1="green", col3="purple", pch1=17, pch3=19, compact =F, priority = "ascending")
mtext("ascending, TRUE", 3,-3)
plot4("temperate",data,data$LD1,xlim=c(-7,7),ticks=-7:7, col1="green", col3="purple", pch1=17, pch3=19, compact =T, priority = "ascending")
mtext("ascending, FALSE", 3,-3)
plot4("temperate",data,data$LD1,xlim=c(-7,7),ticks=-7:7, col1="green",col3="purple", pch1=17,  pch3=19, compact =F, priority = "descending")
mtext("descending, TRUE", 3,-3)
plot4("temperate",data,data$LD1,xlim=c(-7,7),ticks=-7:7, col1="green", col3="purple", pch1=17, pch3=19, compact =T, priority = "descending")
mtext("descending, FALSE", 3,-3)

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
