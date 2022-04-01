\name{plot3}
\alias{plot3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plotting function to display functional ecology model centroids, model and the archaeological samples' linear discriminant scores from \code{\link[FIBS]{model.LDA}}
%%  ~~function to do ... ~~
}
\description{A function which plots the linear discriminant scores of the modern functional ecology models from \code{\link[FIBS]{model.LDA}}, their group centroids and the archaeological data’s discriminant scores.

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot3(model, x, ...)

plot3(model, x, xlims = NULLL, ticks =NULL, col1 = NULL, col2=NULL, col3 =NULL, pch1 =NULL, pch2= NULL, pch3=NULL, compact=NULL, priority=NULL, lines=F,legend =F, site="samples")
## Defaults
plot3 (model,
        x,
        xlim s= NULL,
        ticks = NULL,
        col1 = "black",
        col2 = "black",
        col3 = "black",
        pch1 = 1,
        pch2 = 2,
        pch3 = 0,
        compact =  FALSE
        priority = "density",
        lines = FALSE,
        legend = FALSE,
        site = "samples")

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{the modern model to be plotted – either ‘model1’, ‘model2', or 'model3' - use the model against which the archaeological samples have a been classified against, using \code{\link[FIBS]{model.LDA}}.
%%     ~~Describe \code{model} here~~
}
  \item{x}{the column x from the data frame which contains the linear discriminant scores of the archaeological data - if using data from \code{\link[FIBS]{model.LDA}}, such column will be called LD1
%%     ~~Describe \code{x} here~~
}
  \item{xlims}{the limits of the x axis
%%     ~~Describe \code{xlims} here~~
}
  \item{ticks}{ the location of the x axis labels along the x axis
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
  \item{pch1}{the symbol of the modern model's centroids - must be between 0 and 2
%%     ~~Describe \code{pch1} here~~
}
  \item{pch2}{the symbol of the modern model's datapoints - must be between 0 and 2
%%     ~~Describe \code{pch2} here~~
}
  \item{pch3}{the symbol of the archaeological datapoints
%%     ~~Describe \code{pch3} here~~
}
  \item{compact}{follows \code{\link[beeswarm]{swarmy}} from the beeswarm package
%%     ~~Describe \code{compact} here~~
}
  \item{priority}{follows the \code{\link[beeswarm]{swarmy}} function relating to the method of point layout
%%     ~~Describe \code{priority} here~~
}
 \item{lines}{add lines from group centroid to x-axis
%%     ~~Describe \code{priority} here~~
}
\item{legend}{adds a legend showing the symbols used to denote group centroids and archaeological samples. Options are "right" (located on the right- hand side), "left" (located on the left-hand side of the graph), and "split" (located on the right-hand side, with labels aligning with the graphed data)
%%     ~~Describe \code{priority} here~~
}
\item{site}{add labels to the archaeological data
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
\references{model 1 - see Bogaard et al 2016 "Combining functional weed ecology and crop stable isotope ratios to identify cultivation intensity: a comparison of cereal production regimes in Haute Provence, France and Asturias, Spain" \emph{Vegetation History and Archaeobotany} 25, 57-73

model 2 - see Bogaard et al 2018 "From traditional Farming in Morocco to Early Urban Agroecology in Northern Mesopotamia: Combining Present-day Arable Weed Surveys and crop isotope analysis to reconstruct past Agrosystems in (Semi-)arid Regions" \emph{Environmental Archaeology} 23, 303-322

model 3 - see Bogaard et al 2022 " ????"
%% ~put references to the literature/web site here ~
}
\author{ Elizabeth Stroud
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link[FIBS]{model.LDA}}, \code{\link[FIBS]{plot4}}, \code{\link[FIBS]{plot5}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##example dataset
LD1<-runif(40, min=-6.6, max=6)
Study<-sample(1:3, 40, replace=T)
data<-data.frame(Study,LD1)

##Usage with defaults
plot3("model1", data$LD1)
plot3("model2", data$LD1)
plot3("model3", data$LD1)

## Use of model 1 - modern data from Asturias, Spain and Haute Provence, France - see Bogaard \emph{et al.} 2016

plot3("model1",data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=1, pch2=2, pch3=19, compact =F, priority = "ascending", lines=T, site="example")

##Use of model2 - modern data from Evvia, Greece; Asturias, Spain; Haute Provence, France; and oases and rain fed terraces in Morocco - see Bogaard \emph{et al.} 2018

plot3("model2", data$LD1, xlims=c(-7,7),ticks=-7:7, col1="green",  col2="red",col3="purple", pch1=1, pch2=2,pch3=19, compact =F, priority = "ascending", lines=T, site="example")

##Use of model3 - modern data from Highgrove and Laxton, UK- see Bogaard \emph{et al.} 2022

plot3("model3", data$LD1, xlims=c(-7,7),ticks=-7:7, col1="green",  col2="red",col3="purple", pch1=1,pch2=2, pch3=19, compact =F, priority = "ascending", lines=T, site="example")


##Different priority options using "ascending", "descending", "density", "random","none"
par(mfrow=c(3,2),
    oma=c(0,0,0,0),
    mar=c(0,0,0,0))
plot3(1,data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=1, pch2=2, pch3=19, compact =F, priority = "ascending")
mtext("ascending", 3,-1.5)
plot3(1,data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=1, pch2=2, pch3=19, compact =F, priority = "descending")
mtext("descending", 3, -1.5)
plot3(1,data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=1, pch2=2, pch3=19, compact =F, priority = "density")
mtext("density",3, -1.5)
plot3(1,data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=1, pch2=2, pch3=19, compact =F, priority = "random")
mtext("random",3,-1.5)
plot3(1,data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=1, pch2=2, pch3=19, compact =F, priority = "none")
mtext("none",3, -1.5)

## options with compact T and F using ascending and descending priority
par(mfrow=c(2,2),
    oma=c(0,0,0,0),
    mar=c(0,0,0,0))
plot3(1,data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=1, pch2=2, pch3=19, compact =F, priority = "ascending")
mtext("ascending, TRUE", 3,-1.5)
plot3(1,data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=1, pch2=2, pch3=19, compact =T, priority = "ascending")
mtext("ascending, FALSE", 3,-1.5)
plot3(1,data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=1, pch2=2, pch3=19, compact =F, priority = "descending")
mtext("descending, TRUE", 3,-1.5)
plot3(1,data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=1, pch2=2, pch3=19, compact =T, priority = "descending")
mtext("descending, FALSE", 3,-1.5)

## options with different colours and symbols for different archaeological sites
plot3("model1", data$LD1, col3=data$Study,pch3=data$Study, site=c("Site 1","Site 2","Site 3"))

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
