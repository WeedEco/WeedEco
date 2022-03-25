\name{plot4}
\alias{plot4}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plotting function to plot just the archaeological samples' discriminant scores and model's centroids
%%  ~~function to do ... ~~
}
\description{ A function which plots the archaeological samples discriminant scores from following \code{\link[FIBS]{model.LDA}} in comparation with the chosen modern model's centroids
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot4(model, x,...)

plot4(model, x, xlims = NULLL, ticks =NULL, col1 = NULL, col3 =NULL, pch1 =NULL,pch3=NULL, compact=NULL, priority=NULL, lines=F, site="samples")
## Defaults
plot3 (model,
        x,
        xlims= NULL,
        ticks=NULL,
        col1 = "black",
        col3= "black",
        pch1 = 1,
        pch2= 2,
        pch3= 0,
        compact = c (TRUE, FALSE)
        priority = c( "ascending", "descending", "density", "random", "none"),
        lines=c (TRUE, FALSE),
        site = "samples")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{ either "model1", "model3" or "model3" - following \code{\link[FIBS]{model.LDA}}
%%     ~~Describe \code{model} here~~
}
  \item{x}{the LD1 column from the results dataframe of \code{\link[FIBS]{model.LDA}}
%%     ~~Describe \code{x} here~~
}
  \item{xlims}{ the x limits
%%     ~~Describe \code{xlims} here~~
}
  \item{ticks}{ the location of the x axis labels
%%     ~~Describe \code{ticks} here~~
}
  \item{col1}{ the colour of the centroids of the selected model
%%     ~~Describe \code{col1} here~~
}
  \item{col3}{ the colour of the archaeological samples
%%     ~~Describe \code{col3} here~~
}
  \item{pch1}{the symbol of the the model's centroids
%%     ~~Describe \code{pch1} here~~
}
  \item{pch3}{the symbol of the archaeological samples
%%     ~~Describe \code{pch3} here~~
}
  \item{compact}{follows the \code{\link[beeswarm]{swarmy}} from the beeswarm package
%%     ~~Describe \code{compact} here~~
}
  \item{priority}{follows the \code{\link[beeswarm]{swarmy}} function relating to the method of point layout
%%     ~~Describe \code{priority} here~~
}
\item{lines}{ adds lines from group centroid to x-axis
%%     ~~Describe \code{priority} here~~
}
\item{site}{ name of the archaeological data to appear on legend
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

model 2 - see Bogaard et al 2018 " From traditional Farming in Morocco to Early Urban Agroecology in Northern Mesopotamia: Combining Present-day Arable Weed Surveys and crop isotope analysis to reconstruct past Agrosystems in (Semi-)arid Regions" \emph{Environmental Archaeology} 23, 303-322

model 3 - see Bogaard et al 2022 " ????"
%% ~put references to the literature/web site here ~
}
\author{Elizabeth Stroud
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link[FIBS]{model.LDA}}, \code{\link[FIBS]{plot3}}, \code{\link[FIBS]{plot5}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##example dataset
LD1<-runif(40, min=-6.6, max=6)
Study<-sample(1:3, 40, replace=T)
data<-data.frame(Study,LD1)

#If using defaults

plot4( "model1", data$LD1)
plot4( 1, data$LD1)
## Use of model1 -  modern data from Asturias, Spain and Haute Provence, France - see Boggard et al 2016
plot4("model1",data$LD1,xlim=c(-7,7),ticks=-7:7, col1="green",col3="purple", pch1=17,  pch3=19, compact =F, priority = "ascending", lines=T, site="example")

##Use of model2 - modern data from Evvia, Greece; Asturias, Spain; Haute Provence, France; and oases and rain fed terraces in Morocco - see Bogaard et al 2018
plot4("model2",data$LD1,xlim=c(-7,7),ticks=-7:7, col1="green", col3="purple", pch1=17, pch3=19, compact =F, priority = "ascending", lines=T, site= "example")

##Different  priority options using "ascending", "descending", "density", "random","none"
par(mfrow=c(3,2),
    oma=c(2,0,0,0),
    mar=c(0,0,0,0))
plot4(1,data$LD1, compact =F, priority = "ascending")
mtext("ascending", 3,-2)
plot4(1,data$LD1, compact =F, priority = "descending")
mtext("descending", 3, -2)
plot4(1,data$LD1,compact =F, priority = "density")
mtext("density",3, -2)
plot4(1,data$LD1, compact =F, priority = "random")
mtext("random",3,-2)
plot4(1,data$LD1, compact =F, priority = "none")
mtext("none",3, -2)

## options with compact T and F using ascending and descending priority
par(mfrow=c(2,2),
    oma=c(2,0,0,0),
    mar=c(0,0,0,0))
plot4(1,data$LD1, compact =F, priority = "ascending")
mtext("ascending, TRUE", 3,-3)
plot4(1,data$LD1, compact =T, priority = "ascending")
mtext("ascending, FALSE", 3,-3)
plot4(1,data$LD1,compact =F, priority = "descending")
mtext("descending, TRUE", 3,-3)
plot4(1,data$LD1, compact =T, priority = "descending")
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
