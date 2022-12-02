\name{wplot_phase}
\alias{wplot_phase}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function to plot the archaeobotanical samples' linear discriminant scores and model's centroids by phase or site
%%  ~~function to do ... ~~
}
\description{A function which plots the archaeobotanical samples linear discriminant scores from \code{\link[weedmodels]{wmodel.LDA}} compared to the chosen modern model's group centroids as stacked graphs depending on number of grouping variables ( i.e. sites or phase)
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
wplot_phase(model,x, pcolumn, site,gpch,gcol,gbg, g1,g2 ....)
}

\arguments{
  \item{model}{Either "model1", "model2" or "model3" - following \code{\link[weedmodels]{wmodel.LDA}}
}
  \item{x}{The LD1 column from the results dataframe of \code{\link[weedmodels]{wmodel.LDA}}
}
  \item{pcolumn}{the column with the phases of each sample
}
  \item{site}{Name of the archaeobotanical data to appear on legend
}
  \item{gpch}{symbol shape for split phase graph
}
  \item{gcol}{symbol shape for archaeobotanical samples in legend - use if multiple symbols are used in pch3
}
  \item{gbg}{symbol background colour - like bg in R basic
}
  \item{g1}{name of phase/site group 1
}
  \item{g2}{name of phase/site group 2
}
  \item{g3}{name of phase/site group 3
}
  \item{g4}{name of phase/site group 4
}
  \item{g5}{name of phase/site group 5
}
  \item{ylims}{limits of the y axis (THIS IS REVERSED _ SMALL NUMBERS = big axis range)
}
  \item{xlims}{The x limits
}
  \item{ticks}{The location of the x axis labels
}
  \item{compact}{Follows \code{\link[beeswarm]{swarmy}} from the beeswarm package
}
  \item{priority}{Follows the \code{\link[beeswarm]{swarmy}} function relating to the method of point layout
}
  \item{lines}{Adds lines from group centroid to x-axis
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

\seealso{\code{\link[weedmodels]{wmodel.LDA}}, \code{\link[weedmodels]{wplot_geog}}, \code{\link[weedmodels]{wplot_basic}},\code{\link[weedmodels]{wplot_arch}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Example dataset
LD1<-runif(40, min=-6.6, max=6)
Study<-sample(1:3, 40, replace=T)
data<-data.frame(Study,LD1)

## If using defaults
wplot_phase( "model1", data$LD1, pcolumn= "Study", g1=1, g2=2, g3=3, site=c("phase 1", "phase 2", "phase 3"))
wplot_phase( 1, data$LD1)

## Use of model1 -  modern data from Asturias, Spain and Haute Provence,
France (Boggard et al. 2016)
wplot_arch("model1",data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green",col3="purple",
pch1=1,  pch3=19, compact =F, priority = "ascending", lines=T, site="example")

## Use of model2 - modern data from Evvia, Greece; Asturias, Spain; HauteProvence,
France; and oases and rain fed terraces in Morocco (Bogaard et al. 2018)
wplot_arch("model2",data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green", col3="purple",
pch1=1,pch3=19, compact =F, priority = "ascending", lines=T, site= "example")

## Use of model3 - modern data from Highgrove and Laxton, UK- (Bogaard et al.2022)
wplot_arch("model3", data$LD1, xlims=c(-7,7),ticks=-7:7, col1="green",  col3="purple",
pch1=1,pch3=19, compact =F, priority = "ascending", lines=T, site="example")


## Different legend options
wplot_arch(1,data$LD1, legend = "right")

wplot_arch(1,data$LD1, legend = "left")

wplot_arch(1,data$LD1,legend = "split")

## Different priority options using "ascending", "descending", "density",
"random","none"
par(mfrow=c(3,2),
    oma=c(2,0,0,0),
    mar=c(0,0,0,0))
wplot_arch(1,data$LD1, compact =F, priority = "ascending")
mtext("ascending", 3,-2)
wplot_arch(1,data$LD1, compact =F, priority = "descending")
mtext("descending", 3, -2)
wplot_arch(1,data$LD1,compact =F, priority = "density")
mtext("density",3, -2)
wplot_arch(1,data$LD1, compact =F, priority = "random")
mtext("random",3,-2)
wplot_arch(1,data$LD1, compact =F, priority = "none")
mtext("none",3, -2)

## Options with compact TRUE and FALSE while using ascending or descending priority
par(mfrow=c(2,2),
    oma=c(2,0,0,0),
    mar=c(0,0,0,0))
wplot_arch(1,data$LD1, compact =F, priority = "ascending")
mtext("ascending, TRUE", 3,-3)
wplot_arch(1,data$LD1, compact =T, priority = "ascending")
mtext("ascending, FALSE", 3,-3)
wplot_arch(1,data$LD1,compact =F, priority = "descending")
mtext("descending, TRUE", 3,-3)
wplot_arch(1,data$LD1, compact =T, priority = "descending")
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
