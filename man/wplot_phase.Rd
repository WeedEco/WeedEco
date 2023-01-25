\name{wplot_phase}
\alias{wplot_phase}

\title{ Function to plot the archaeobotanical samples' linear discriminant scores and model's centroids by phase or site

}
\description{A function which plots the archaeobotanical samples' linear discriminant scores from \code{\link[WeedEco]{wmodel.LDA}} compared to the chosen modern model's group centroids as stacked graphs depending on number of grouping variables (i.e. sites or phase).

}
\usage{
wplot_phase(model,x, pcolumn, site, gpch,gcol,gbg, g1,g2)
}

\arguments{
  \item{model}{Either "model1", "model2" or "model3" - following \code{\link[WeedEco]{wmodel.LDA}}
}
  \item{x}{The output from \code{\link[WeedEco]{wmodel.LDA}}, or one containing LDA scores. This should also contain a grouping variable to be used in argument pcolumn
}
  \item{pcolumn}{The column with the phases of each sample from the dataframe x
}

  \item{gpch}{Symbol shape for the archaeobotanical phases
}
  \item{gcol}{Symbol colour for archaeobotanical phases
}
  \item{gbg}{Symbol infill colour if using pch 21,22,23,24 or 25
}
\item{site}{The names to appear in the legend
}
  \item{g1}{Name of phase/site group 1 in pcolumn
}
  \item{g2}{Name of phase/site group 2 in pcolumn
}
  \item{g3}{Name of phase/site group 3 in pcolumn
}
  \item{g4}{Name of phase/site group 4 in pcolumn
}
  \item{g5}{Name of phase/site group 5 in pcolumn
}
  \item{xlims}{The x axis limits
}
  \item{ticks}{The location of the x axis ticks
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

\seealso{\code{\link[WeedEco]{wmodel.LDA}},  \code{\link[WeedEco]{wplot_geog}},  \code{\link[WeedEco]{wplot_basic}}, \code{\link[WeedEco]{wplot_arch}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Example dataset

LD1<-runif(40, min=-6.6, max=6)
Study<-sample(1:3, 40, replace=T)
data<-data.frame(Study,LD1)

## If using defaults

wplot_phase( "model1", data, pcolumn= data$Study, g1=1, g2=2, g3=3,
site=c("phase 1", "phase 2", "phase 3"))

## Changes to colour and symbol type

wplot_phase( "model1", data, pcolumn= data$Study, g1=1, g2=2, g3=3,
site=c("phase 1", "phase 2", "phase 3"),gpch = c(24,4,22), gbg = c("red","black","grey"),
gcol=c("black","black","black"))
}
