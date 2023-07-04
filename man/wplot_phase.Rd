\name{wplot_phase}
\alias{wplot_phase}

\title{ Function to plot samples' linear discriminant scores and model's centroids by phase or site
}
\description{A function which plots the samples' linear discriminant scores from \code{\link[WeedEco]{wmodel.LDA}} compared to the chosen modern model's group centroids as stacked graphs depending on number of grouping variables (i.e. sites or phase).
}
\usage{
wplot_phase(model, x,
site= c("phase 1", "phase 2", "phase 3", "phase 4", "phase 5"),
g1=NULL, g2= NULL, g3=NULL, g4=NULL, g5=NULL,
gcol=c("black","black","black","black", "black"),
gpch=c(22,23,24,8,4), gbg=c("grey","grey","grey","grey", "grey"), pcolumn, compact= FALSE,
priority= "density", xlims= NULL, ylims = NULL, ticks =NULL, xlab="Discriminant function",
lines=TRUE, lcex=1)
}
\arguments{
\item{model}{Either "model1", "model2" or "model3" - following \code{\link[WeedEco]{wmodel.LDA}}}
\item{x}{The output from \code{\link[WeedEco]{wmodel.LDA}}, or one containing LDA scores. This should also contain a grouping variable to be used in argument pcolumn}
\item{site}{The names to appear in the legend}
\item{g1}{Name of phase/site group 1 in pcolumn}
\item{g2}{Name of phase/site group 2 in pcolumn}
\item{g3}{Name of phase/site group 3 in pcolumn}
\item{g4}{Name of phase/site group 4 in pcolumn}
\item{g5}{Name of phase/site group 5 in pcolumn}
\item{gcol}{Symbol colour for athe entered samples' phases}
\item{gpch}{Symbol shape for the entered samples' phases}
\item{gbg}{Symbol infill colour if using pch 21,22,23,24 or 25}
\item{pcolumn}{The column with the phases of each sample from the dataframe x}
\item{compact}{Follows \code{\link[beeswarm]{swarmy}} from the beeswarm package}
\item{priority}{Follows the \code{\link[beeswarm]{swarmy}} function relating to the method of point layout}
\item{xlims}{The x axis limits}
\item{ylims}{The y axis limits}
\item{ticks}{The location of the x axis ticks}
\item{xlab}{x axis label}
\item{lines}{Adds lines from group centroid to x-axis}
\item{lcex}{Size of the legend text}
}
\author{Elizabeth Stroud
}
\seealso{\code{\link[WeedEco]{wmodel.LDA}},  \code{\link[WeedEco]{wplot_geog}},  \code{\link[WeedEco]{wplot_basic}}
}
\examples{
## Example dataset

LD1<-runif(40, min=-6.6, max=6)
Study<-sample(1:3, 40, replace=TRUE)
data<-data.frame(Study,LD1)

## If using defaults

wplot_phase( "model1", data, pcolumn= data$Study, g1=1, g2=2, g3=3,
site=c("phase 1", "phase 2", "phase 3"))

## Changes to colour and symbol type

wplot_phase( "model1", data, pcolumn= data$Study, g1=1, g2=2, g3=3,
site=c("phase 1", "phase 2", "phase 3"),gpch = c(24,4,22), gbg = c("red","black","grey"),
gcol=c("black","black","black"))
}
