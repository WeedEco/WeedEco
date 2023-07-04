\name{wplot_basic}
\alias{wplot_basic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plotting function to display the linear discriminant scores from \code{\link[WeedEco]{wmodel.LDA}} against the functional ecological model centroids and linear discriminant scores
}
\description{A function which plots the linear discriminant scores from \code{\link[WeedEco]{wmodel.LDA}} against the functional ecological model centroids and linear discriminant scores
}
\usage{
wplot_basic (model, x, xlims= NULL,ticks = NULL,col1 = "black",col2 = "black",
col3 = "black", pch1 = 1, pch2 = 2, pch3 = 0, xlab="Discriminant function",
compact =  FALSE, priority = "density",
lines = TRUE, site = "Samples", lcol=col3, lpch=pch3,legend = "split")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{The modern model to be plotted – either "model1", "model2", or "model3" - use the model against which the samples have a been classified against, using \code{\link[WeedEco]{wmodel.LDA}}.
%%     ~~Describe \code{model} here~~
}
  \item{x}{The data frame which contains the linear discriminant scores of the anaylsised data - if using data from a source other than \code{\link[WeedEco]{wmodel.LDA}}, column needs to be called LD1
%%     ~~Describe \code{x} here~~
}
  \item{xlims}{The limits of the x axis
%%     ~~Describe \code{xlims} here~~
}
  \item{ticks}{The location of the x axis ticks along the x axis
}
  \item{col1}{The colour of the modern model's centroids
}
  \item{col2}{The colour of the modern model's datapoints
}
  \item{col3}{The colour of the archaeobotanical datapoints
}
  \item{pch1}{The symbol of the modern model's centroids - must be between 0 and 2
}
  \item{pch2}{The symbol of the modern model's datapoints - must be between 0 and 2
}
  \item{pch3}{The symbol of entered samples datapoints
}
\item{xlab}{Label on the x axis, default is "Discriminant function"
}
  \item{compact}{Follows \code{\link[beeswarm]{swarmy}} from the beeswarm package
}
  \item{priority}{Follows the \code{\link[beeswarm]{swarmy}} function relating to the method of point layout
}
 \item{lines}{Adds lines from group centroid to x-axis
}

\item{site}{Add labels in the legend for the archaeobotanical data
}
\item{lcol}{Colour for archaeobotanical samples in legend - use if multiple colours are used in col3
}
\item{lpch}{Symbol shape for entered samples in legend - use if multiple symbols are used in pch3
}
\item{legend}{Adds a legend showing the symbols used to denote group centroids and entered samples (x). Options are "right" (located on the right- hand side), "left" (located on the left-hand side of the graph), and "split" (located on the right-hand side, with labels aligning with the graphed data)
}
}

\references{model 1 - see Bogaard, A., Hodgson, J., Nitsch, E., Jones, G., Styring, A., Diffey, C., Pouncett, J., Herbig, C., Charles, M., Ertuğ, F., Tugay, O. Filipović, D. and Fraser, R. (2016) 'Combining functional weed ecology and crop stable isotope ratios to identify cultivation intensity: a comparison of cereal production regimes in Haute Provence, France and Asturias, Spain' \emph{Vegetation History and Archaeobotany} 25, 57-73

model 2 - see Bogaard, A., Styring, A., Ater, M., Hmimsa, Y., Green, L., Stroud, E., Whitlam, J., Diffey, C., Nitsch, E., Charles, M., Jones, G. and Hodgson, J.2018 'From traditional farming in Morocco to early urban agroecology in northern Mesopotamia: combining present-day arable weed surveys and crop isotope analysis to reconstruct past agrosystems in (semi-)arid regions' \emph{Environmental Archaeology} 23, 303-322

model 3 - see Bogaard, A., Hodgson, J., Kropp, C., McKerracher, M. and Stroud, E. (2022). ‘Lessons from Laxton, Highgrove and Lorsch: Building arable weed-based models for the investigation of early medieval agriculture in England’, in M. McKerracher and H. Hamerow (eds) \emph{New Perspectives on the Medieval ‘Agricultural Revolution’: Crop, Stock and Furrow} (Liverpool: Liverpool University Press).
%% ~put references to the literature/web site here ~
}
\author{Elizabeth Stroud
%%  ~~who you are~~
}

\seealso{\code{\link[WeedEco]{wmodel.LDA}}, \code{\link[WeedEco]{wplot_phase}}, \code{\link[WeedEco]{wplot_geog}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##Example dataset

LD1<-runif(40, min=-6.6, max=6)
Study<-sample(1:3, 40, replace=TRUE)
data<-data.frame(Study,LD1)

##Usage with defaults

wplot_basic("model1", data)
wplot_basic("model2", data)
wplot_basic("model3", data)

## Use of model 1 - modern data from Asturias, Spain and Haute Provence, France
#(Bogaard \emph{et al.} 2016)

wplot_basic("model1",data,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",
col3="purple",pch1=1, pch2=2, pch3=19, compact =FALSE, priority = "ascending", lines=TRUE,
site="example")

##Use of model2 - modern data from Evvia, Greece; Asturias, Spain; Haute Provence,
#France; and oases and rainfed terraces in Morocco (Bogaard \emph{et al.} 2018)

wplot_basic("model2", data, xlims=c(-7,7),ticks=-7:7, col1="green",  col2="red",
col3="purple",pch1=1, pch2=2,pch3=19, compact =FALSE, priority = "ascending", lines=TRUE,
site="example")

##Use of model3 - modern data from Highgrove and Laxton,UK -see Bogaard \emph{et al.} 2022

wplot_basic("model3", data, xlims=c(-7,7),ticks=-7:7, col1="green",  col2="red",
col3="purple",pch1=1,pch2=2, pch3=19, compact =FALSE, priority = "ascending", lines=TRUE,
site="example")

## Options with different colours and symbols for different archaeological sites

wplot_basic("model1", data, col3=data$Study,pch3=data$Study, site=c("Site 1",
"Site 2","Site 3"))

##Different priority options using "ascending", "descending", "density", "random", "none"

par(mfrow=c(3,2),
    oma=c(0,0,0,0),
    mar=c(0,0,0,0))
wplot_basic(1,data,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",
col3="purple", pch1=1,pch2=2, pch3=19, compact =FALSE, priority = "ascending")
mtext("ascending", 3,-1.5)
wplot_basic(1,data,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",
col3="purple", pch1=1,pch2=2, pch3=19, compact =FALSE, priority = "descending")
mtext("descending", 3, -1.5)
wplot_basic(1,data,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",
col3="purple", pch1=1, pch2=2, pch3=19, compact =FALSE, priority = "density")
mtext("density",3, -1.5)
wplot_basic(1,data,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",
col3="purple", pch1=1, pch2=2, pch3=19, compact =FALSE, priority = "random")
mtext("random",3,-1.5)
wplot_basic(1,data,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",
col3="purple", pch1=1, pch2=2, pch3=19, compact =FALSE, priority = "none")
mtext("none",3, -1.5)

## Grpah options using compact TRUE and FALSE as well as ascending and descending priority

par(mfrow=c(2,2),
    oma=c(0,0,0,0),
    mar=c(0,0,0,0))
wplot_basic(1,data,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",
col3="purple", pch1=1, pch2=2, pch3=19, compact =FALSE, priority = "ascending")
mtext("ascending, TRUE", 3,-1.5)
wplot_basic(1,data,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",
col3="purple", pch1=1, pch2=2, pch3=19, compact =TRUE, priority = "ascending")
mtext("ascending, FALSE", 3,-1.5)
wplot_basic(1,data,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",
col3="purple", pch1=1, pch2=2, pch3=19, compact =FALSE, priority = "descending")
mtext("descending, TRUE", 3,-1.5)
wplot_basic(1,data,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",
col3="purple", pch1=1, pch2=2, pch3=19, compact =TRUE, priority = "descending")
mtext("descending, FALSE", 3,-1.5)

}

