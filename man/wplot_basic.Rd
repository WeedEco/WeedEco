\name{wplot_basic}
\alias{wplot_basic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plotting function to display functional ecological model centroids, the model's linear discriminant scores and the archaeological samples' linear discriminant scores from \code{\link[Weedmodels]{wmodel.LDA}}
%%  ~~function to do ... ~~
}
\description{A function which plots the linear discriminant scores of the modern models based on arable fields from \code{\link[weedmodels]{wmodel.LDA}}, their group centroids and the archaeobotanical data’s discriminant scores.

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
wplot_basic(model, x)

## Defaults
wplot_basic (model, x, xlims= NULL,ticks = NULL,col1 = "black",col2 = "black",
col3 = "black", pch1 = 1,pch2 = 2,pch3 = 0,compact =  FALSE, priority = "density",
lines = TRUE, legend = "split", site = "samples")

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{the modern model to be plotted – either "model1", "model2", or "model3" - use the model against which the archaeobotanical samples have a been classified against, using \code{\link[Weedmodels]{wmodel.LDA}}.
%%     ~~Describe \code{model} here~~
}
  \item{x}{the column x from the data frame which contains the linear discriminant scores of the archaeobotanical data - if using data from \code{\link[weedmodels]{wmodel.LDA}}, such column will be called LD1
%%     ~~Describe \code{x} here~~
}
  \item{xlims}{The limits of the x axis
%%     ~~Describe \code{xlims} here~~
}
  \item{ticks}{The location of the x axis labels along the x axis
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
  \item{pch3}{The symbol of the archaeobotanical datapoints
}
  \item{compact}{Follows \code{\link[beeswarm]{swarmy}} from the beeswarm package
}
  \item{priority}{Follows the \code{\link[beeswarm]{swarmy}} function relating to the method of point layout
}
 \item{lines}{Adds lines from group centroid to x-axis
}
\item{legend}{Adds a legend showing the symbols used to denote group centroids and archaeobotanical samples. Options are "right" (located on the right- hand side), "left" (located on the left-hand side of the graph), and "split" (located on the right-hand side, with labels aligning with the graphed data)
}
\item{site}{Add labels in the legend for the archaeobotanical data
}
\item{lcol}{ colour for archaeobotanical samples in legend - use if multiple colours are used in col3
}
\item{lpch}{symbol shape for archaeobotanical samples in legend - use if multiple symbols are used in pch3
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
\references{model 1 - see Bogaard, A., Hodgson, J., Nitsch, E., Jones, G., Styring, A., Diffey, C., Pouncett, J., Herbig, C., Charles, M., Ertuğ, F., Tugay, O. Filipović, D. and Fraser, R. (2016) 'Combining functional weed ecology and crop stable isotope ratios to identify cultivation intensity: a comparison of cereal production regimes in Haute Provence, France and Asturias, Spain' \emph{Vegetation History and Archaeobotany} 25, 57-73

model 2 - see Bogaard, A., Styring, A., Ater, M., Hmimsa, Y., Green, L., Stroud, E., Whitlam, J., Diffey, C., Nitsch, E., Charles, M., Jones, G. and Hodgson, J.2018 'From traditional farming in Morocco to early urban agroecology in northern Mesopotamia: combining present-day arable weed surveys and crop isotope analysis to reconstruct past agrosystems in (semi-)arid regions' \emph{Environmental Archaeology} 23, 303-322

model 3 - see Bogaard, A., Hodgson, J., Kropp, C., McKerracher, M. and Stroud, E. (2022). ‘Lessons from Laxton, Highgrove and Lorsch: Building arable weed-based models for the investigation of early medieval agriculture in England’, in M. McKerracher and H. Hamerow (eds) \emph{New Perspectives on the Medieval ‘Agricultural Revolution’: Crop, Stock and Furrow} (Liverpool: Liverpool University Press).
%% ~put references to the literature/web site here ~
}
\author{Elizabeth Stroud
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link[weedmodels]{wmodel.LDA}}, \code{\link[weedmodels]{wplot_arch}}, \code{\link[weedmodels]{wplot_geog}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##Example dataset
LD1<-runif(40, min=-6.6, max=6)
Study<-sample(1:3, 40, replace=T)
data<-data.frame(Study,LD1)

##Usage with defaults
wplot_basic("model1", data$LD1)
wplot_basic("model2", data$LD1)
wplot_basic("model3", data$LD1)

## Use of model 1 - modern data from Asturias, Spain and Haute Provence, France
(Bogaard \emph{et al.} 2016)

wplot_basic("model1",data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple",
pch1=1, pch2=2, pch3=19, compact =F, priority = "ascending", lines=T, site="example")

##Use of model2 - modern data from Evvia, Greece; Asturias, Spain; Haute Provence, France;
and oases and rainfed terraces in Morocco (Bogaard \emph{et al.} 2018)

wplot_basic("model2", data$LD1, xlims=c(-7,7),ticks=-7:7, col1="green",  col2="red",col3="purple",
pch1=1, pch2=2,pch3=19, compact =F, priority = "ascending", lines=T, site="example")

##Use of model3 - modern data from Highgrove and Laxton, UK- see Bogaard \emph{et al.} 2022

wplot_basic("model3", data$LD1, xlims=c(-7,7),ticks=-7:7, col1="green",  col2="red",col3="purple",
pch1=1,pch2=2, pch3=19, compact =F, priority = "ascending", lines=T, site="example")

## Options with different colours and symbols for different archaeological sites
wplot_basic("model1", data$LD1, col3=data$Study,pch3=data$Study, site=c("Site 1","Site 2","Site 3"))

##Different priority options using "ascending", "descending", "density", "random", "none"
par(mfrow=c(3,2),
    oma=c(0,0,0,0),
    mar=c(0,0,0,0))
wplot_basic(1,data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=1,
pch2=2, pch3=19, compact =F, priority = "ascending")
mtext("ascending", 3,-1.5)
wplot_basic(1,data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=1,
pch2=2, pch3=19, compact =F, priority = "descending")
mtext("descending", 3, -1.5)
wplot_basic(1,data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=1,
pch2=2, pch3=19, compact =F, priority = "density")
mtext("density",3, -1.5)
wplot_basic(1,data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=1,
pch2=2, pch3=19, compact =F, priority = "random")
mtext("random",3,-1.5)
wplot_basic(1,data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=1,
pch2=2, pch3=19, compact =F, priority = "none")
mtext("none",3, -1.5)

## Grpah options using compact TRUE and FALSE as well as ascending and descending priority
par(mfrow=c(2,2),
    oma=c(0,0,0,0),
    mar=c(0,0,0,0))
wplot_basic(1,data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=1,
pch2=2, pch3=19, compact =F, priority = "ascending")
mtext("ascending, TRUE", 3,-1.5)
wplot_basic(1,data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=1,
pch2=2, pch3=19, compact =T, priority = "ascending")
mtext("ascending, FALSE", 3,-1.5)
wplot_basic(1,data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=1,
pch2=2, pch3=19, compact =F, priority = "descending")
mtext("descending, TRUE", 3,-1.5)
wplot_basic(1,data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=1,
pch2=2, pch3=19, compact =T, priority = "descending")
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
