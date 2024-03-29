\name{wplot_geog}
\alias{wplot_geog}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plotting function for displaying samples' linear discriminant scores from  \code{\link[WeedEco]{wmodel.LDA}} compared to with the chosen modern model's centroids, and the model's discriminant scores separated according to modern study location

%%  ~~function to do ... ~~
}
\description{A function which plots the samples' linear discriminant scores from \code{\link[WeedEco]{wmodel.LDA}}, the chosen model's centroids, and the model's discriminant scores with separate symbols for the different modern study's arable fields.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
wplot_geog(model, x,xlims= NULL,ticks= NULL,col1= "black",col2= "black",
col3= "black",pch1= 1,pch3= 5,xlab="Discriminant function",compact= FALSE,
priority= "descending",site= "Samples",lcol=col3, lpch=pch3, lines= TRUE,
legend= TRUE)
}
\arguments{
  \item{model}{The modern model to be plotted – either "model1", "model3" or "model3" following \code{\link[WeedEco]{wmodel.LDA}}
}
 \item{x}{The data frame which contains the linear discriminant scores of the anaylsised data from \code{\link[WeedEco]{wmodel.LDA}}.  Can also use linear discriminant scores from alternative sources however the column needs to be called LD1

}
  \item{xlims}{The limits of the x axis
}
  \item{ticks}{The location of the x axis ticks
}
  \item{col1}{The colour of the modern model's centroids
}
  \item{col2}{The colour of the modern model's datapoints
}
  \item{col3}{The colour of the entered samples' datapoints
}
  \item{pch1}{The symbol of the modern model's centroids- must be between 0 and 2
}
  \item{pch3}{The symbol of the entered samples' datapoints
}
 \item{xlab}{The label for the x axis
}
  \item{compact}{Follows the beeswarm package's \code{\link[beeswarm]{swarmy}} compact variable
%%     ~~Describe \code{compact} here~~
}
  \item{priority}{Follows \code{\link[beeswarm]{swarmy}} priority regarding the method of point layout
%%     ~~Describe \code{priority} here~~
}
\item{site}{Adds the name of the entered data to be shown in the legend
%%     ~~Describe \code{site} here~~
}

\item{lcol}{Colour for the entered samples in legend - use if multiple colours are used in col3
}
\item{lpch}{Symbol shape for the entered samples in legend - use if multiple symbols are used in pch3
}
\item{lines}{Adds lines from group centroid to x-axis
%%     ~~Describe \code{lines} here~~
}\item{legend}{Adds a legend to the plot
}
}

\references{model 1 - see Bogaard, A., Hodgson, J., Nitsch, E., Jones, G., Styring, A., Diffey, C., Pouncett, J., Herbig, C., Charles, M., Ertuğ, F., Tugay, O. Filipović, D. and Fraser, R. (2016) 'Combining functional weed ecology and crop stable isotope ratios to identify cultivation intensity: a comparison of cereal production regimes in Haute Provence, France and Asturias, Spain' \emph{Vegetation History and Archaeobotany} 25, 57-73

model 2 - see Bogaard, A., Styring, A., Ater, M., Hmimsa, Y., Green, L., Stroud, E., Whitlam, J., Diffey, C., Nitsch, E., Charles, M., Jones, G. and Hodgson, J.2018 'From traditional farming in Morocco to early urban agroecology in northern Mesopotamia: combining present-day arable weed Surveys and crop isotope analysis to reconstruct past agrosystems in (semi-)arid regions' \emph{Environmental Archaeology} 23, 303-322

model 3 - see Bogaard, A., Hodgson, J., Kropp, C., McKerracher, M. and Stroud, E. (2022). ‘Lessons from Laxton, Highgrove and Lorsch: building arable weed-based models for the investigation of early medieval agriculture in England’, in M. McKerracher and H. Hamerow (eds) \emph{New Perspectives on the Medieval ‘Agricultural Revolution’: Crop, Stock and Furrow} (Liverpool: Liverpool University Press).
%% ~put references to the literature/web site here ~
}
\author{Elizabeth Stroud
%%  ~~who you are~~
}

\seealso{\code{\link[WeedEco]{wmodel.LDA}}, \code{\link[WeedEco]{wplot_phase}}, \code{\link[WeedEco]{wplot_basic}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##example dataset

LD1<-runif(40, min=-6.6, max=6)
Study<-sample(1:3, 40, replace=TRUE)
data<-data.frame(Study,LD1)

##If using with defaults

wplot_geog("model1", data)
wplot_geog("model2", data)
wplot_geog("model3", data)

##Use of model 1 -  modern data from Asturias, Spain and Haute Provence, France
#- see Bogaard et al. 2016

wplot_geog("model1",data,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",
col3="purple", pch1=1, pch3=19, compact =FALSE, priority = "ascending", lines=TRUE, site="example")

##Use of model 2 - modern data from Evvia, Greece; Asturias, Spain; Haute Provence,
#France; and oases and rain fed terraces in Morocco - see Bogaard et al. 2018

wplot_geog("model2", data,xlims=c(-7,7),ticks=-7:7, col1="green",  col2="red",
col3="purple", pch1=1, pch3=19, compact =FALSE, priority = "ascending", lines=TRUE, site="example")

##Use of model 3 - modern data from Highgrove and Laxton, UK- see Bogaard et al. 2022

wplot_geog("model3", data, xlims=c(-7,7),ticks=-7:7, col1="green",  col2="red",
col3="purple", pch1=1, pch3=19, compact =FALSE, priority = "ascending", lines=TRUE, site="example")


##Different priority options using "ascending", "descending", "density", "random", "none"

par(mfrow=c(3,2),
    oma=c(0,0,0,0),
    mar=c(0,0,0,0))
wplot_geog(1,data,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple",
pch1=1,  pch3=19,compact =FALSE, priority = "ascending")
mtext("ascending", 3,-1.5)
wplot_geog(1,data,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple",
pch1=1,  pch3=19,compact =FALSE, priority = "descending")
mtext("descending", 3, -1.5)
wplot_geog(1,data,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple",
pch1=1, pch3=19,compact =FALSE, priority = "density")
mtext("density",3, -1.5)
wplot_geog(1,data,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple",
pch1=1,  pch3=19,compact =FALSE, priority = "random")
mtext("random",3,-1.5)
wplot_geog(1,data,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple",
pch1=1,  pch3=19,compact =FALSE, priority = "none")
mtext("none",3, -1.5)

## options with compact TRUE and FALSE using ascending and descending priority

par(mfrow=c(2,2),
    oma=c(0,0,0,0),
    mar=c(0,0,0,0))
wplot_geog(1,data,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple",
pch1=1, pch3=19,
compact =FALSE, priority = "ascending")
mtext("ascending, TRUE", 3,-1.5)
wplot_geog(1,data,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple",
pch1=1, pch3=19,compact =TRUE, priority = "ascending")
mtext("ascending, FALSE", 3,-1.5)
wplot_geog(1,data,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple",
pch1=1, pch3=19, compact =FALSE, priority = "descending")
mtext("descending, TRUE", 3,-1.5)
wplot_geog(1,data,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple",
pch1=1, pch3=19,compact =TRUE, priority = "descending")
mtext("descending, FALSE", 3,-1.5)

## options with different col and pch for different archaeobotanical sites or different phases

wplot_geog("model2", data, col3=data$Study,pch3=data$Study, legend=TRUE, site=c("Site 1",
"Site 2","Site 3"))

}

