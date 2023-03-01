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
wplot_geog(model, x)

#defaults
wplot_geog(model, x,xlims= NULL,ticks= NULL,col1= "black",col2= "black",col3= "black",pch1= 1,pch3= 5,xlab="Discriminant function",compact= F,priority= "descending",site= "samples",lines= F,legend= F)

}

\arguments{
  \item{model}{The modern model to be plotted – either "model1", "model3" or "model3" following \code{\link[WeedEco]{wmodel.LDA}}
%%     ~~Describe \code{model} here~~
}
  \item{x}{The unstandarised LD1* column from the results dataframe of \code{\link[WeedEco]{wmodel.LDA}}. Can also use linear discriminant scores from alternative sources.
%%     ~~Describe \code{x} here~~
}
  \item{xlims}{The limits of the x axis
%%     ~~Describe \code{xlims} here~~
}
  \item{ticks}{The location of the x axis ticks
%%     ~~Describe \code{ticks} here~~
}
  \item{col1}{The colour of the modern model's centroids
%%     ~~Describe \code{col1} here~~
}
  \item{col2}{The colour of the modern model's datapoints
%%     ~~Describe \code{col2} here~~
}
  \item{col3}{The colour of the entered samples' datapoints
%%     ~~Describe \code{col3} here~~
}
  \item{pch1}{The symbol of the modern model's centroids- must be between 0 and 2
%%     ~~Describe \code{pch1} here~~
}
  \item{pch3}{The symbol of the entered samples' datapoints
%%     ~~Describe \code{pch3} here~~
}
 \item{xlab}{The label for the x axis
%%     ~~Describe \code{pch3} here~~
}
  \item{compact}{Follows the beeswarm package's \code{\link[beeswarm]{swarmy}} compact variable
%%     ~~Describe \code{compact} here~~
}
  \item{priority}{Follows \code{\link[beeswarm]{swarmy}} priority regarding the method of point layout
%%     ~~Describe \code{priority} here~~
}
 \item{lines}{Adds lines from group centroid to x-axis
%%     ~~Describe \code{lines} here~~
}
\item{site}{Adds the name of the entered data to be shown in the legend
%%     ~~Describe \code{site} here~~
}
\item{legend}{Adds a legend to the plot
}
\item{lcol}{Colour for the entered samples' in legend - use if multiple colours are used in col3
}
\item{lpch}{Symbol shape for the entered samples' in legend - use if multiple symbols are used in pch3
}
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
Study<-sample(1:3, 40, replace=T)
data<-data.frame(Study,LD1)

##If using with defaults

wplot_geog("model1", data$LD1)
wplot_geog("model2", data$LD1)
wplot_geog("model3", data$LD1)

##Use of model 1 -  modern data from Asturias, Spain and Haute Provence, France
#- see Bogaard et al. 2016

wplot_geog("model1",data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",
col3="purple", pch1=1, pch3=19, compact =F, priority = "ascending", lines=T, site="example")

##Use of model 2 - modern data from Evvia, Greece; Asturias, Spain; Haute Provence,
#France; and oases and rain fed terraces in Morocco - see Bogaard et al. 2018

wplot_geog("model2", data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green",  col2="red",
col3="purple", pch1=1, pch3=19, compact =F, priority = "ascending", lines=T, site="example")

##Use of model 3 - modern data from Highgrove and Laxton, UK- see Bogaard et al. 2022

wplot_geog("model3", data$LD1, xlims=c(-7,7),ticks=-7:7, col1="green",  col2="red",
col3="purple", pch1=1, pch3=19, compact =F, priority = "ascending", lines=T, site="example")


##Different priority options using "ascending", "descending", "density", "random", "none"

par(mfrow=c(3,2),
    oma=c(0,0,0,0),
    mar=c(0,0,0,0))
wplot_geog(1,data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple",
pch1=1,  pch3=19,compact =F, priority = "ascending")
mtext("ascending", 3,-1.5)
wplot_geog(1,data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple",
pch1=1,  pch3=19,compact =F, priority = "descending")
mtext("descending", 3, -1.5)
wplot_geog(1,data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple",
pch1=1, pch3=19,compact =F, priority = "density")
mtext("density",3, -1.5)
wplot_geog(1,data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple",
pch1=1,  pch3=19,compact =F, priority = "random")
mtext("random",3,-1.5)
wplot_geog(1,data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple",
pch1=1,  pch3=19,compact =F, priority = "none")
mtext("none",3, -1.5)

## options with compact T and F using ascending and descending priority

par(mfrow=c(2,2),
    oma=c(0,0,0,0),
    mar=c(0,0,0,0))
wplot_geog(1,data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple",
pch1=1, pch3=19,
compact =F, priority = "ascending")
mtext("ascending, TRUE", 3,-1.5)
wplot_geog(1,data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple",
pch1=1, pch3=19,compact =T, priority = "ascending")
mtext("ascending, FALSE", 3,-1.5)
wplot_geog(1,data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple",
pch1=1, pch3=19, compact =F, priority = "descending")
mtext("descending, TRUE", 3,-1.5)
wplot_geog(1,data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple",
pch1=1, pch3=19,compact =T, priority = "descending")
mtext("descending, FALSE", 3,-1.5)

## options with different col and pch for different archaeobotanical sites or different phases

wplot_geog("model2", data$LD1, col3=data$Study,pch3=data$Study, legend=T, site=c("Site 1",
"Site 2","Site 3"))

}
