\name{plot5}
\alias{plot5}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plotting function for displaying the output of \code{\link[FIBS]{model.LDA}}, plotting the archaeobotanical samples linear discriminant scores, the chosen model's centroids, and the model's discriminant scores with separate symbols for the different modern study's arable fields.
%%  ~~function to do ... ~~
}
\description{A function which plots the archaeobotanical samples linear discriminant scores from following \code{\link[FIBS]{model.LDA}} compared to with the chosen modern model's centroids, and the model's discriminant scores separated according to modern study location
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot5(model, x)

#defaults
plot5(model,
  x,
  xlims= NULL,
  ticks= NULL,
  col1= "black",
  col2= "black",
  col3= "black",
  pch1= 1,
  pch3= 5,
  compact= F,
  priority= "descending",
  site= "samples",
  lines= F,
  legend= F)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{the modern model to be plotted – either "model1", "model3" or "model3" following \code{\link[FIBS]{model.LDA}}
%%     ~~Describe \code{model} here~~
}
  \item{x}{the LD1 column from the results dataframe of \code{\link[FIBS]{model.LDA}}. Can also use linear discriminant score from other sources.
%%     ~~Describe \code{x} here~~
}
  \item{xlims}{the limits of the x axis
%%     ~~Describe \code{xlims} here~~
}
  \item{ticks}{ the location of the x axis labels
%%     ~~Describe \code{ticks} here~~
}
  \item{col1}{the colour of the modern model's centroids
%%     ~~Describe \code{col1} here~~
}
  \item{col2}{the colour of the modern model's datapoints
%%     ~~Describe \code{col2} here~~
}
  \item{col3}{the colour of the archaeobotanical datapoints
%%     ~~Describe \code{col3} here~~
}
  \item{pch1}{the symbol of the modern model's centroids- must be between 0 and 2
%%     ~~Describe \code{pch1} here~~
}
  \item{pch3}{the symbol of the archaeobotanical datapoints
%%     ~~Describe \code{pch3} here~~
}
  \item{compact}{ follows the beeswarm package's \code{\link[beeswarm]{swarmy}} compact variable
%%     ~~Describe \code{compact} here~~
}
  \item{priority}{ follows \code{\link[beeswarm]{swarmy}} priority regarding the method of point layout
%%     ~~Describe \code{priority} here~~
}
 \item{lines}{ adds lines from group centroid to x-axis
%%     ~~Describe \code{lines} here~~
}
\item{site}{adds name of archaeobotanical data
%%     ~~Describe \code{site} here~~
}
\item{legend}{adds legend to the plot
%%     ~~Describe \code{legend} here~~
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

model 2 - see Bogaard, A., Styring, A., Ater, M., Hmimsa, Y., Green, L., Stroud, E., Whitlam, J., Diffey, C., Nitsch, E., Charles, M., Jones, G. and Hodgson, J.2018 'From traditional farming in Morocco to Early Urban Agroecology in northern Mesopotamia: combining Present-day Arable Weed Surveys and crop isotope analysis to reconstruct past Agrosystems in (Semi-)arid Regions' \emph{Environmental Archaeology} 23, 303-322

model 3 - see Bogaard, A., Hodgson, J., Kropp, C., McKerracher, M. and Stroud, E. (in press 2022). ‘Lessons from Laxton, Highgrove and Lorsch: Building arable weed-based models for the investigation of early medieval agriculture in England’, in M. McKerracher and H. Hamerow (eds) \emph{New Perspectives on the Medieval ‘Agricultural Revolution’: Crop, Stock and Furrow} (Liverpool: Liverpool University Press).
%% ~put references to the literature/web site here ~
}
\author{Elizabeth Stroud
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link[FIBS]{model.LDA}}, \code{\link[FIBS]{plot3}}, \code{\link[FIBS]{plot4}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##example dataset
LD1<-runif(40, min=-6.6, max=6)
Study<-sample(1:3, 40, replace=T)
data<-data.frame(Study,LD1)

##If using with defaults
plot5("model1", data$LD1)
plot5("model2", data$LD1)
plot5("model3", data$LD1)

##Use of model 1 -  modern data from Asturias, Spain and Haute Provence, France - see Bogaard et al 2016

plot5("model1",data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=1, pch3=19, compact =F, priority = "ascending", lines=T,site="example")

##Use of model 2 - modern data from Evvia, Greece; Asturias, Spain; Haute Provence, France; and oases and rain fed terraces in Morocco - see Bogaard et al 2018

plot5("model2",data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green",  col2="red",col3="purple", pch1=1,pch3=19, compact =F, priority = "ascending", lines=T,site="example")

##Use of model 3 - modern data from Highgrove and Laxton, UK- see Bogaard et al 2022

plot5("model3", data$LD1, xlims=c(-7,7),ticks=-7:7, col1="green",  col2="red",col3="purple", pch1=1, pch3=19, compact =F, priority = "ascending", lines=T, site="example")


##Different priority options using "ascending", "descending", "density", "random", "none"
par(mfrow=c(3,2),
    oma=c(0,0,0,0),
    mar=c(0,0,0,0))
plot5(1,data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=1,  pch3=19, compact =F, priority = "ascending")
mtext("ascending", 3,-1.5)
plot5(1,data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=1,  pch3=19, compact =F, priority = "descending")
mtext("descending", 3, -1.5)
plot5(1,data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=1, pch3=19, compact =F, priority = "density")
mtext("density",3, -1.5)
plot5(1,data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=1,  pch3=19, compact =F, priority = "random")
mtext("random",3,-1.5)
plot5(1,data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=1,  pch3=19, compact =F, priority = "none")
mtext("none",3, -1.5)

## options with compact T and F using ascending and descending priority
par(mfrow=c(2,2),
    oma=c(0,0,0,0),
    mar=c(0,0,0,0))
plot5(1,data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=1, pch3=19, compact =F, priority = "ascending")
mtext("ascending, TRUE", 3,-1.5)
plot5(1,data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=1, pch3=19, compact =T, priority = "ascending")
mtext("ascending, FALSE", 3,-1.5)
plot5(1,data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=1, pch3=19, compact =F, priority = "descending")
mtext("descending, TRUE", 3,-1.5)
plot5(1,data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green", col2="red",col3="purple", pch1=1, pch3=19, compact =T, priority = "descending")
mtext("descending, FALSE", 3,-1.5)

## options with different col and pch for different archaeobotanical sites or different phases
plot5("model2", data$LD1, col3=data$Study,pch3=data$Study, legend=T, site=c("Site 1","Site 2","Site 3"))


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
