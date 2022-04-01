---
editor_options:
  markdown:
    wrap: 72
output:
  word_document: default
  pdf_document: default
---

# `model.LDA` {#model.lda}

Linear Discriminant Analysis of Functional Attributes of Archaeological
Weed Seeds

## Description

This function conducts linear discriminant analysis using one of three
included modern models to classify archaeobotanical samples. The
function provides the classification of the input data (class), as well
as the discriminant scores of each sample (LD1) and the posterior
probability 1 and 2 (posterior.1 and posterior.2)

## Usage

``` r
model.LDA(model,x)
```

##Arguments

| Argument | Description                                                                                                                                                               |
|----------------|--------------------------------------------------------|
| `model`  | Either "model1", "model2" or "model3" (also 1,2 or 3). This argument allows the choice between three modern models against which the archaeological dataset is classified |
| `x`      | The archaeological data that will be compared against the model. Note data must be labelled with specific headers                                                         |

## Details

model.LDA requires the input dataset to have specific column names. The
columns providing the values for the different functional attributes
must be spelt as follows: SLA, ARNODE, LOGCAHN, LOGCADN, FLOWPER,
VEGPROP

## Value

A table containing the samples classification, the posterior.1 and
posterior.2 value (the posterior probabilities of the two linear
discriminant functions as per Venables and Ripley (2003) and the MASS
package), the linear discriminant score (LD1), and the input functional
attributes of data x

## Author

Elizabeth Stroud and others (list)

## References

model 1 - see Bogaard et al. 2016 "Combining functional weed ecology and
crop stable isotope ratios to identify cultivation intensity: a
comparison of cereal production regimes in Haute Provence, France and
Asturias, Spain" *Vegetation, History and Archaeobotany* 25, 57-73

model 2 - see Bogaard et al. 2018 "From traditional Farming in Morocco
to Early Urban Agroecology in Northern Mesopotamia: Combining
Present-day Arable Weed Surveys and crop isotope analysis to reconstruct
past Agrosystems in (Semi-)arid Regions" *Environmental Archaeology* 23,
303-322

model 3 - see Bogaard et al 2022 "????"

## Examples

``` r
model.LDA ('model1', data)
##Create random dataset for examples
SLA<-runif(40, min=20, max=30)
ARNODE<-runif(40, min=3000, max=22000)
LOGCAHN<-runif(40, min=5.5, max=6.5)
LOGCADN<-runif(40, min=5.5, max=7)
FLOWPER<-runif(40, min=3.5, max=7)
VEGPROP<-runif(40, min=0.15, max=0.8)
Study<-sample(1:3, 40, replace=T)
data<-as.data.frame(cbind(SLA,ARNODE,LOGCAHN,LOGCADN,FLOWPER,VEGPROP,Study))
## Use of model 1 -  modern data from Asturias, Spain and Haute Provence, France - see Bogaard et al 2016
model<-model.LDA("model1",data)
model<-model.LDA(1,data)

##Use of model 2 - modern data from Evvia, Greece; Asturias, Spain; Haute Provence, France; and oases and rain fed terraces in Morocco - see Bogaard et al 2018
model<-model.LDA("model2",data)
model<-model.LDA(2,data)

##Use of model 3 - modern data from Highgrove and Laxton, UK - see Bogaard et al ???
model<-model.LDA("model3",data)
model<-model.LDA(3,data)
```

# `plot3` {#plot3}

Plotting function to display functional ecology model centroids, model
and the archaeological samples' linear discriminant scores from
[`model.LDA`](#model.lda)

## Description

A function which plots the linear discriminant scores of the modern
functional ecology models from [`model.LDA`](#model.lda) , their group
centroids and the archaeological data's discriminant scores.

## Usage

``` r
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
```

## Arguments

| Argument   | Description                                                                                                                                                                                                                                                                                       |
|-----------------|-------------------------------------------------------|
| `model`    | the modern model to be plotted -- either 'model1', 'model2', or 'model3' - use the model against which the archaeological samples have a been classified against, using [`model.LDA`](#model.lda) .                                                                                               |
| `x`        | the column x from the data frame which contains the linear discriminant scores of the archaeological data - if using data from [`model.LDA`](#model.lda) , such column will be called LD1                                                                                                         |
| `xlims`    | the limits of the x axis                                                                                                                                                                                                                                                                          |
| `ticks`    | the location of the x axis labels along the x axis                                                                                                                                                                                                                                                |
| `col1`     | the colour of the modern model's centroids                                                                                                                                                                                                                                                        |
| `col2`     | the colour of the modern model's datapoints                                                                                                                                                                                                                                                       |
| `col3`     | the colour of the archaeological datapoints                                                                                                                                                                                                                                                       |
| `pch1`     | the symbol of the modern model's centroids - must be between 0 and 2                                                                                                                                                                                                                              |
| `pch2`     | the symbol of the modern model's datapoints - must be between 0 and 2                                                                                                                                                                                                                             |
| `pch3`     | the symbol of the archaeological datapoints                                                                                                                                                                                                                                                       |
| `compact`  | follows [`swarmy`](#swarmy) from the beeswarm package                                                                                                                                                                                                                                             |
| `priority` | follows the [`swarmy`](#swarmy) function relating to the method of point layout                                                                                                                                                                                                                   |
| `lines`    | add lines from group centroid to x-axis                                                                                                                                                                                                                                                           |
| `legend`   | adds a legend showing the symbols used to denote group centroids and archaeological samples. Options are "right" (located on the right- hand side), "left" (located on the left-hand side of the graph), and "split" (located on the right-hand side, with labels aligning with the graphed data) |
| `site`     | add labels to the archaeological data                                                                                                                                                                                                                                                             |

## See also

[`model.LDA`](#model.lda) , [`plot4`](#plot4) , [`plot5`](#plot5)

## Author

Elizabeth Stroud

## References

model 1 - see Bogaard et al 2016 "Combining functional weed ecology and
crop stable isotope ratios to identify cultivation intensity: a
comparison of cereal production regimes in Haute Provence, France and
Asturias, Spain" *Vegetation, History and Archaeobotany* 25, 57-73

model 2 - see Bogaard et al 2018 "From traditional Farming in Morocco to
Early Urban Agroecology in Northern Mesopotamia: Combining Present-day
Arable Weed Surveys and crop isotope analysis to reconstruct past
Agrosystems in (Semi-)arid Regions" *Environmental Archaeology* 23,
303-322

model 3 - see Bogaard et al 2022 " ????"

## Examples

``` r
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
```

# `plot4` {#plot4}

Plotting function to plot the archaeological samples' discriminant
scores and model's centroids

## Description

A function which plots the archaeological samples discriminant scores
from [`model.LDA`](#model.lda) compared to the chosen modern model's
centroids

## Usage

``` r
plot4(model, x,...)
## Defaults
plot4 (model,
        x,
        xlims= NULL,
        ticks=NULL,
        col1 = "black",
        col3= "black",
        pch1 = 1,
        pch3= 0,
        compact = FALSE,
        priority = "density",
        lines=FALSE,
        site = "samples",
        legend=F)
```

## Arguments

| Argument   | Description                                                                                                                                                                                                                                                                                   |
|-----------------|-------------------------------------------------------|
| `model`    | either "model1", "model2" or "model3" - following [`model.LDA`](#model.lda)                                                                                                                                                                                                                   |
| `x`        | the LD1 column from the results dataframe of [`model.LDA`](#model.lda)                                                                                                                                                                                                                        |
| `xlims`    | the x limits                                                                                                                                                                                                                                                                                  |
| `ticks`    | the location of the x axis labels                                                                                                                                                                                                                                                             |
| `col1`     | the colour of the centroids of the selected model                                                                                                                                                                                                                                             |
| `col3`     | the colour of the archaeological samples                                                                                                                                                                                                                                                      |
| `pch1`     | the symbol of the model's centroids                                                                                                                                                                                                                                                           |
| `pch3`     | the symbol of the archaeological samples                                                                                                                                                                                                                                                      |
| `compact`  | follows [`swarmy`](#swarmy) from the beeswarm package                                                                                                                                                                                                                                         |
| `priority` | follows the [`swarmy`](#swarmy) function relating to the method of point layout                                                                                                                                                                                                               |
| `lines`    | adds lines from group centroid to x-axis                                                                                                                                                                                                                                                      |
| `legend`   | adds a legend showing the symbols used to denote group centroids and archaeology sample. Options are "right" (located on the right- hand side), "left" (located on the left-hand side of the graph), and "split" (located on the right-hand side, with labels aligning with the graphed data) |
| `site`     | name of the archaeological data to appear on legend                                                                                                                                                                                                                                           |

## See also

[`model.LDA`](#model.lda) , [`plot3`](#plot3) , [`plot5`](#plot5)

## Author

Elizabeth Stroud

## References

model 1 - see Bogaard et al. 2016 "Combining functional weed ecology and
crop stable isotope ratios to identify cultivation intensity: a
comparison of cereal production regimes in Haute Provence, France and
Asturias, Spain" *Vegetation History and Archaeobotany* 25, 57-73

model 2 - see Bogaard et al. 2018 "From traditional Farming in Morocco
to Early Urban Agroecology in Northern Mesopotamia: Combining
Present-day Arable Weed Surveys and crop isotope analysis to reconstruct
past Agrosystems in (Semi-)arid Regions" *Environmental Archaeology 23*,
303-322

model 3 - see Bogaard et al 2022 "????"

## Examples

``` r
##example dataset
LD1<-runif(40, min=-6.6, max=6)
Study<-sample(1:3, 40, replace=T)
data<-data.frame(Study,LD1)

#If using defaults
plot4( "model1", data$LD1)
plot4( 1, data$LD1)

## Use of model1 -  modern data from Asturias, Spain and Haute Provence, France - see Boggard et al. 2016
plot4("model1",data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green",col3="purple", pch1=1,  pch3=19, compact =F, priority = "ascending", lines=T, site="example")

##Use of model2 - modern data from Evvia, Greece; Asturias, Spain; Haute Provence, France; and oases and rain fed terraces in Morocco - see Bogaard et al. 2018
plot4("model2",data$LD1,xlims=c(-7,7),ticks=-7:7, col1="green", col3="purple", pch1=1, pch3=19, compact =F, priority = "ascending", lines=T, site= "example")

##Use of model3 - modern data from Highgrove and Laxton, UK- see Bogaard et al. 2022
plot4("model3", data$LD1, xlims=c(-7,7),ticks=-7:7, col1="green",  col3="purple", pch1=1, pch3=19, compact =F, priority = "ascending", lines=T, site="example")

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

## different legend options
plot4(1,data$LD1, legend = "right")

plot4(1,data$LD1, legend="left")

plot4(1,data$LD1,legend="split")
```

# `plot5` {#plot5}

Plotting function for displaying the output of [`model.LDA`](#model.lda)
, plotting the archaeological samples discriminant scores, the chosen
model's centroids, and the model's discriminant scores with separate
symbols for the different modern study sites.

## Description

A function which plots the archaeological samples discriminant scores
from following [`model.LDA`](#model.lda) compared to with the chosen
modern model's centroids, and the model's discriminant scores separated
according to modern study sites

## Usage

``` r
plot5(model, x)
plot5(model, x, xlims = NULLL, ticks =NULL, col1 = NULL, col2=NULL, col3=NULL, pch1=NULL, pch3=NULL, compact=NULL, priority=NULL, lines=F, site= "Archaeological samples", legend=F)
#defaults
plot5(model,
  x,
  xlims= NULL,
  ticks =NULL,
  col1="black",
  col2= "black",
  col3="black",
  pch1=1,
  pch3=5,
  compact= F,
  priority= "descending",
  site= "Archaeological samples",
  lines=F,
  legend=F)
```

## Arguments

| Argument   | Description                                                                                                                         |
|------------------|------------------------------------------------------|
| `model`    | the modern model to be plotted -- either "model1", "model3" or "model3" following [`model.LDA`](#model.lda)                         |
| `x`        | the LD1 column from the results dataframe of [`model.LDA`](#model.lda) . Can also use linear discriminant score from other sources. |
| `xlims`    | the limits of the x axis                                                                                                            |
| `ticks`    | the location of the x axis labels                                                                                                   |
| `col1`     | the colour of the modern model's centroids                                                                                          |
| `col2`     | the colour of the modern model's datapoints                                                                                         |
| `col3`     | the colour of the archaeological datapoints                                                                                         |
| `pch1`     | the symbol of the modern model's centroids- must be between 0 and 2                                                                 |
| `pch3`     | the symbol of the archaeological datapoints                                                                                         |
| `compact`  | follows the beeswarm package's [`swarmy`](#swarmy) compact variable                                                                 |
| `priority` | follows [`swarmy`](#swarmy) priority regarding the method of point layout                                                           |
| `lines`    | adds lines from group centroid to x-axis                                                                                            |
| `site`     | adds name of archaeological data                                                                                                    |
| `legend`   | adds legend to the plot                                                                                                             |

## See also

[`model.LDA`](#model.lda) , [`plot3`](#plot3) , [`plot4`](#plot4)

## Author

Elizabeth Stroud

## References

model 1 - see Bogaard et al. 2016 "Combining functional weed ecology and
crop stable isotope ratios to identify cultivation intensity: a
comparison of cereal production regimes in Haute Provence, France and
Asturias, Spain" *Vegetation History and Archaeobotany* 25, 57-73

model 2 - see Bogaard et al. 2018 "From traditional Farming in Morocco
to Early Urban Agroecology in Northern Mesopotamia: Combining
Present-day Arable Weed Surveys and crop isotope analysis to reconstruct
past Agrosystems in (Semi-)arid Regions" *Environmental Archaeology* 23,
303-322

model 3 - see Bogaard et al 2022 "????"

## Examples

``` r
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

## options with different col and pch for different archaeological sites or different phases
plot5("model2", data$LD1, col3=data$Study,pch3=data$Study, legend=T, site=c("Site 1","Site 2","Site 3"))
```

