
<!-- README.md is generated from README.Rmd. Please edit that file -->

# WeedEco

<!-- badges: start -->
<!-- badges: end -->

This repository stores the R package WeedEco. This package contains
functions which conduct linear discriminant analysis using one of three
modern models (i.e., sets of discriminated modern arable fields) to
classify archaeobotanical data or other cases the investigator wishes to
classify, such as survey data from other farming regimes, on the basis
of relevant functional ecological traits (or attributes) of weed
species. Other functions include those related to data organisation, as
well as functions for plotting the results of the linear discriminant
analysis in comparison with the selected modern model.

## Referencing

The package draws on data from the functional trait database, as well as
models constructed by [Bogaard et
al. (2016)](https://doi.org/10.1007/s00334-015-0524-0), [Bogaard et
al. (2018)](https://doi.org/10.1080/14614103.2016.1261217) and [Bogaard
et al. (2021)](https://library.oapen.org/handle/20.500.12657/58568).

When publishing results obtained via the use of this package please cite
the package and its version, as well as the functional trait database
and the model used. A best practice example paragraph is provided in
Stroud et al. (2024).

**Package citation**: Stroud, E., (2023) WeedEco: Classification of
unknown cases using linear discriminant analysis to understand farming
regimes. R package version 1.0.0, <https://github.com/WeedEco/WeedEco>

**Functional trait database**: Hodgson, J., Jones, G., Charles, M., 
Stroud, E., Ater, M., Band, P., Cerabolini, B., Diffey, C., Ertug, F.,
Ferguson, H., Filipović, D., Forster, E., Green, L., Halstead, P., Herbig, C., 
Hmimsa, Y., Hodgson, J., Hoppe, C., Hynd, A., … Bogaard, A. (2023).
A functional trait database of arable weeds from Eurasia and North Africa.
University of Oxford. http://dx.doi.org/10.5287/ora-pp4y9nkoz

**Model 1**: Bogaard, A., Hodgson, J., Nitsch, E. *et al.* (2016)
Combining functional weed ecology and crop stable isotope ratios to
identify cultivation intensity: a comparison of cereal production
regimes in Haute Provence, France and Asturias, Spain. *Veget Hist
Archaeobot* **25**, 57–73. [DOI:
10.1007/s00334-015-0524-0](https://doi.org/10.1007/s00334-015-0524-0)

**Model 2**: Bogaard, A., Styring, A., Ater, M., *et al.* (2018) From
Traditional Farming in Morocco to Early Urban Agroecology in Northern
Mesopotamia: Combining Present-day Arable Weed Surveys and Crop Isotope
Analysis to Reconstruct Past Agrosystems in (Semi-)arid
Regions, Environmental
Archaeology, 23:4, 303-322, DOI: [10.1080/14614103.2016.1261217](#0)

**Model 3**: Bogaard, A., Hodgson, J., Kropp, C., *et al.* (2022)
Lessons from Laxton, Highrove and Lorsch: Building arable weed-based
models for the investigation of Early Medieval Agriculture in England in
McKerracher, M., and H. Hamerow (eds) *New Perspectives on the Medieval
‘Agricultural Revolution’: Crop, Stock and Furrow*. Liverpool University
Press. [Online
resource](https://library.oapen.org/handle/20.500.12657/58568)

**Example paper**: Stroud, E., Charles, M., Jones, G. et al. Seeing the
fields through the weeds: introducing the WeedEco R package for comparing
past and present arable farming systems using functional weed ecology. 
Veget Hist Archaeobot 33, 475–487 (2024). 
https://doi.org/10.1007/s00334-023-00964-8 

## Installation

You can install the package WeedEco from [GitHub](https://github.com/)
with:

``` r
# install.packages("devtools")
devtools::install_github("WeedEco/WeedEco")
```

## Example

This is a basic example which shows the use of the package with a
simulated data set. Please see Stroud et al. (2023) for a fully worked
example using archaeobotanical data. Please note that each species used
requires its correct four_three code - the unique identifier which links
it with the corresponding species within the functional trait database.
More details can be found here \<insert link\>.

``` r
library(WeedEco)
## Random data created to represent survey data or archaeobotanical counts
species<-c("Chenopodium album" , "Anthemis cotula", "Brassica rapa",
  "Raphanus raphanistrum", "Agrostemma githago" , "Poa annua" )
Four_three_code<-c("chenalb", "anthcot", "brasrap","raphrap","agrogit", "poa_ann")
s.1246<-sample(1:3, 6, replace=T)
s.46178<-sample(1:5, 6, replace=T)
s.1<-sample(0:10, 6, replace=T)
s.23<-sample(0:3, 6, replace=T)
s.987<-sample(3:9, 6, replace=T)
s.11<-sample(c(2:8,0), 6, replace=T)
s.244<-sample(c(0,2:3), 6, replace=T)
s.872<-sample(c(0,1,16), 6, replace=T)
dataset<-data.frame(species,Four_three_code,s.1246,s.46178,s.1,s.23,s.987,s.11, s.244, s.872) # the random dataset
## Creation of random flowering period dataset
# Note that flowering period (max duration) is not provided and must be collected from relevant literature
FLOWPER<-sample(3:9, 6, replace=T)
x<-data.frame(Four_three_code,FLOWPER) 
## Data organisation - function used to organise archaeobotncial or survey data for LDA analysis
results<-wdata_org(dataset, samples=3, codes=2, codename="Four_three_code", model=1, fl_pr=x)
#>              SLA    ARNODE LOGCANH LOGCAND  FLOWPER
#> s.1246  23.78448 12008.417     5.5     5.5 6.333333
#> s.46178 23.78448 12008.417     5.5     5.5 6.333333
#> s.1     23.78448 12008.417     5.5     5.5 6.333333
#> s.23    23.12871 14241.862     5.6     5.4 6.400000
#> s.987   23.78448 12008.417     5.5     5.5 6.333333
#> s.11    23.78448 12008.417     5.5     5.5 6.333333
#> s.244   24.83237  5685.177     5.4     5.2 7.000000
#> s.872   22.06833  9088.447     6.0     5.5 7.000000
```

``` r
## LDA anaylsis classification using model 1
LDA<-wmodel.LDA(results, model = 1)
#> 
#> Results and linear discriminant scores:
#>         CLASS_std* Prob.1_std* Prob.2_std*   LD1*
#> s.1246           2           0           1 -3.139
#> s.46178          2           0           1 -3.139
#> s.1              2           0           1 -3.139
#> s.23             2           0           1 -3.157
#> s.987            2           0           1 -3.139
#> s.11             2           0           1 -3.139
#> s.244            2           0           1 -3.214
#> s.872            2           0           1 -2.518
#> 
#> Centroids:
#>   Group Centroid1
#> 1     1     2.441
#> 2     2    -2.833
```

``` r
## Visulastion using wplot_basic
wplot_basic(model = 1, LDA)
```

<img src="man/figures/README-Plot-1.png" width="100%" />

## 
