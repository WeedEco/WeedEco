\name{plot2}
\alias{plot2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Plotting samples and centroids
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot2(model, x, xlims, method, pch1 = NULL, pch3 = NULL, col1 = NULL, col3 = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
%%     ~~Describe \code{model} here~~
}
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{xlims}{
%%     ~~Describe \code{xlims} here~~
}
  \item{method}{
%%     ~~Describe \code{method} here~~
}
  \item{pch1}{
%%     ~~Describe \code{pch1} here~~
}
  \item{pch3}{
%%     ~~Describe \code{pch3} here~~
}
  \item{col1}{
%%     ~~Describe \code{col1} here~~
}
  \item{col3}{
%%     ~~Describe \code{col3} here~~
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
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (model, x, xlims, method, pch1 = NULL, pch3 = NULL,
    col1 = NULL, col3 = NULL)
{
    library(beeswarm)
    library(dplyr)
    library(haven)
    library(MASS)
    if (model == "temperate")
        load(file = "data_model.rda")
    if (model == "arid")
        load(file = "data_model_arid.rda")
    discrim_cv <- lda(Study ~ SLA + ARNODE + LOGCAHN + LOGCADN +
        FLOWPER, data.model, CV = TRUE)
    model_lda <- lda(Study ~ SLA + ARNODE + LOGCAHN + LOGCADN +
        FLOWPER, data.model)
    predictionmodel <- predict(model_lda, data.model)
    functionalAt <- data.frame(Study = as.factor(data.model$Study),
        Classification = predictionmodel$class, LD1 = predictionmodel$x)
    centroids <- functionalAt \%>\% group_by(Study) \%>\% summarise(centroid1 = mean(LD1))
    beeswarm::beeswarm(centroids$centroid1 * -1, vertical = FALSE,
        side = -1, method = method, xlim = xlims, col = col1,
        pch = pch1, axes = F)
    par(new = T)
    beeswarm::beeswarm(x * -1, vertical = FALSE, ylim = c(1,
        1.05), side = 1, method = method, xlim = xlims, col = col3,
        pch = pch3, axes = F)
    axis(side = 1)
  }
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
