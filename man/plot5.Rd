\name{plot5}
\alias{plot5}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plotting the model with seperate symbols for study sites
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot5(model, df, x, xlims, ticks, col1, col2, col3, pch1, pch2, pch3, compact, priority)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
%%     ~~Describe \code{model} here~~
}
  \item{df}{
%%     ~~Describe \code{df} here~~
}
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{xlims}{
%%     ~~Describe \code{xlims} here~~
}
  \item{ticks}{
%%     ~~Describe \code{ticks} here~~
}
  \item{col1}{
%%     ~~Describe \code{col1} here~~
}
  \item{col2}{
%%     ~~Describe \code{col2} here~~
}
  \item{col3}{
%%     ~~Describe \code{col3} here~~
}
  \item{pch1}{
%%     ~~Describe \code{pch1} here~~
}
  \item{pch2}{
%%     ~~Describe \code{pch2} here~~
}
  \item{pch3}{
%%     ~~Describe \code{pch3} here~~
}
  \item{compact}{
%%     ~~Describe \code{compact} here~~
}
  \item{priority}{
%%     ~~Describe \code{priority} here~~
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
function (model, df, x, xlims, ticks, col1, col2, col3, pch1,
    pch2, pch3, compact, priority)
{
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
    plot(2:5, type = "n", xlim = xlims, ylim = c(0, 0.2), axes = F,
        xlab = "", ylab = "")
    points(swarmy(centroids$centroid1 * -1, rep(0.15, 2)), col = col1,
        pch = pch1, cex = 1.2)
    points(swarmy())
    points(swarmy(x * -1, rep(0.05, nrow(df)), side = 1, compact = compact,
        priority = priority), col = col3, pch = pch3)
    axis(1, ticks)
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
