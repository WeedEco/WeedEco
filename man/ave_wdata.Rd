\name{ave_wdata}
\alias{ave_wdata}
\title{Function to average the functional trait data of a group of species
}
\description{This function averages the functional trait data (SLA, ARNODE, LOGCANH, LOGCAND, VEGPROP) of entered species. This should be used to average data of taxa whose identification is not to species level.
}
\usage{
ave_wdata(newname, species1, species2, species3 = NULL, species4 = NULL)
}

\arguments{
  \item{newname}{The code of the composite species to be created
}
  \item{species1}{The four-three code of the first species to be averaged

}
  \item{species2}{The four-three code of the second species to be averaged

}
  \item{species3}{The four-three code of the third species to be averaged

}
  \item{species4}{The four-three code of the fourth species to be averaged
}
}
\details{It is highly recommended that species are only averaged if their functional attributes are similar.
}
\value{ a data frame containing the averaged SLA, ARNODE, LOGCANH, LOGCAND and VEGPROP of the entered species.
}
\references{ ADD paper
%% ~put references to the literature/web site here ~
}
\author{Elizabeth Stroud}

\examples{

av_sp<-ave_wdata("bromahs", "bromarv","bromhor", "bromsec")
av_sp
}

