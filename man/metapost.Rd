\name{metapost}
\alias{metapost}
\title{
  Generate a MetaPost File
}
\description{
  Generate a MetaPost file from a MetaPost path.
}
\usage{
metapost(x, file = "fig.mp", digits=2)
}
\arguments{
  \item{x}{
    A MetaPost path, as produced from \code{\link{knot}} etc.
  }
  \item{file}{
    The name of the file to produce.  If \code{NULL}, no file
    is created.
  }
  \item{digits}{
    The number of decimal places to use when writing floating
    point values in MetaPost code.
  }
}
\value{
  The MetaPost code is returned invisibly.
}
\author{
  Paul Murrell
}
\seealso{
\code{\link{knot}}
}
\examples{
p <- knot(0, 0) + dir(0) + dir(0) + knot(1, 1)
mpcode <- metapost(p, NULL)
mpcode
}
\keyword{ dplot }% use one of  RShowDoc("KEYWORDS")
