\name{grid.metapost}
\alias{grid.metapost}
\alias{metapostGrob}
\alias{metapostGrob.mppath}
\alias{metapostGrob.mpcontrols}
\alias{metapostGrob.mpcontrolList}
\title{
  Draw a MetaPost curve.
}
\description{
  Draw a MetaPost curve in \pkg{grid} graphics.
}
\usage{
\method{metapostGrob}{mppath}(x, gp = gpar(), name = NULL, digits=2, ...)
\method{metapostGrob}{mpcontrols}(x, gp = gpar(), name = NULL, ...)
\method{metapostGrob}{mpcontrolList}(x, gp = gpar(), name = NULL, ...)
grid.metapost(...)
}
\arguments{
  \item{x}{
    A MetaPost path, either unsolved (a description generated using
    \code{\link{knot}} etc), or solved (as produced by
    \code{\link{mptrace}}).
  }
  \item{gp}{
    Graphical parameters (from a call to \code{gpar}).
  }
  \item{name}{
    A name for the grob that is created.
  }
  \item{digits}{
    The number of decimal places to use when writing floating
    point values in MetaPost code.
  }
  \item{\dots}{
    Arguments passed to \code{metapostGrob}.
  }
}
\value{
  \code{metapostGrob} creates a \code{"metapostgrob"} object.
}
\author{
  Paul Murrell
}
\seealso{
  \code{\link{knot}},
  \code{\link{mptrace}}.
}
\examples{
p <- knot(0, 0) + dir(0) + dir(0) + knot(1, 1) 
grid.metapost(p)
}
\keyword{ dplot }% use one of  RShowDoc("KEYWORDS")
