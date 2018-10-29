\name{knot}
\alias{knot}
\alias{cp}
\alias{curl}
\alias{cycle}
\alias{dir}
\alias{tension}
\alias{\%+\%}
\alias{\%-\%}
\alias{"paths"}
\title{
  Create a MetaPost Path
}
\description{
  These functions can be used to describe a MetaPost path,
  consisting of two or more knots, with various constraints
  on how the path behaves between the knots.
}
\usage{
knot(x, y, units = getOption("metapost.units"),
     dir = NA, dir.left = dir, dir.right = dir,
     cp.left.x = NA, cp.right.x = NA, cp.left.y = NA, cp.right.y = NA,
     curl.left = NA, curl.right = NA,
     tension.left = NA, tension.right = NA)
cp(x, y, units = getOption("metapost.units"))
curl(x)
cycle()
dir(x, y = NULL)
tension(x)
}
\arguments{
  \item{x}{
    Numeric value:  a location (for \code{knot} and \code{cp}), or
    an angle (for \code{dir}, if \code{y} is \code{NULL}), or a
    vector component (for \code{dir}), or
    a curl or tension value.
    Or a \pkg{grid} unit (for \code{knot} and \code{cp}).
  }
  \item{y}{
    Numeric value:  a location (for \code{knot} and \code{cp}), or a
    vector component (for \code{dir}).
    Or a \pkg{grid} unit (for \code{knot} and \code{cp}).
  }
  \item{units}{
    The \pkg{grid} coordinate system to use for locations
    (if locations are only given as numeric values).
  }
  \item{dir, dir.left, dir.right}{
    A numeric angle. 
  }
  \item{cp.left.x, cp.right.x, cp.left.y, cp.right.y}{
    A numeric location.
  }
  \item{curl.left, curl.right}{
    A numeric curl value (must be at least 0).
  }
  \item{tension.left, tension.right}{
    A numeric tension value (must be at least 3/4).
    A negative values indicates a lower bound.
  }
}
\details{
  A MetaPost path is constructed using calls to \code{knot} and
  combining the results using the \code{+} operator (see the examples
  below).

  Constraints for a knot can be specified within the call to \code{knot}
  or by combining connectors (\code{cp}, \code{dir}, etc)
  with a knot using \code{+}.

  Knots can also be combined using \code{-} (a straight line rather than
  a curve), \code{\%+\%} (no inflection), and \code{\%-\%} (straight
    line with smooth connection) operators.
}
\value{
  The individual functions generate knots and connectors, but when
  combined together, they produce a MetaPost path (\code{"mppath"}) object.
}
\references{
  Hobby, J. D. and the MetaPost development team (2018). METAPOST a
  user's manual. \url{https://www.tug.org/docs/metapost/mpman.pdf}
}
\author{
  Paul Murrell
}
\seealso{
  \code{\link{metapost}}, 
  \code{\link{mpost}}, 
  \code{\link{mptrace}}, 
  \code{\link{grid.metapost}}.
}
\examples{
knot(0, 0, dir.right=0)
knot(0, 0, dir.right=0) + knot(1, 1)
knot(0, 0) + dir(0) + knot(1, 1)
}
\keyword{ dplot }% use one of  RShowDoc("KEYWORDS")
