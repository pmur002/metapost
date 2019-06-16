\name{metapost-package}
\alias{metapost-package}
\docType{package}
\title{
  R Interface to MetaPost
}
\description{
  Provides an R interface to METAPOST.
  There are functions to generate an R description of a MetaPost
  curve, functions to generate MetaPost code from an R description,
  functions to process MetaPost code, and functions to read
  solved MetaPost paths back into R.
}
\details{
  Generate a MetaPost path with functions like \code{\link{knot}}.

  Write the MetaPost path to a file with \code{\link{metapost}}.

  Run \command{mpost} on the file with \code{\link{mpost}}.

  Read the solved path (Bezier control points) into R 
  with \code{\link{mptrace}}.

  Draw a solved path with \code{\link{grid.metapost}}.

  It is also possible to pass \code{\link{grid.metapost}} the original
  path (and it will perform the write/solve/read steps itself).
}
\author{
Paul Murrell <paul@stat.auckland.ac.nz>
}
\references{
  Hobby, J. D. and the MetaPost development team (2018). METAPOST a
  user's manual. \url{https://www.tug.org/docs/metapost/mpman.pdf}
}
\keyword{ package }
\seealso{
  \code{\link{knot}}
  \code{\link{metapost}}
  \code{\link{mpost}}
  \code{\link{mptrace}}
  \code{\link{grid.metapost}}
}
\examples{
\donttest{
oldunits <- options(metapost.units="in")
oldwd <- setwd(tempdir())
p <- knot(0, 0) + dir(0) + dir(0) + knot(1, 1)
grid.metapost(p)
setwd(oldwd)
options(oldunits)
}
}
